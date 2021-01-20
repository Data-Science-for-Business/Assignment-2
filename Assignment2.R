#Assignment 2 DSB P3
#Author: Nathan Hart
#Date: 1/19/21


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages

Credit_Data<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE) # Load the datafile to R
New_Apps_Data<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE) # Load the datafile to R

str(Credit_Data) # See if some data types were misclassified when importing data from CSV
str(New_Apps_Data)

#-----------------------------------
#fix data types here
#-----------------------------------
Credit_Data$ID <- as.factor(Credit_Data$ID) #maybe this should stay as numeric - represents chronological order? Assumption. Leaving as is for now
Credit_Data$SEX <- as.factor(Credit_Data$SEX)
Credit_Data$EDUCATION <- as.factor(Credit_Data$EDUCATION)
Credit_Data$MARRIAGE <- as.factor(Credit_Data$MARRIAGE)
Credit_Data$PAY_1 <- as.factor(Credit_Data$PAY_1)
Credit_Data$PAY_2 <- as.factor(Credit_Data$PAY_2)
Credit_Data$PAY_3 <- as.factor(Credit_Data$PAY_3)
Credit_Data$PAY_4 <- as.factor(Credit_Data$PAY_4)
Credit_Data$PAY_5 <- as.factor(Credit_Data$PAY_5)
Credit_Data$PAY_6 <- as.factor(Credit_Data$PAY_6)
Credit_Data$default_0 <- as.factor(Credit_Data$default_0)

New_Apps_Data$SEX <- as.factor(New_Apps_Data$SEX)
New_Apps_Data$EDUCATION <- as.factor(New_Apps_Data$EDUCATION)
New_Apps_Data$MARRIAGE <- as.factor(New_Apps_Data$MARRIAGE)
New_Apps_Data$PAY_1 <- as.factor(New_Apps_Data$PAY_1)
New_Apps_Data$PAY_2 <- as.factor(New_Apps_Data$PAY_2)
New_Apps_Data$PAY_3 <- as.factor(New_Apps_Data$PAY_3)
New_Apps_Data$PAY_4 <- as.factor(New_Apps_Data$PAY_4)
New_Apps_Data$PAY_5 <- as.factor(New_Apps_Data$PAY_5)
New_Apps_Data$PAY_6 <- as.factor(New_Apps_Data$PAY_6)
New_Apps_Data$default_0 <- as.factor(New_Apps_Data$default_0)


str(Credit_Data) # Check the classifications make sense now
str(New_Apps_Data)


# Create a custom function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}

Credit_Data<-fixNAs(Credit_Data) #Apply fixNAs function to the data to fix missing values
New_Apps_Data<-fixNAs(New_Apps_Data) #Apply fixNAs function to the data to fix missing values

str(Credit_Data) # test


#----------------------------------------------------------------------------------
#-----------------------As far as I got tonight -----------------------------------
#----------------------------------------------------------------------------------

table(Credit_Data$Group.State)# check for rare categories


# Create another a custom function to combine rare categories into "Other."+the name of the original variable (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }


#Apply combinerarecategories function to the data and then split it into testing and training data.

Credit_Data<-combinerarecategories(Credit_Data,20) #combine categories with <20 values in STCdata into "Other"

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = Credit_Data$Retained.in.2012.,
                               p = 1888/2389, list = FALSE)
training <- Credit_Data[ inTrain,]
testing <- Credit_Data[ -inTrain,]

# Select the variables to be included in the "base-case" model
# First include all variables use glm(Retained.in.2012.~ ., data=training, family="binomial"(link="logit")) Then see which ones have "NA" in coefficients and remove those

model_logistic<-glm(Retained.in.2012.~ Special.Pay + 
                      To.Grade + Group.State + Is.Non.Annual. +
                      Tuition + FRP.Active + FRP.Cancelled + FRP.Take.up.percent. + 
                      Cancelled.Pax + Total.Discount.Pax + Initial.System.Date + 
                      Poverty.Code + CRM.Segment + School.Type + Parent.Meeting.Flag + 
                      MDR.Low.Grade + MDR.High.Grade + Total.School.Enrollment + 
                      EZ.Pay.Take.Up.Rate + School.Sponsor +  
                      SPR.New.Existing + FPP + FirstMeeting + LastMeeting + 
                      DifferenceTraveltoFirstMeeting + DepartureMonth  + MajorProgramCode + SingleGradeTripFlag + 
                      FPP.to.School.enrollment + FPP.to.PAX + SchoolSizeIndicator, data=training, family="binomial"(link="logit"))

summary(model_logistic) 

# to add surrogates paste this to the list of variables; note, it will run quite a bit slower
#Special.Pay_surrogate + Early.RPL_surrogate + Latest.RPL_surrogate + 
#Initial.System.Date_surrogate + CRM.Segment_surrogate + MDR.High.Grade_surrogate + 
#Total.School.Enrollment_surrogate + FirstMeeting_surrogate + 
#LastMeeting_surrogate + DifferenceTraveltoFirstMeeting_surrogate + 
#DifferenceTraveltoLastMeeting_surrogate + FPP.to.School.enrollment_surrogate

##The model clearly has too many variables, most of which are insignificant 

## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both"
## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") #Predict probabilities
logistic_classification<-rep("1",500)
logistic_classification[logistic_probabilities<0.6073]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$Retained.in.2012.,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$Retained.in.2012.)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

