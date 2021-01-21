#------------------------------------------------------------------------------#
#--------------------------IMPORT LIBRARIES------------------------------------#
#------------------------------------------------------------------------------#

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", 
               "tidyverse", "dplyr", "GGally", "ggplot2", "hrbrthemes") #Check, and if needed install the necessary packages

cd_df<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE, sep = ";") # Load "CSV_DSB_S8_9_Credit"

str(cd_df) # See if some data types were misclassified when importing data from CSV

#------------------------------------------------------------------------------#
#---------------------------CLEAN THE DATA-------------------------------------#
#------------------------------------------------------------------------------#

cd_df$ID <- as.factor(cd_df$ID)
cd_df$SEX <- as.factor(cd_df$SEX)
cd_df$EDUCATION <- as.factor(cd_df$EDUCATION)
cd_df$MARRIAGE <- as.factor(cd_df$MARRIAGE)

cd_df$PAY_1 <- as.factor(cd_df$PAY_1)
cd_df$PAY_2 <- as.factor(cd_df$PAY_2)
cd_df$PAY_3 <- as.factor(cd_df$PAY_3)
cd_df$PAY_4 <- as.factor(cd_df$PAY_4)
cd_df$PAY_5 <- as.factor(cd_df$PAY_5)
cd_df$PAY_6 <- as.factor(cd_df$PAY_6)

#cd_df$BILL_AMT1 <- as.numeric(levels(cd_df$BILL_AMT1))
#cd_df$BILL_AMT2 <- as.integer(as.character(cd_df$BILL_AMT2))
#cd_df$BILL_AMT3 <- as.numeric(as.character(cd_df$BILL_AMT3))
#cd_df$BILL_AMT4 <- as.integer(as.character(cd_df$BILL_AMT4))
#cd_df$BILL_AMT5 <- as.integer(as.character(cd_df$BILL_AMT5))
#cd_df$BILL_AMT6 <- as.integer(as.character(cd_df$BILL_AMT6))
#cd_df$PAY_AMT1 <- as.numeric(as.character(cd_df$PAY_AMT1))
#cd_df$PAY_AMT2 <- as.numeric(as.character(cd_df$PAY_AMT2))
#cd_df$PAY_AMT3 <- as.numeric(as.character(cd_df$PAY_AMT3))
#cd_df$PAY_AMT4 <- as.numeric(as.character(cd_df$PAY_AMT4))
#cd_df$PAY_AMT5 <- as.numeric(as.character(cd_df$PAY_AMT5))
#cd_df$PAY_AMT6 <- as.numeric(as.character(cd_df$PAY_AMT6))

cd_df$default_0 <- as.factor(cd_df$default_0)

#for some reason the above line create a new column in some of our computers (but not to all)
#Delete the wrongly created line
#colnames(cd_df)
#cd_df = subset(cd_df, select = -c(default_0...))

cd_df = subset(cd_df, select = -c(X))#deleting spurious column from csv modifications

str(cd_df)

#------------------------------------------------------------------------------#
#---------------------------CHECK FOR MISSING VALUES---------------------------#
#------------------------------------------------------------------------------#

colSums(is.na(cd_df))

#------------------------------------------------------------------------------#
#---------------------------CORRECT DATA---------------------------------------#
#------------------------------------------------------------------------------#

cd_df$MARRIAGE[cd_df$MARRIAGE == 0] <- 3
cd_df$EDUCATION[cd_df$EDUCATION == 0] <- 5
cd_df$EDUCATION[cd_df$EDUCATION == 6] <- 5

#create a dummy variable for uncommon account "negative billing" at AMT 1
cd_df <- cd_df %>% add_column(NEG_BILL=0,.before = "BILL_AMT1")
cd_df$NEG_BILL[cd_df$BILL_AMT1 <0] <- 1

#------------------------------------------------------------------------------#
#---------------------------CHECK FOR RARE VALUES------------------------------#
#------------------------------------------------------------------------------#
combinerarecategories <- function(data_frame,mincount){ 
  for (i in 2 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame)
}

cd_df <- combinerarecategories(cd_df,20) #combine categories with <20 values in STCdata into "Other"

str(cd_df)

#------------------------------------------------------------------------------#
#-----------------------BASIC EXPLORATORY ANALYSIS-----------------------------#
#------------------------------------------------------------------------------#

#Correlation matrix
GGally::ggcorr(cd_df[,-9], hjust = 1, layout.exp = 2, label = T, label_size = 2.9)

# #------------------------------------------------------------------------------#
# #---------------------------FEATURE ENGINEERING--------------------------------#
# #------------------------------------------------------------------------------#

#Feature_1: difference with previous period bill amount
cd_df$Delta_Bill_AMT1_vs_Bill_AMT2 <- as.integer(cd_df$BILL_AMT1 - cd_df$BILL_AMT2) #difference bill_AMT1 vs. bill_AMT2
cd_df$Delta_Bill_AMT2_vs_Bill_AMT3 <- as.integer(cd_df$BILL_AMT2 - cd_df$BILL_AMT3) #difference bill_AMT2 vs. bill_AMT3
cd_df$Delta_Bill_AMT3_vs_Bill_AMT4 <- as.integer(cd_df$BILL_AMT3 - cd_df$BILL_AMT4) #difference bill_AMT3 vs. bill_AMT4
cd_df$Delta_Bill_AMT4_vs_Bill_AMT5 <- as.integer(cd_df$BILL_AMT4 - cd_df$BILL_AMT5) #difference bill_AMT4 vs. bill_AMT5
cd_df$Delta_Bill_AMT5_vs_Bill_AMT6 <- as.integer(cd_df$BILL_AMT5 - cd_df$BILL_AMT6) #difference bill_AMT5 vs. bill_AMT6

#Feature_2: % delta with previous period bill amount
cd_df$PER_Delta_Bill_AMT1_vs_Bill_AMT2 <- as.double((cd_df$BILL_AMT1 - cd_df$BILL_AMT2)/cd_df$BILL_AMT2) #Percentage difference bill_AMT1 vs. bill_AMT2
cd_df$PER_Delta_Bill_AMT2_vs_Bill_AMT3 <- as.double((cd_df$BILL_AMT2 - cd_df$BILL_AMT3)/cd_df$BILL_AMT3) #Percentage difference bill_AMT2 vs. bill_AMT3
cd_df$PER_Delta_Bill_AMT3_vs_Bill_AMT4 <- as.double((cd_df$BILL_AMT3 - cd_df$BILL_AMT4)/cd_df$BILL_AMT4) #Percentage difference bill_AMT3 vs. bill_AMT4
cd_df$PER_Delta_Bill_AMT4_vs_Bill_AMT5 <- as.double((cd_df$BILL_AMT4 - cd_df$BILL_AMT5)/cd_df$BILL_AMT5) #Percentage difference bill_AMT4 vs. bill_AMT5
cd_df$PER_Delta_Bill_AMT5_vs_Bill_AMT6 <- as.double((cd_df$BILL_AMT5 - cd_df$BILL_AMT6)/cd_df$BILL_AMT5) #Percentage difference bill_AMT5 vs. bill_AMT6

cd_df[is.na(cd_df)] <- 0 #For feature #2, because we sometimes divided by 0, we produced NAN. Here I overwrote these NANs as 0
cd_df <- cd_df %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x)) #same, taking care of the NaNs

#Feature_3: balance remaining -> Limit_amount - Bill_amount
cd_df$Balance_remaining_1 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT1)
cd_df$Balance_remaining_2 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT2)
cd_df$Balance_remaining_3 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT3)
cd_df$Balance_remaining_4 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT4)
cd_df$Balance_remaining_5 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT5)
cd_df$Balance_remaining_6 <- as.integer(cd_df$LIMIT_BAL - cd_df$BILL_AMT6)

#Feature_4: % balance remaining -> (Limit_amount - Bill_amount)/Limit_amount
cd_df$PER_Balance_remaining_1 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT1)/cd_df$LIMIT_BA)
cd_df$PER_Balance_remaining_2 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT2)/cd_df$LIMIT_BA)
cd_df$PER_Balance_remaining_3 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT3)/cd_df$LIMIT_BA)
cd_df$PER_Balance_remaining_4 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT4)/cd_df$LIMIT_BA)
cd_df$PER_Balance_remaining_5 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT5)/cd_df$LIMIT_BA)
cd_df$PER_Balance_remaining_6 <- as.numeric((cd_df$LIMIT_BAL - cd_df$BILL_AMT6)/cd_df$LIMIT_BA)

#Feature_5: Flag --> bill_amount > limit amount
cd_df$Limit_Alert_1 <- as.factor(ifelse(cd_df$PER_Balance_remaining_1 <0 , 1, 0))
cd_df$Limit_Alert_2 <- as.factor(ifelse(cd_df$PER_Balance_remaining_2 <0 , 1, 0))
cd_df$Limit_Alert_3 <- as.factor(ifelse(cd_df$PER_Balance_remaining_3 <0 , 1, 0))
cd_df$Limit_Alert_4 <- as.factor(ifelse(cd_df$PER_Balance_remaining_4 <0 , 1, 0))
cd_df$Limit_Alert_5 <- as.factor(ifelse(cd_df$PER_Balance_remaining_5 <0 , 1, 0))
cd_df$Limit_Alert_6 <- as.factor(ifelse(cd_df$PER_Balance_remaining_6 <0 , 1, 0))

view(cd_df)
str(cd_df)


####Feature engineering backlog##########
#---------------------------------------#
#Feature_6: Avg. Pay_X -> Pay_Y category

#Feature_7: Max. Pay Category

#Feature_8: Range. Pay Category

#Feature_9: Abs range Bill Amount

#Feature_10: Abs range bill as % of limit balance

#Feature_11: Max delta Pay_Amount_t vs. Bill_Amount_t-1 --> Find the maximum difference between a bill amount (t-1) and the amount actually paid (t)


View(cd_df)
View(colSums(is.na(cd_df)))


#------------------------------------------------------------------------------#
#---------------------------CREATE TRAIN AND TEST SET--------------------------#
#------------------------------------------------------------------------------#

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
cd_df_in_non_holdout <- createDataPartition(y = cd_df$default_0,
                                     p = 0.8, list = FALSE)#create partition for holdout and non

cd_df_non_holdout <- cd_df[ cd_df_in_non_holdout,]
cd_df_inTrain <- createDataPartition(y = cd_df_non_holdout$default_0,
                                            p = 0.8, list = FALSE)#create partition for training from non-holdout partition
cd_df_training <- cd_df_non_holdout[ cd_df_inTrain, ]
cd_df_testing <- cd_df_non_holdout[ -cd_df_inTrain, ] #testing negative of training within non-holdout section
cd_df_holdout <- cd_df[ -cd_df_in_non_holdout, ] #hold is negative of non-holdout

cd_df_inDev <- createDataPartition(y = cd_df_non_holdout$default_0,
                                 p = 0.05, list = FALSE)
cd_df_dev <- cd_df_non_holdout[cd_df_inDev, ]

colnames(cd_df_training)
colnames(cd_df_testing)
nrow(cd_df_training)
nrow(cd_df_testing)

#------------------------------------------------------------------------------#
#--------------------MODEL: LOGISTIC REGRESSION--------------------------------#
#------------------------------------------------------------------------------#

#Step 1: Set-up the logistic model
cd_df_model_logistic<-glm(default_0 ~ 
                            LIMIT_BAL + 
                            SEX + 
                            EDUCATION + 
                            MARRIAGE +
                            AGE + 
                            PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                            BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                            PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +
                            #Delta_Bill_AMT1_vs_Bill_AMT2 + Delta_Bill_AMT2_vs_Bill_AMT3 + Delta_Bill_AMT3_vs_Bill_AMT4 +Delta_Bill_AMT4_vs_Bill_AMT5 + Delta_Bill_AMT5_vs_Bill_AMT6 + 
                            PER_Delta_Bill_AMT1_vs_Bill_AMT2 + PER_Delta_Bill_AMT2_vs_Bill_AMT3 + PER_Delta_Bill_AMT3_vs_Bill_AMT4 + PER_Delta_Bill_AMT4_vs_Bill_AMT5 + PER_Delta_Bill_AMT5_vs_Bill_AMT6 +
                            #Balance_remaining_1 + Balance_remaining_2 + Balance_remaining_3 + Balance_remaining_4 +Balance_remaining_5 + Balance_remaining_6 +
                            Limit_Alert_1 + Limit_Alert_2 + Limit_Alert_3 + Limit_Alert_4 + Limit_Alert_5 +Limit_Alert_6, 
                            data=cd_df_training, family="binomial"(link="logit"))

summary(cd_df_model_logistic) 

#------------------------------------------------------------------------------
#Step 2: STEPWISE AIC -> drop the insignificant variables to improve the model
cd_df_model_logistic_stepwiseAIC<-stepAIC(cd_df_model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(cd_df_model_logistic_stepwiseAIC) 
#------------------------------------------------------------------------------

par(mfrow=c(1,4))
plot(cd_df_model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

#Step 3: Determine probabilities and classification
cd_df_model_logistic_stepwiseAIC_probabilities<-predict(cd_df_model_logistic_stepwiseAIC,
                                                        newdata=cd_df_testing,
                                                        type="response") #Predict probabilities

cd_df_model_logistic_stepwiseAIC_probabilities

#------------------------------------------------------------------------------#
#--------------------FIND OPTIMAL T-THRESHOLD----------------------------------#
#------------------------------------------------------------------------------#

counter <- 0
t_threshold <- 0
t_threshold <- as.double(t_threshold)
best_t_threshold <- 0
max_expected_return <- 0
max_confusion_matrix <- 0

my_t_threshold_list <- list()
my_expected_value_list <- list()

while (t_threshold < 0.95) {
  t_threshold = as.double(t_threshold+0.01)
  counter = counter+1
  
  my_t_threshold_list[counter] <- t_threshold
  
  print(paste0("Current T-threshold applied to classification: ", t_threshold))
  
  cd_df_logistic_classification <-rep("1",nrow(cd_df_testing)) #Generate number of rows depending on size of training set
  cd_df_logistic_classification[cd_df_model_logistic_stepwiseAIC_probabilities<t_threshold]="0"
  
  print("T-threshold applied...")
  
  cd_df_logistic_classification<-as.factor(cd_df_logistic_classification)
  
  cd_df_logistic_classification
  
  #Need to check if the predictor and actual outcomes have the same level
  #str(cd_df_logistic_classification)
  #levels(cd_df_testing$default_0) <- list("0" = "1", "1" = "2")
  #str(cd_df_testing$default_0)
  
  #----------------------------------------------------------------------------#
  #--------------------RESULTS: LOGISTIC REGRESSION----------------------------#
  #----------------------------------------------------------------------------#
  
  #confusion matrix
  cm_1 <- confusionMatrix(cd_df_logistic_classification, 
                          cd_df_testing$default_0, 
                          positive = "0",) ###############---> According to data dictionary, positive outcome = 0
  
  cm_1
  
  True_non_defaulter_TP <- as.integer(cm_1$table[1])
  False_defaulter_FP <- as.integer(cm_1$table[2])
  
  False_non_defaulter_FN <- as.integer(cm_1$table[3])
  True_defaulter_TN <- as.integer(cm_1$table[4])
  
  ####ROC Curve
  ####AUC (area under curve)
  #### Lift chart
  
  #------------------------------------------------------------------------------#
  #--------------------RESULTS: BUSINESS OUTCOME---------------------------------#
  #------------------------------------------------------------------------------#
  
  Default_Cost = -5000 #as provided in the case
  No_Default_Profit = 1500#as provided in the case
  
  expected_return = (Default_Cost*False_non_defaulter_FN) + (No_Default_Profit*True_non_defaulter_TP)
  print(paste0("The expected return with this T-threshold is: ", expected_return))
  my_expected_value_list[counter] <- expected_return
  
  if (expected_return > max_expected_return) {
    best_t_threshold <- t_threshold
  }
  
  if (expected_return > max_expected_return) {
    max_expected_return <- expected_return
  }
  
  print("")
  print("")
}

print(paste0("The maximum expected return is realized with T-threshold = ", best_t_threshold))
print(paste0("The maximum expected return equals ", max_expected_return))

###Final output logistic regression
my_t_threshold_list
my_expected_value_list

output_logistic_exp_value <- do.call(rbind, Map(data.frame, A=my_t_threshold_list, B=my_expected_value_list))

ggplot(output_logistic_exp_value, aes(x=A, y=B)) +
  geom_line(color="darkgrey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  theme_ipsum() +
  ggtitle("T-Threshold (#) vs. Total Expected Value ($)") +
  xlab("T-threshold") +
  ylab("Expected Return")


##TO do: 
#1.Create diff features, 
#2.finalize the logistic regression results, 
#3.Determine the Optimal T-threshold returning the highest expected value
#4.Exploratory data analysis in Tableau to identify features to be added
#5.Plot the payoffs for different T-thresholds










