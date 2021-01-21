#-------------------------------------------------------------------------------
#-----------CART and Random Forest models
#-------------------------------------------------------------------------------


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

# There are two families of CART algorithms: conditoinal interence trees (ctree function; caret package) and recursive partitioning (rpart function; partykit package)

###
### CTREE 
###

ctree_tree<-ctree(default_0~ 
                    LIMIT_BAL + 
                    SEX + 
                    EDUCATION + 
                    MARRIAGE +
                    AGE + 
                    PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                    PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                    #+ Delta_Bill_AMT1_vs_Bill_AMT2 + Delta_Bill_AMT2_vs_Bill_AMT3 + Delta_Bill_AMT3_vs_Bill_AMT4 +Delta_Bill_AMT4_vs_Bill_AMT5 + Delta_Bill_AMT5_vs_Bill_AMT6 + 
                    #PER_Delta_Bill_AMT1_vs_Bill_AMT2 + PER_Delta_Bill_AMT2_vs_Bill_AMT3 + PER_Delta_Bill_AMT3_vs_Bill_AMT4 + PER_Delta_Bill_AMT4_vs_Bill_AMT5 + PER_Delta_Bill_AMT5_vs_Bill_AMT6 +
                    #Balance_remaining_1 + Balance_remaining_2 + Balance_remaining_3 + Balance_remaining_4 +Balance_remaining_5 + Balance_remaining_6 +
                    #Limit_Alert_1 + Limit_Alert_2 + Limit_Alert_3 + Limit_Alert_4 + Limit_Alert_5 +Limit_Alert_6
                    ,data=cd_df_training) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=cd_df_testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",500)
ctree_classification[ctree_probabilities[,2]<0.6073]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$default_0 == "1"))
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,cd_df_testing$default_0,positive = "1")

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=cd_df_testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], cd_df_testing$default_0) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_probabilities[,2],  cd_df_testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

###
### RPART
###

# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0001) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0~ 
                    LIMIT_BAL + 
                    SEX + 
                    EDUCATION + 
                    MARRIAGE +
                    AGE + 
                    PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                    PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                  #+ Delta_Bill_AMT1_vs_Bill_AMT2 + Delta_Bill_AMT2_vs_Bill_AMT3 + Delta_Bill_AMT3_vs_Bill_AMT4 +Delta_Bill_AMT4_vs_Bill_AMT5 + Delta_Bill_AMT5_vs_Bill_AMT6 + 
                  #PER_Delta_Bill_AMT1_vs_Bill_AMT2 + PER_Delta_Bill_AMT2_vs_Bill_AMT3 + PER_Delta_Bill_AMT3_vs_Bill_AMT4 + PER_Delta_Bill_AMT4_vs_Bill_AMT5 + PER_Delta_Bill_AMT5_vs_Bill_AMT6 +
                  #Balance_remaining_1 + Balance_remaining_2 + Balance_remaining_3 + Balance_remaining_4 +Balance_remaining_5 + Balance_remaining_6 +
                  #Limit_Alert_1 + Limit_Alert_2 + Limit_Alert_3 + Limit_Alert_4 + Limit_Alert_5 +Limit_Alert_6
                  ,data=cd_df_training, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.0015) #Prune the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=cd_df_testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,cd_df_testing$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=cd_df_testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], cd_df_testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class, cd_df_testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart



#------------------------------------------------------------------------------#
#------------------------Random Forest ----------------------------------------#
#------------------------------------------------------------------------------#


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code


model_forest <- randomForest(default_0~ 
                               LIMIT_BAL + 
                               SEX + 
                               EDUCATION + 
                               MARRIAGE +
                               AGE + 
                               PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                               BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + 
                               PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                             #+ Delta_Bill_AMT1_vs_Bill_AMT2 + Delta_Bill_AMT2_vs_Bill_AMT3 + Delta_Bill_AMT3_vs_Bill_AMT4 +Delta_Bill_AMT4_vs_Bill_AMT5 + Delta_Bill_AMT5_vs_Bill_AMT6 + 
                             #PER_Delta_Bill_AMT1_vs_Bill_AMT2 + PER_Delta_Bill_AMT2_vs_Bill_AMT3 + PER_Delta_Bill_AMT3_vs_Bill_AMT4 + PER_Delta_Bill_AMT4_vs_Bill_AMT5 + PER_Delta_Bill_AMT5_vs_Bill_AMT6 +
                             #Balance_remaining_1 + Balance_remaining_2 + Balance_remaining_3 + Balance_remaining_4 +Balance_remaining_5 + Balance_remaining_6 +
                             #Limit_Alert_1 + Limit_Alert_2 + Limit_Alert_3 + Limit_Alert_4 + Limit_Alert_5 +Limit_Alert_6
                             , data=cd_df_training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot

varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=cd_df_testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",500)
forest_classification[forest_probabilities[,2]<0.5]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,cd_df_testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=cd_df_testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], cd_df_testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  cd_df_testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)


