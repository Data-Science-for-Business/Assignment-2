t_threshold = 0.263


print(paste0("Current T-threshold applied to classification: ", t_threshold))

cd_df_logistic_classification <-rep("1",nrow(cd_df_testing)) #Generate number of rows depending on size of training set
#cd_df_logistic_classification[cd_df_model_logistic_stepwiseAIC_probabilities<t_threshold]="0"
cd_df_logistic_classification[cd_df_model_logistic_stepwiseAIC_probabilities<t_threshold]="0"
print("T-threshold applied...")

cd_df_logistic_classification<-as.factor(cd_df_logistic_classification)

cm_2 <- confusionMatrix(cd_df_logistic_classification, 
                        cd_df_testing$default_0, 
                        positive = "0",) ###############---> According to data dictionary, positive outcome = 0

cm_2

True_non_defaulter_TP <- as.integer(cm_2$table[1])
False_defaulter_FP <- as.integer(cm_2$table[2])

False_non_defaulter_FN <- as.integer(cm_2$table[3])
True_defaulter_TN <- as.integer(cm_2$table[4])

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
cm_1
