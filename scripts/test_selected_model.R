rm(list = ls())
library(tidyverse)
library(randomForest)
library(ROCit)
set.seed(123)
OUTPUT = TRUE

##################################################################
# Description: Testing model 5 (model that performed best in training) on test set. Model 5 is as follows:
# Party Change within the next 4 years ~ Participation Rate +
# Unemployment Rate + 
# GDP % Change + 
# gdp_pchange_ma4_8_diff + gdp + 
# gdp_ma4_8_diff + 
# gdp_pchange_ma4_8_diff +
# uer_ma4_8_diff 
##################################################################

fileName = paste0("../output/testing/testing_model5.pdf")

test_df <- read.csv("../data_processed/party_change_and_econ_df_test.csv")  %>% select(-1) # Remove row numbers
test_df <- transform(test_df, change_within_4years = as.factor(change_within_4years))

training_df <- read.csv("../data_processed/party_change_and_econ_df_training.csv")  %>% select(-1) # Remove row numbers
training_df <- transform(training_df, change_within_4years = as.factor(change_within_4years))

# Train model
fitted_rf <- randomForest(
  formula = change_within_4years ~ participation_rate + unemployment_rate + gdp_pchange + gdp + gdp_ma4_8_diff + gdp_pchange_ma4_8_diff + uer_ma4_8_diff, 
  data = training_df
)

# Use model to predict test set
pred_prob_rf <- as.data.frame(predict(fitted_rf, newdata=test_df, type = "prob"))

# Create ROC curve object and ks plot
roc_df <- data.frame(rf_score = pred_prob_rf$`TRUE`, 
                     label = ifelse(as.factor(test_df$change_within_4years) == "FALSE", "Negative", "Positive"))


rf_ROC_obj <- rocit(score=roc_df$rf_score,class=roc_df$label)
rf_ROC_ci <- ciROC(rf_ROC_obj, level = 0.95)

positive_dist <- roc_df %>% filter(label == "Positive")
negative_dist <- roc_df %>% filter(label == "Negative")
rf_ks_test_res <- ks.test(positive_dist$rf_score, negative_dist$rf_score)


# Plotting
{
  
  pdf(fileName)

  plot(rf_ROC_obj)
  lines(rf_ROC_ci$LowerTPR ~ rf_ROC_ci$FPR)
  lines(rf_ROC_ci$UpperTPR ~ rf_ROC_ci$FPR)
  text(x = 0.8, y = 0.2, labels = paste0("AUC = ", round(rf_ROC_obj$AUC, 3)))
  title("Random Forest ROC on Model: Party Change within 4 Years ~ \n Part.% + Unemp.% + GDP + GDP %Change + Moving Avg Diffs")
  
  
  rf_ksplot <- ksplot(rf_ROC_obj)
  text(x = 0.6, y= 0.45, label = paste0("p-value = ", round(rf_ks_test_res$p.value, 3)))
  text(x = 0.6, y =0.35, label = paste0("Optimal cutoff = ", round(0.36, 3)))
  
  # Plot the two distributions 
  
  p <- ggplot(data = roc_df, aes(x = rf_score, group = label, fill = label), alpha = 0.5) + 
    geom_histogram() +
    geom_vline(xintercept = 0.36, color = "orange", alpha=0.5) +
    geom_text(aes(x=0.36, label="RF Optimal Cutoff", y=130), colour="black") +
    labs(x = "Random Forest Score", y = "Count", fill = "Party Change \nwithin 4 Years") + 
    ggtitle("Random Forest Score by +/- Distributions")
  print(p)
  
  dev.off()
}

# Test set AUC is still better than the second best model's AUC in training.

# Using "optimal cutoff" from TRAINING set, not from testing set.
roc_df$predicted_label <- as.factor(ifelse(roc_df$rf_score >  0.36, "Positive", "Negative"))
cm <- table(predicted = roc_df$predicted_label, actual = roc_df$label)
cm
print(paste0("Accuracy = ", round((cm[1,1]+cm[2,2])/nrow(roc_df), 4)*100, "%")) # 68.02





