

test_df <- read.csv("../data_processed/party_change_and_econ_df_test.csv")  %>% select(-1) # Remove row numbers
test_df <- transform(test_df, change_within_4years = as.factor(change_within_4years))

training_df <- read.csv("../data_processed/party_change_and_econ_df_training.csv")  %>% select(-1) # Remove row numbers
training_df <- transform(training_df, change_within_4years = as.factor(change_within_4years))

# Train model
set.seed(123)
fitted_rf <- randomForest(
  formula = change_within_4years ~ participation_rate + unemployment_rate + gdp_pchange + gdp + gdp_ma4_8_diff + gdp_pchange_ma4_8_diff + uer_ma4_8_diff, 
  data = training_df
)

# Use model to predict test set
pred_prob_rf <- as.data.frame(predict(fitted_rf, newdata=test_df, type = "prob"))

# Create ROC curve object and ks plot
roc_df <- data.frame(rf_score = pred_prob_rf$`TRUE`, 
                     label = ifelse(as.factor(test_df$change_within_4years) == "FALSE", "Negative", "Positive"))

# roc_df <- roc_df %>% transform(label = as.factor(label))
# roc_df$label <- ifelse(roc_df$label == "1", "Negative", "Positive")

rf_ROC_obj <- rocit(score=roc_df$rf_score,class=roc_df$label)
rf_ROC_ci <- ciROC(rf_ROC_obj, level = 0.95)

plot(rf_ROC_obj)
lines(rf_ROC_ci$LowerTPR ~ rf_ROC_ci$FPR)
lines(rf_ROC_ci$UpperTPR ~ rf_ROC_ci$FPR)
text(x = 0.8, y = 0.2, labels = paste0("AUC = ", round(rf_ROC_obj$AUC, 3)))
title("Random Forest ROC on Model: Party Change within 4 Years ~ \n Part.% + Unemp.% + GDP + GDP %Change + Moving Avg Diffs")



rf_ksplot <- ksplot(rf_ROC_obj)
text(x = 0.6, y= 0.45, label = paste0("p-value = ", round(rf_ks_test_res$p.value, 3)))
text(x = 0.6, y =0.35, label = paste0("Optimal cutoff = ", round(rf_ksplot$`KS Cutoff`, 3)))

# Plot the two distributions 

p <- ggplot(data = roc_df, aes(x = rf_score, group = label, fill = label), alpha = 0.5) + 
  geom_histogram() +
  geom_vline(xintercept = rf_ksplot$`KS Cutoff`, color = "orange", alpha=0.5) +
  geom_text(aes(x=rf_ksplot$`KS Cutoff`, label="RF Optimal Cutoff", y=130), colour="black") +
  labs(x = "Random Forest Score", y = "Count", fill = "Party Change \nwithin 4 Years") + 
  ggtitle("Random Forest Score by +/- Distributions")
print(p)

# Test set AUC is still better than the second best model's AUC in training.




