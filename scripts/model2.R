rm(list = ls())
library(tidyverse)
library(randomForest)
library(ROCit)
set.seed(123)

OUTPUT = TRUE

##################################################################
# Description: Training and cross validation for
# Model: Party Change next year ~ Participation Rate + Unemployment Rate + GDP % Change
##################################################################

data_df <- read.csv("../data_processed/party_change_and_econ_df_training.csv") %>% select(-1) # Remove row numbers
data_df <- transform(data_df, change_next_year = as.factor(change_next_year))

# Leave one out cross validation
roc_df <- data.frame(matrix(NA, ncol = 3, nrow = nrow(data_df)))
colnames(roc_df) <- c("label", 'rf_score', "logit_score")

for (i in 1:nrow(data_df)) {
  
  if (i %% 100 == 0) {print(paste0("Iteration: ", i, " out of ", nrow(data_df)))}
  loo_index <- i
  cv_test_set <- data_df[loo_index,]
  cv_training_set <- data_df[-loo_index,]
  
  fitted_rf <- randomForest(
    formula = change_next_year ~ participation_rate + unemployment_rate + gdp_pchange,
    data = cv_training_set
  )
  
  fitted_logit <- glm(
    formula = change_next_year ~ participation_rate + unemployment_rate + gdp_pchange, 
    data = cv_training_set, 
    family = binomial
  )
  
  pred_prob_rf <- as.data.frame(predict(fitted_rf, newdata=cv_test_set, type = "prob"))
  pred_prob_logit <- predict(fitted_logit, newdata = cv_test_set, type = "response")
  
  roc_df[i,]$label <- cv_test_set$change_next_year
  roc_df[i,]$rf_score <- pred_prob_rf$`TRUE`
  roc_df[i,]$logit_score <- pred_prob_logit
}

#saved_roc_df <- roc_df
roc_df <- roc_df %>% transform(label = as.factor(label))
roc_df$label <- ifelse(roc_df$label == "1", "Negative", "Positive")

#######################################################
# Logistic Regression Evaluation
#######################################################

##### ROC ##### 

logit_ROC_obj <- rocit(score=roc_df$logit_score,class=roc_df$label)
logit_ROC_ci <- ciROC(logit_ROC_obj, level = 0.95)

# https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html

##### KS-test ##### 
positive_dist <- roc_df %>% filter(label == "Positive")
negative_dist <- roc_df %>% filter(label == "Negative")
logit_ks_test_res <- ks.test(positive_dist$logit_score, negative_dist$logit_score)


#######################################################
# Random Forest Evaluation
#######################################################
rf_ROC_obj <- rocit(score=roc_df$rf_score,class=roc_df$label)
rf_ROC_ci <- ciROC(rf_ROC_obj, level = 0.95)

# https://cran.r-project.org/web/packages/ROCit/vignettes/my-vignette.html


##### KS-test ##### 

positive_dist <- roc_df %>% filter(label == "Positive")
negative_dist <- roc_df %>% filter(label == "Negative")
rf_ks_test_res <- ks.test(positive_dist$rf_score, negative_dist$rf_score)


###### Plotting ######

myFormulaName <- "model_2_next_year_part_unemp_gdp_pchange"
fileName = paste0("../output/cross_val/", myFormulaName, ".pdf")


{  
  pdf(fileName)
  
  ####### Plotting Logistic Regression ########
  
  plot(logit_ROC_obj)
  lines(logit_ROC_ci$LowerTPR ~ logit_ROC_ci$FPR)
  lines(logit_ROC_ci$UpperTPR ~ logit_ROC_ci$FPR)
  text(x = 0.8, y = 0.2, labels = paste0("AUC = ", round(logit_ROC_obj$AUC, 3)))
  title("Logistic Regression ROC on Model: Party Change next year ~ \n Part % + Unemp. % + GDP % Change")
  
  
  logit_ksplot <- ksplot(logit_ROC_obj)
  text(x = 0.3, y= 0.45, label = paste0("p-value = ", round(logit_ks_test_res$p.value, 3)))
  text(x = 0.3, y =0.35, label = paste0("Optimal cutoff = ", round(logit_ksplot$`KS Cutoff`, 3)))
  
  
  # Plot the two distributions
  p <- ggplot(data = roc_df, aes(x = logit_score, group = label, fill = label), alpha = 0.5) + 
    geom_histogram() +
    geom_vline(xintercept = logit_ksplot$`KS Cutoff`, color = "orange", alpha=0.5) +
    geom_text(aes(x=logit_ksplot$`KS Cutoff`, label="Logit Optimal Cutoff", y=230), colour="black") +
    labs(x = "Logistic Regression Score", y = "Count", fill = "Party Change \nnext year") + 
    ggtitle("Logistic Regression Scores by +/- Distributions")
  print(p)
  
  ####### 2. Plotting Random Forest ########
  
 
  plot(rf_ROC_obj)
  lines(rf_ROC_ci$LowerTPR ~ rf_ROC_ci$FPR)
  lines(rf_ROC_ci$UpperTPR ~ rf_ROC_ci$FPR)
  text(x = 0.8, y = 0.2, labels = paste0("AUC = ", round(rf_ROC_obj$AUC, 3)))
  title("Random Forest ROC on Model: Party Change next year ~ \n Part % + Unemp % + GDP % Change")
  
  
  rf_ksplot <- ksplot(rf_ROC_obj)
  text(x = 0.6, y= 0.45, label = paste0("p-value = ", round(rf_ks_test_res$p.value, 3)))
  text(x = 0.6, y =0.35, label = paste0("Optimal cutoff = ", round(rf_ksplot$`KS Cutoff`, 3)))
  
  
  # Plot the two distributions 
  p <- ggplot(data = roc_df, aes(x = rf_score, group = label, fill = label), alpha = 0.5) + 
    geom_histogram() +
    geom_vline(xintercept = rf_ksplot$`KS Cutoff`, color = "orange", alpha=0.5) +
    geom_text(aes(x=rf_ksplot$`KS Cutoff`, label="RF Optimal Cutoff", y=130), colour="black") +
    labs(x = "Random Forest Score", y = "Count", fill = "Party Change \nnext year") + 
    ggtitle("Random Forest Score by +/- Distributions")
  print(p)
  
  dev.off()
}



