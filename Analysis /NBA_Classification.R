library(tidyverse)
library(caret)
library(rsample)
library(ROCR)
library(randomForest)
source("scrape_script.R")
set.seed(1001)

# Load data
nbadata <- read.csv("/Users/mbp/Documents/Side Projects/Classification of Playoff Teams/NBAdata.csv")

# set train and test sets:
inTrain <- createDataPartition(y = nbadata$playoffs, p = 0.70, list = FALSE)
nbadata_train <- nbadata[inTrain,]
nbadata_test <- nbadata[-inTrain,]

# correlation plot to observe data and correlated variables to the outcome
corrplot::corrplot(cor(nbadata_train[,c(3:20,22)]))

# Re-code outcome variable as a factor:
nbadata_train$playoffs <- factor(nbadata_train$playoffs, labels = c("0","1"))
nbadata_test$playoffs <- factor(nbadata_test$playoffs, labels = c("0","1"))

# construct logistic regression model:
logit1 <- train(playoffs ~ FGA + X3P + AST + FTM + DR + TO + STL, data = nbadata_train, 
                method = "glm", family = "binomial", 
                trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10))

summary(logit1)

# construct random forest model:
rf_model <- train(playoffs ~ FGA + X3P + AST + FTM + DR + TO + STL,
                  data = nbadata_train, method = 'rf', 
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10),
                  importance = TRUE, ntree = 300)
print(rf_model)

# construct naive bayes model:
nb_model <- train(playoffs ~ FGA + X3P + AST + FTM + DR + TO + STL,
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10),
                           method = "nb",
                           data = nbadata_train)
print(nb_model)

# construct svm model:
svm_model <- train(playoffs ~ FGA + X3P + AST + FTM + DR + TO + STL,
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10),
                  method = "svmLinear",
                  data = nbadata_train)
print(svm_model)

# predict playoff outcomes:
nbadata_test$preds <- predict(logit1, newdata = nbadata_test, type = "raw")
nbadata_test$preds_rf <- predict(rf_model, newdata = nbadata_test)
nbadata_test$preds_nb <- predict(nb_model, newdata = nbadata_test)
nbadata_test$preds_svm <- predict(svm_model, newdata = nbadata_test)

# confusion matrices:
confusionMatrix(data = nbadata_test$preds, reference = nbadata_test$playoffs)
confusionMatrix(nbadata_test$preds_rf, nbadata_test$playoffs)
confusionMatrix(nbadata_test$preds_nb, nbadata_test$playoffs)
confusionMatrix(nbadata_test$preds_svm, nbadata_test$playoffs)

# ROC curves & accuracy:
rocrpred <- prediction(as.numeric(nbadata_test$preds), as.numeric(nbadata_test$playoffs))
rocrpred_rf <- prediction(as.numeric(nbadata_test$preds_rf), as.numeric(nbadata_test$playoffs))
rocrpred_nb <- prediction(as.numeric(nbadata_test$preds_nb), as.numeric(nbadata_test$playoffs))
rocrpred_svm <- prediction(as.numeric(nbadata_test$preds_svm), as.numeric(nbadata_test$playoffs))

perf <- performance(rocrpred, "tpr", "fpr")
perf_rf <- performance(rocrpred_rf, "tpr", "fpr")
perf_nb <- performance(rocrpred_nb, "tpr", "fpr")
perf_svm <- performance(rocrpred_svm, "tpr", "fpr")

  par(mfrow = c(2,2))
  plot(perf, main = "ROC Curve - Logistic Regression")
  plot(perf_rf, main = "ROC Curve - Random Forest")
  plot(perf_nb, main = "ROC Curve - Naive Bayes")
  plot(perf_svm, main = "ROC Curve - SVM")

(auc_roc <- performance(rocrpred, measure = "auc")@y.values)
(auc_roc_rf <- performance(rocrpred_rf, measure = "auc")@y.values)
(auc_roc_nb <- performance(rocrpred_nb, measure = "auc")@y.values)
(auc_roc_svm <- performance(rocrpred_svm, measure = "auc")@y.values)
  
# Summarize accuracy of models:
kableExtra::kable((summmary_acc = data.frame(
    Model = c("Logistic Regression", "Random Forest", "Naive Bayes", "SVM"),
    TestAccuracy = c(unlist(auc_roc), unlist(auc_roc_rf), unlist(auc_roc_nb), unlist(auc_roc_svm)))), 
    format = "markdown")

# Predict playoff teams for NBA 2020 season (cut short due to Covid-19):

nba2020 <- getdata(2020,2020)
nba2020$pred_playoffs <- predict(logit1, newdata = nba2020, type = 'raw')
nba2020$preds_rf <- predict(rf_model, newdata= nba2020)
nba2020$preds_nb <- predict(nb_model, newdata = nba2020)
nba2020$preds_svm <- predict(svm_model, newdata = nba2020)

kableExtra::kable(cbind(nba2020 %>% filter(pred_playoffs==1) %>% select(teams),
nba2020 %>% filter(preds_rf==1) %>% select(teams),
nba2020 %>% filter(preds_nb==1) %>% select(teams),
nba2020 %>% filter(preds_svm==1) %>% select(teams)))


nbadata %>% filter(teams == "New York Knicks") %>%   
ggplot(., aes(x = factor(season), y = FG, color = factor(playoffs))) + 
geom_point()

# (rf_logit <- calc_acc(predicted = nbadata_test$preds, actual = nbadata_test$playoffs))
# (rf_acc <- calc_acc(predicted = nbadata_test$preds_rf, actual = nbadata_test$playoffs))
