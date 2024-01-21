rm(list = ls())


caravan_file_path <- file.choose()
caravan_data <- read_csv(caravan_file_path)
caravan_data$Purchase <- ifelse(caravan_data$Purchase == "Yes", 1, 0)

#a) Create a training set consisting of the first 1,000 observations, and a test set consisting
#of the remaining observations.

train_caravan <- 1:1000
caravan_train <- caravan_data[train_caravan,]
caravan_test <- caravan_data[-train_caravan,]

#b) Fit a boosting model to the training set with Purchase as the response and the other
#variables as predictors. Use 1,000 trees, and a shrinkage value of 0.01. Which predictors
#appear to be the most important?
  
set.seed(123)
boost_caravan <- gbm(Purchase ~ ., data = caravan_train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.01)
summary(boost_caravan)

#c) Use the boosting model to predict the response on the test data. Predict that a person
#will make a purchase if the estimated probability of purchase is greater than 20 %. Form
#a confusion matrix. What fraction of the people predicted to make a purchase do in fact
#make one? How does this compare with the results obtained from applying KNN or
#logistic regression to this data set?

probs_test <- predict(boost_caravan, caravan_test, n.trees = 1000, type = "response")
pred_test <- ifelse(probs_test > 0.2, 1, 0)
table(caravan_test$Purchase, pred_test)

logit_caravan <- glm(Purchase ~ ., data = caravan_train, family = "binomial")
probs_test2 <- predict(logit_caravan, caravan_test, type = "response")
pred_test2 <- ifelse(probs_test2 > 0.2, 1, 0)
table(caravan_test$Purchase, pred_test2)

