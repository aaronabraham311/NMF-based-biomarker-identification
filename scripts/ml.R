# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: January 2, 2019

# TO DO:
# Implement machine learning frameworks (KNN, Random Forest, XGBoost, SVM, hierarchical clustering, LogReg)
# Implement ensemble ML framework
# Save models as .RDS

# Libraries
library(caret)

baseML <- function (
  train, 
  test,
  predictor,
  model.address) {
  
  # Factorizing predictor
  train[,predictor] <- as.factor(train[,predictor])
  test[,predictor] <- as.factor(test[,predictor])
  
  # Shuffling training set
  train <- train[sample(nrow(train)), ]
  
  # Control parameters
  controlParameters <- trainControl(
    method = "LOOCV", #Leave one out cross validation
    savePrediction = TRUE,
    classProbs = TRUE
  )
  
  # Models
  rf <- trainPredict(train, test, method = "rf", controlParameters, model.address) # Random forest
  knn <- trainPredict(train, test, method = "knn", controlParameters, model.address) # K-nearest neighbors
  xgb <- trainPredict(train, test, method = "xgbTree", controlParameters, model.address) # XGBoost
  svm <- trainPredict(train, test, method = "svmRadial", controlParameters, model.address) # Support vector machines
  log <- trainPredict(train, test, method = "glm", controlParameters, model.address) # Logistic regression
}

# General train and predict function. 
trainPredict <- function (
  train,
  test,
  method,
  controlParameters,
  model.address) {
  # Training model
  model <- train(diagnosis ~.,
                 data = train,
                 method = method,
                 trControl = controlParameters)
  
  # Predictions and accuracy metrics
  testPredictions <- predict(model, test) # Could use probabilities
  confMatrix <- table(predictions = testPredictions, actual = test$diagnosis)
  accuracyMetric <- accuracy(confMatrix)
  mccMetric <- mcc(confMatrix)
  
  returnValues <- list("model" = model, "testPredictions" = testPredictions, 
                       "confusionMatrix" = confMatrix, "accuracy" = accuracyMetric,
                       "mcc" = mccMetric)
  
  # Writing model
  saveRDS(model, file = paste(model.address, method,".RDS"))
  
  return(returnValues)
}

# Ensemble function via XGBoost 
ensemble <- function (
  rf,
  knn,
  xgb,
  log,
  svm,
  train,
  test,
  controlParameters) {
  
  # Getting predictions on train and test set
  rfPredictions <- predict(rf, train)
  knnPredictions <- predict(knn, train)
  xgbPredictions <- predict(xgb, train)
  logPredictions <- predict(log, train)
  svmPredictions <- predict(svm, train)
  
  test$rfPredictions <- predict(rf, test)
  test$knnPredictions <- predict(knn, test)
  test$xgbPredictions <- predict(xgb, test)
  test$logPredictions <- predict(log, test)
  test$svmPredictions <- predict(svm, test)
  
  # Combining new training sets 
  predDF <- data.frame(rfPredictions, knnPredictions, xgbPredictions, logPredictions,
                       svmPredictions, diagnosis = train$diagnosis)
  
  # Training XGB model
  ensembleModel <- model <- train(diagnosis ~.,
                                  data = predDF,
                                  method = "xgb",
                                  trControl = controlParameters)
  
  # Getting accuracies of model
  ensemblePredict <- predict(ensembleModel, test, type = "prob")
  confMatrix <- table(predictions = ensemblePredict, actual = test$diagnosis)
  accuracyMetric <- accuracy(confMatrix)
  mccMetric <- mcc(confMatrix)
  
  # Returning values
  returnValues <- list("model" = ensembleModel, "predictions" = ensemblePredict, 
                       "confusionMatrix" = confMatrix, "accuracy" = accuracyMetric,
                       "mcc" = mccMetric)
  return(returnValues)
}

# Function for accuracy
accuracyMetric <- function (conMatrix) {
  tp <- conMatrix[1,1]
  fp <- conMatrix[1,2]
  fn <- conMatrix[2,1]
  tn <- conMatrix[2,2]
  
  accuracy <- (tp + tn)/(tp + fp + fn + tn)
  return(accuracy)
}

# Function for Matthew's correlation coefficient
mcc <- function(conMatrix) {
  tp <- conMatrix[1,1]
  fp <- conMatrix[1,2]
  fn <- conMatrix[2,1]
  tn <- conMatrix[2,2]
  
  metric <- ((tp * tn) - (fp * fn))/(sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)))
  return(metric)
}