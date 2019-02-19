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
  
  # Removing ID
  if ("RID" %in% colnames(train))
  {
    train$RID <- NULL
    test$RID <- NULL
  }
  
  # Factorizing predictor
  train[,predictor] <- as.factor(train[,predictor])
  test[,predictor] <- as.factor(test[,predictor])
  levels(train$diagnosis) <- make.names(levels(factor(train$diagnosis)))
  levels(test$diagnosis) <- make.names(levels(factor(test$diagnosis)))
  
  # Shuffling training set
  train <- train[sample(nrow(train)), ]
  
  # Control parameters
  controlParameters <- trainControl(
    method = "cv",
    number = 10, #10 fold cross validation
    savePrediction = TRUE,
    classProbs = F,
    sampling = "up",
    search = "random"
  )
  
  # Models
  rf <- trainPredict(train, test, method = "rf", controlParameters, model.address) # Random forest
  knn <- trainPredict(train, test, method = "knn", controlParameters, model.address) # K-nearest neighbors
  xgb <- trainPredict(train, test, method = "xgbLinear", controlParameters, model.address) # XGBoost
  svm <- trainPredict(train, test, method = "svmRadial", controlParameters, model.address) # Support vector machines
  #ada <- trainPredict(train, test, method = "adaboost", controlParameters, model.address) # Adaboost
  ensembleModel <- ensemble(rf$model, knn$model, xgb$model, svm$model, train, test, model.address, controlParameters) # Ensemble model
  
  # Writing data
  writeData(rf, knn, xgb, svm, ensembleModel, model.address)
  returnValues <- c(rf, knn, xgb, svm, ensembleModel)
  return (returnValues)
}

linearDiscriminantAnalysis <- function (train, test, output.address)
{
  # Data clean
  data <- data.matrix(data)
  row.names(data) <- data[,"RID"]
  data$diagnosis <- as.factor(data$diagnosis)
  
  # Normality
  data <- apply(data[-c(RID,diagnosis),], 2, scale) # Scaling all variables except RID and diagnosis
  
  # LDA construction
  model <- lda(formula = diagnosis ~ ., data = train, CV = TRUE)
  
  # Secondary stats
  prop = model$svd^2/sum(model$svd^2) # Propoprtion of between-class variance explained
  
  # LDA test
  testPredictions <- predict(model, test)
  confMatrix <- table(predictions = testPredictions, actual = test$diagnosis)
  accuracyMetric <- accuracy(confMatrix)
  
  # Writing data into .txt file
  date.string <- date()
  date.string2 <- paste(unlist(strsplit(date.string, " ")), sep="_", collapse="_")
  date.string3 <- paste(unlist(strsplit(date.string2, ":")), sep="_", collapse="_")
  params.file <- paste(model.address, date.string3, ".lda.results.txt", sep="")

  write(c("Linear Discriminant Analysis Results on ", date.string2), file= params.file, ncolumns=100, append=F)
  write(c("  "), file= params.file, ncolumns=100, append=T)
  
  write.table(c("Confusion matrix" ,rForest$confusionMatrix), file = params.file, append = T)
  write.table(c("Accuracy" ,rForest$accuracy), file = params.file, append = T)
  
  returnValues <- list("model" = model, "testPredictions" = testPredictions, 
                       "confusionMatrix" = confMatrix, "accuracy" = accuracyMetric, "prop_explained" = prop)
  return(returnValues)
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
                 trControl = controlParameters,
                 tuneLength = 15)
  
  # Predictions and accuracy metrics
  testPredictions <- predict(model, test) # Could use probabilities
  confMatrix <- table(predictions = testPredictions, actual = test$diagnosis)
  accuracyMetric <- accuracy(confMatrix)
  
  returnValues <- list("model" = model, "testPredictions" = testPredictions, 
                       "confusionMatrix" = confMatrix, "accuracy" = accuracyMetric)
  
  # Writing model
  saveRDS(model, file = paste(model.address, method,".RDS", sep = ""))
  
  return(returnValues)
}

# Ensemble function via XGBoost 
ensemble <- function (
  rforest,
  knn,
  xgb,
  svm,
  train,
  test,
  model.address,
  controlParameters) {
  
  # Getting predictions on train set
  rfPredictions <- predict(rforest, train)
  knnPredictions <- predict(knn, train)
  xgbPredictions <- predict(xgb, train)
  svmPredictions <- predict(svm, train)
  
  # Combining new training sets 
  predDF <- data.frame(rfPredictions, knnPredictions, xgbPredictions,
                       svmPredictions, diagnosis = train$diagnosis)
  
  # Prediction on testing set and combining
  rfPredictions <- predict(rforest, test)
  knnPredictions <- predict(knn, test)
  xgbPredictions <- predict(xgb, test)
  svmPredictions <- predict(svm, test)
  test <- data.frame(rfPredictions, knnPredictions, xgbPredictions,
                     svmPredictions, diagnosis = test$diagnosis)
  
  # Training XGB model
  ensembleModel <- train(diagnosis ~.,
                          data = predDF,
                          method = "xgbLinear",
                          trControl = controlParameters)
  
  # Getting accuracies of model
  ensemblePredict <- predict(ensembleModel, test)
  confMatrix <- table(predictions = ensemblePredict, actual = test$diagnosis)
  accuracyMetric <- accuracy(confMatrix)
  
  # Writing model
  saveRDS(ensembleModel, file = paste(model.address, "ensemble.RDS"))
  
  # Returning values
  returnValues <- list("model" = ensembleModel, "predictions" = ensemblePredict, 
                       "confusionMatrix" = confMatrix, "accuracy" = accuracyMetric)
  return(returnValues)
}

writeData <- function (
  rForest,
  knn,
  xgb,
  svm,
  ensemble,
  model.address) {
  # Writing data into .txt file
  date.string <- date()
  date.string2 <- paste(unlist(strsplit(date.string, " ")), sep="_", collapse="_")
  date.string3 <- paste(unlist(strsplit(date.string2, ":")), sep="_", collapse="_")
  params.file <- paste(model.address, date.string3, ".results.txt", sep="")
  
  write(c("Machine Learning Results on ", date.string2), file= params.file, ncolumns=100, append=F)
  write(c("  "), file= params.file, ncolumns=100, append=T)
  
  write.table(c("Random forest confusion matrix" ,rForest$confusionMatrix), file = params.file, append = T)
  write.table(c("Random forest accuracy" ,rForest$accuracy), file = params.file, append = T)
  
  write.table(c("K-nearest neighbors confusion matrix" ,knn$confusionMatrix), file = params.file, append = T)
  write.table(c("K-nearest neighbors accuracy" , knn$accuracy), file = params.file, append = T)
  
  write.table(c("XGBoost confusion matrix" ,xgb$confusionMatrix), file = params.file, append = T)
  write.table(c("XGBoost accuracy" ,xgb$accuracy), file = params.file, append = T)
  
  write.table(c("Support vector machine confusion matrix" ,svm$confusionMatrix), file = params.file, append = T)
  write.table(c("Support vector machine accuracy" ,svm$accuracy), file = params.file, append = T)
  
  write.table(c("Ensemble confusion matrix" ,ensemble$confusionMatrix), file = params.file, append = T)
  write.table(c("Ensemble accuracy" ,ensemble$accuracy), file = params.file, append = T)
  
}

# Function for accuracy
accuracy <- function (conMatrix) {
  tp <- conMatrix[1,1] + conMatrix[2,2] + conMatrix[3,3]
  total <- sum(conMatrix)
  
  accuracy <- tp/total
  return(accuracy)
}
