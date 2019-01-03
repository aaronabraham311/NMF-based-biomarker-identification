# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: January 2, 2019

# TO DO:
# Implement machine learning frameworks (KNN, Random Forest, XGBoost, SVM, hierarchical clustering, LogReg)
# Implement ensemble ML framework

# Libraries
library(caret)

machineLearning <- function (
  train, 
  test,
  predictor) {
  
  # Factorizing predictor
  train[,predictor] <- as.factor(train[,predictor])
  test[,predictor] <- as.factor(test[,predictor])
  
  # Shuffling training set
  train <- train[sample(nrow(train)), ]
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