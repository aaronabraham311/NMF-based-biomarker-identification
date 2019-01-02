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
  
}