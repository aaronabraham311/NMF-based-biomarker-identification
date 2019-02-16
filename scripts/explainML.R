# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: February 14, 2019

library(iml)
library(caret)

baseExplainFunction <- function (
  train,
  test,
  model,
  feature,
  classify,
  output.address,
  file
)
{
  # Creating Predictor object
  X <- train[which(names(train) != classify)]
  y <- as.factor(train[,classify])
  predictor = Predictor$new(model, data = X, y)
  
  featureExplainability(predictor, feature, output.address, file)
  modelExplainability(model, predictor, output.address, file)
}

featureExplainability <- function(predictor, feature, output.address, file)
{
  # ALE Plot
  #ale = FeatureEffect$new(predictor, feature, method = "ale")
  #imageSave(ale, output.address, file, ale.plot.title, method = ".ale")
  
  # PDP Plot
  pdp = Partial$new(predictor, feature)
  
  png(filename = paste(output.address, file, "pdp.png", sep = ""))
  plot(pdp) 
  dev.off()
}

modelExplainability <- function(model, predictor, output.address,file)
{
  # Shapley Plot 
  shapley = Shapley$new(predictor, x.interest = X[1,])
  
  png(filename = paste(output.address, file, "shap.png", sep = ""))
  plot(shapley) 
  dev.off()
  
  # Permutation Plot
  object <- varImp(model)
  png(filename = paste(output.address, file, "varImp.png", sep = ""))
  plot(object) 
  dev.off()
}