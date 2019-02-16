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
  file,
  perm.plot.title,
  ale.plot.title,
  pdp.plot.title,
  shap.plot.title
)
{
  # Creating Predictor object
  X <- train[which(names(combined_data) != classify)]
  y <- as.factor(train[,classify])
  predictor = Predictor$new(model, data = X, y)
  
  featureExplainability(predictor, feature, output.address, file, pdp.plot.title, ale.plot.title)
  modelExplainability(predictor, output.address, file, shap.plot.title, perm.plot.title)
}

imageSave <- function(object, output.address, file, title)
{
  png(filename = paste(output.address, file, sep = ""))
  plot(object, main = title)
  dev.off()
}

featureExplainability <- function(predictor, feature, output.address, file, pdp.plot.title, ale.plot.title)
{
  # ALE Plot
  ale = FeatureEffect$new(predictor, feature, method = "ale")
  imageSave(ale, output.address, file, ale.plot.title)
  
  # PDP Plot
  pdp = FeatureEffect$new(predictor, feature, method = "pdp")
  imageSave(pdp, output.address, file, pdp.plot.title)
}

modelExplainability <- function(model, predictor, output.address,file,shap.plot.title, perm.plot.title)
{
  # Shapley Plot 
  shapley = Shapley$new(predictor, x.interest = X[1,])
  imageSave(shapley, output.address, file, shap.plot.title)
  
  # Permutation Plot
  imageSave(varImp(model), output.address, file, perm.plot.title)
}