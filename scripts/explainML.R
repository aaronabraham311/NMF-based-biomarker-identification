# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: February 14, 2019

library(iml)
library(caret)
library(PIMP)

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
  levels(y) <- list(Control = "1", MCI = "2", AD = "3")
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
  pdp$center(min(X[,feature]))
  
  png(filename = paste(output.address, file, ".pdp.png", sep = ""))
  pdp$plot() + ggtitle(feature)
  dev.off()
}

modelExplainability <- function(model, predictor, output.address,file)
{
  # Shapley Plot 
  shapley = Shapley$new(predictor, x.interest = X[1,])
  
  png(filename = paste(output.address, file, "shap_1.png", sep = ""))
  shapley$plot() + ggtitle("Shapley Plot for Sample 1")
  dev.off()
  
  shapley = Shapley$new(predictor, x.interest = X[50, ])
  
  png(filename = paste(output.address, file, "shap_50.png", sep = ""))
  plot(shapley) + ggtitle("Shapley Plot for Sample 50")
  dev.off()
  
  shapley = Shapley$new(predictor, x.interest = X[90, ])
  
  png(filename = paste(output.address, file, "shap_90.png", sep = ""))
  plot(shapley) + ggtitle("Shapley Plot for Sample 90")
  dev.off()
  
  # Permutation Plot
  object <- varImp(model)
  png(filename = paste(output.address, file, "varImp.png", sep = ""))
  plot(object) + ggtitle("Permuatation Plot for Specific Model")
  dev.off()
}