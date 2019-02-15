# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: February 14, 2019

library(iml)

baseExplainFunction <- function (
  combined_data,
  model,
  feature,
  classify,
  output.address,
  perm.plot.title,
  ale.plot.title,
  shap.plot.title
)
{
  # Creating Predictor object
  X = combined_data[which(names(combined_data) != classify)]
  predictor = Predictor$new(model, data = X, y = combined_data[,classify])
  
  # Permutation importance of features
  importance = FeatureImp$new(predictor, loss = "mae")
  
  #Intializing file
  png(filename = paste(output.address, file))
  
  plot(importance, main = perm.plot.title)
  
  #Saving plot
  dev.off()
  
  # ALE Plot
  ale = FeatureEffect$new(predictor, feature)
  
  #Intializing file
  png(filename = paste(output.address, file))
  
  plot(ale, main = ale.plot.title)
  
  #Saving plot
  dev.off()
  
  # Shapley Plot
  shapley = Shapley$new(predictor, x.interest = X[1,])
  
  #Intializing file
  png(filename = paste(output.address, file))
  
  plot(shapley, main = shap.plot.title)
  
  #Saving plot
  dev.off()
  
}