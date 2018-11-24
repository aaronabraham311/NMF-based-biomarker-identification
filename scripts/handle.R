# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# TO DO
# Fold selection
# Threshold, ceiling
# Column rank normalization
# Variation filter

handle <- function (
  data ){
  scaledData <- normalize(data)
  
}

# Scaling all variables to mean of 0 and SD of 1
normalize <- function (
  data){
  scaledData <- scale(data)
  return (scaledData)
}