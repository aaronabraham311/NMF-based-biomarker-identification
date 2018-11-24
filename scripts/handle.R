# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# TO DO
# Fold selection
# Threshold, ceiling
# Column rank normalization
# Variation filter

handle <- function (
  data,
  ceiling,
  threshold){
  scaledData <- normalize(data)
  replacedData <- thresholdAndCeiling(data, ceiling, threshold)
}

# Scaling all variables to mean of 0 and SD of 1
normalize <- function (
  data){
  scaledData <- scale(data)
  return (scaledData)
}

# Replaces all data values with threshold and ceiling to remove huge outliers
thresholdAndCeiling <- function (
  data, 
  ceiling,
  threshold) {
  data <- apply(data, 2, function(col) pmax(col, threshold))
  data <- apply(data, 2, function(col) pmin(col, ceiling))
}