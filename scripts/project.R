# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# TO DO:
# Project NMF
# Save H and W matrices
# Construct projection 
# Return transformed data and all matrices

# Libraries
library("NMF") #https://cran.r-project.org/web/packages/NMF/vignettes/NMF-vignette.pdf

projectNMF <- function (
  data,
  data.address,
  ncol,
  nrow,
  k,
  niter = 1000,
  theta = 0,
  lamba = 1) {
  
  # Output start of methodology
  print(c("Running projectNMF on the following dataset: ", data.address))
  
  # Default NMF 
  nmfReduced <- nmf(data, k)
  w <- basis(nmfReduced) # convert to data table? 
  h <- coef(nmfReduced) # convert to data table?
  features <- extractFeatures(nmfReduced)
  
  return(nmfReduced, w, h, features)
}