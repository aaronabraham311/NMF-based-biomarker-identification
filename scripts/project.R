# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# TO DO:
# Project NMF
# Save H and W matrices
# Construct projection 
# Return transformed data and all matrices

# Libraries
library("NNLM") #https://cran.r-project.org/web/packages/NNLM/vignettes/Fast-And-Versatile-NMF.html#background
library("MASS")

extractFactors <- function (
  data,
  output.address,
  indices,
  k,
  niter = 1000,
  theta = 0,
  lamba = 1) {
  
  # Output start of methodology
  print(c("Running extractFactors on the following dataset: ", output.address))
  
  # Transpose of matrix for factorization
  data <- data.matrix(data)
  row.names(data) <- data[,"RID"]
  labels <- data[,c("RID", "diagnosis")] # Removing labels such that it is not involved in NMF
  metaboliteData <- subset(data, select = -c(RID, diagnosis))
  
  metaboliteData <- t(metaboliteData)
  
  # Default NMF 
  decomp <- nnmf(metaboliteData[,indices], k, rel.tol = 1e-5)
  w <- decomp$W
  h <- decomp$H
  
  # Adding diagnosis row
  h <- rbind(h, labels[indices,"diagnosis"])
  
  w <- data.frame(w)
  h <- data.frame(h)
  row.names(h)[nrow(h)] <- "diagnosis"
  
  # Writing to train file
  train <- data.frame(t(h))
  
  # Writing to external file
  write.csv(w, paste(output.address, "w.csv"), sep = "", row.names = FALSE)
  write.csv(h, paste(output.address, 'h.csv'), sep = "",  row.names = FALSE)
  
  # Predicting on test set
  newH <- predict(decomp, metaboliteData[,-indices], which = "H")
  test <- newH$coefficients
  
  test <- rbind(test, labels[-indices, "diagnosis"])
  test <- data.frame(h)
  row.names(test)[nrow(test)] <- "diagnosis"
  
  test <- data.frame(t(test))
  
  returnValues <- list("decomp" = decomp, "w" = w, "h" = h, "train" = train, "test" = test)
  return(returnValues)
}