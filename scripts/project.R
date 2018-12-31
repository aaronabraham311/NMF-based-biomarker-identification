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
  ncol,
  nrow,
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
  decomp <- nnmf(metaboliteData, k, rel.tol = 1e-5)
  w <- decomp$W
  h <- decomp$H
  w <- cbind(w, labels[,"diagnosis"])
  colnames(w) <- c("V1", "V2", "V3", "diagnosis")
  
  # Writing to external file
  write.csv(w, paste(output.address, "w.csv"), row.names = FALSE)
  write.csv(h, paste(output.address, 'h.csv'), row.names = FALSE)
  
  return(decomp, w, h)
}

newDataProjection <- function (
  data,
  data.address,
  w,
  h) {
  print(c("Running newDataProjection on the following dataset: ", data.address))
  
  labels <- data[c("RID", "diagnosis"),] # Removing labels such that it is not involved in NMF
  metaboliteData <- data[-c("RID","diagnosis"),]
  
  data.rows <- row.names(metaboliteData)
  w.row.names <- row.names(w)
  
  overlap <- intersect(data.rows, w.row.names)
  
  print (c("Number of metabolites in original data: ", nrow(data)))
  print (c("Number of metabolites in W:", nrow(w)))
  print (c("Size of overlap:", length(overlap)))
  
  locations.w <- match(overlap, w.row.names, nomatch = 0)
  w2 <- w[locations.w, ]
  
  locations.m <- match(overlap, data.rows, nomatch = 0)
  m2 <- m[locations.m, ]
  
  print("Projecting using pseudo-inverse")
  H <- ginv(w2) %*% m2
  
  # Normalizing projected data
  n.col <- length(H[1,])
  for (i in 1:n.col) {
    S.2 <- sqrt(sum(H[,i]*H[,i]))
    H[,i] <- H[,i]/S.2
  }
  
  # Saving projected dataset 
  V <- data.frame(H)
  return(V)
  
}