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
  
  # Default NMF 
  labels <- data[,c("rid", "diagnosis")] # Removing labels such that it is not involved in NMF
  metaboliteData <- subset(data, select = -c(rid, diagnosis))
  
  nmfReduced <- nmf(metaboliteData, k)
  w <- basis(nmfReduced) # convert to data table? 
  h <- coef(nmfReduced) # convert to data table?
  fullData <- cbind(h, labels)
  features <- extractFeatures(nmfReduced)
  
  # Ordering features
  dist.matrix <- dist(t(w))
  HC <- hclust(dist.matrix, method = "complete")
  w <- w[,HC$order]
  h<- h[HC$order,]
  
  # Writing to external file
  write.csv(w, paste(output.address, "w.csv"), row.names = FALSE)
  write.csv(h, paste(output.address, 'h.csv'), row.names = FALSE)
  
  
  return(nmfReduced, w, h, features)
}

newDataProjection <- function (
  data,
  data.address,
  w,
  h) {
  print(c("Running newDataProjection on the following dataset: ", data.address))
  
  labels <- data[c("rid", "diagnosis"),] # Removing labels such that it is not involved in NMF
  metaboliteData <- data[-c("rid","diagnosis"),]
  
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