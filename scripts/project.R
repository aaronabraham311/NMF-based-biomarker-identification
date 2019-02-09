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
library("Rtsne")

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

pcaProject <- function(data, output.address, indices, k)
{
  print(c("Running pcaProject function on "), output.address)
  
  # Transpose of matrix for factorization
  data <- data.matrix(data)
  row.names(data) <- data[,"RID"]
  labels <- data[,c("RID", "diagnosis")] # Removing labels such that it is not involved in PCA
  metaboliteData <- subset(data, select = -c(RID, diagnosis))
  
  # Removing columns with constant variance
  metaboliteData <- metaboliteData[,apply(metaboliteData, 2, var) != 0]
  
  pca.train <- metaboliteData[indices,]
  pca.test <- metaboliteData[-indices,]
  
  pca_comp <- prcomp(pca.train, scale. = TRUE) 
  components <- pca_comp$rotation #Getting number of PVA components
  
  # Statistics
  std_dev <- pca_comp$sdev
  variance <- std_dev^2
  prop_var_exp <- variance/(sum(variance))
  
  # Transforming train and test data into principal components and adding diagnosis back in
  pca.transformed.train <- pca_comp$x
  pca.transformed.train <- rbind(pca.transformed.train, labels[indices,"diagnosis"])
  pca.transformed.train <- data.frame(pca.transformed.train)
  row.names(pca.transformed.train)[nrow(pca.transformed.train)] <- "diagnosis"
  
  pca.transformed.test <- scale(pca.test, pca_comp$center, pca_comp$scale) %*% pca_comp$rotation
  pca.transformed.test <- rbind(pca.transformed.test, labels[-indices, "diagnosis"])
  pca.transformed.test <- data.frame(pca.transformed.test)
  row.names(pca.transformed.test)[nrow(pca.transformed.test)] <- "diagnosis"
  
  returnValues <- list("pca_model" = pca_comp, "components" = components, 
                       "variance_explained" = prop_var_exp, "trans_train" = pca.transformed.train,
                       "trans_test" = pca.transformed.test)
  return(returnValues)
  
  # Continue from here: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
  
}

tSNEProject <- function(data, output.address, indices, k)
{
  
}

importantMetabolites <- function (w, k) #ncol represents number of meta-metabolites
{
  metaMetabolite <- w
  metaMetabolite[,"metabolites"] <- rownames(w)
  
  ##Extracting 25 most important metabolites for each metagene
  meta1 <- subset(metaMetabolite, select = c(X1, metabolites))
  meta1 <- meta1[order(meta1$X1, decreasing = T),]
  importantMetabolites1 <- meta1[1:25,"metabolites"] 
  
  meta2 <- subset(metaMetabolite, select = c(X2, metabolites))
  meta2 <- meta2[order(meta2$X2, decreasing = T), ]
  importantMetabolites2 <- meta2[1:25, "metabolites"]
  
  meta3 <- subset(metaMetabolite, select = c(X3, metabolites))
  meta3 <- meta3[order(meta3$X3, decreasing = T), ]
  importantMetabolites3 <- meta3[1:25, "metabolites"]
  
  meta4 <- subset(metaMetabolite, select = c(X4, metabolites))
  meta4 <- meta4[order(meta4$X4, decreasing = T), ]
  importantMetabolites4 <- meta4[1:25, "metabolites"]
  
  meta5 <- subset(metaMetabolite, select = c(X5, metabolites))
  meta5 <- meta5[order(meta5$X5, decreasing = T), ]
  importantMetabolites5 <- meta5[1:25, "metabolites"]

  # Finding all unique metabolites
  importantList <- do.call(c, list( importantMetabolites1, importantMetabolites2, importantMetabolites3,
                           importantMetabolites4, importantMetabolites5))
  importantList <- unique(importantList)
  
  return(importantList)
}