# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Main script to start project

# Libraries
library(dplyr)
library(ggplot2)

set.seed(123)

# Importing files into main script
data.cleaning.address <- "scripts/dataClean.R"
data.handling.address <- "scripts/handle.R"
nmf.projection.address <- "scripts/project.R"
statistics.address <- "scripts/statistics.R"
visualizations.address <- "scripts/visualizations.R"
ml.address <- "scripts/ml.R"

source(data.cleaning.address)
source(data.handling.address) 
source(nmf.projection.address)
source(visualizations.address)
source(ml.address)
source(statistics.address)

# Addresses for important sub-directories
data.address <- "./data/raw/adni_plasma_qc_multiplex_11Nov2010.csv"
key.address <- "./data/raw/DXSUM_PDXCONV_ADNIALL.csv"
data.output.address <- "./data/cleaned/"
visualizations.output.address <- "./visuals/"
models.output.address <- "./models/"
stats.output.address <- "./statistics/"
boxplot.output.address <- "./visuals/boxplots/"

# Parameters for this run:
#rowRemoval <- "" # Array of rows to remove. NA otherwise
columnRemoval <- c(1,3,4,5) # Array of columns to remove. NA otherwise
ceiling <- 100000
threshold <- 0
nonScaleColumns <- c("rid", "diagnosis")
k <- 5
labels <- "diagnosis"

## Calling functions

# Cleaning data
cleanedDataObject <- cleanData(data.address, key.address, columnRemoval, data.output.address)
cleanedData <- cleanedDataObject$cleanedData
rowNumber <- cleanedDataObject$numRows
columnNumber <- cleanedDataObject$numCols

# Handling data 
handledData <- handle(cleanedData, ceiling, threshold, nonScaleColumns, data.output.address)

# Splitting into training and testing sets
indices <- createDataPartition(handledData$RID, p = 0.7, list = FALSE) # Setting 70/30 split between training and testing
train <- handledData[indices,]
test <- handledData[-indices,]

# NMF Projection creation
trainingProjectionsObj <- extractFactors(handledData, data.output.address, indices, k)
w <- trainingProjectionsObj$w
h <- trainingProjectionsObj$h

# PCA Projection creation
pcaProjectionObj <- pcaProject(handledData, paste(data.output.address, "pca."), indices, k)

# tSNE projection creation
tsneProjectionObj <- tSNEProject(handledData, paste(data.output.address,"tsne."), indices, k)

# Most important metabolites and statistics
importantNMF <- importantMetabolites(w)
#statisticsMain(importantNMF, handledData, stats.output.address)

# NMF train and test dataset
nmfTrain <- trainingProjectionsObj$train
nmfTest <- trainingProjectionsObj$test

# PCA train and test dataset
pcaTrain <- pcaProjectionObj$trans_train
pcaTest <- pcaProjectionObj$trans_test

# tSNE train and test dataset
tsneTrain <- tsneProjectionObj$train
tsneTest <- tsneProjectionObj$test

# Hierarchical clustering
hierarchicalClustering(t(h), k, labels, title = "NMF Clustering", file = "nmfcluster", visualizations.output.address)

# Traditional machine learning
traditionalModels <- baseML(train, test, predictor = "diagnosis", models.output.address)

# NMF machine learning
baseML(nmfTrain, nmfTest, predictor = "diagnosis", paste(models.output.address, "nmf."))

# PCA machine learning
baseML(pcaTrain, pcaTest, predictor = "diagnosis", paste(models.output.address, "pca."))

# tSNE machine learning
baseML(tsneTrain, tsneTest, predictor = "diagnosis", paste(models.output.address, "tsne."))

normalRf <- readRDS(paste(models.output.address, "rf .RDS"))
normalKnn <- readRDS(paste(models.output.address, "knn .RDS"))
normalXgb <- readRDS(paste(models.output.address, "xgbLinear .RDS"))
normalSvm <- readRDS(paste(models.output.address, "svmRadial .RDS"))
normalEnsembleModel <- readRDS(paste(models.output.address, "ensemble.RDS"))

nmfRf <- readRDS(paste(models.output.address, "nmf. rf .RDS"))
nmfKnn <- readRDS(paste(models.output.address, "nmf. knn .RDS"))
nmfXgb <- readRDS(paste(models.output.address, "nmf. xgbLinear .RDS"))
nmfSvm <- readRDS(paste(models.output.address, "nmf. svmRadial .RDS"))
nmfEnsembleModel <- readRDS(paste(models.output.address, "nmf. ensemble.RDS"))
