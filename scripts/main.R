# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Main script to start project

# Libraries
library(dplyr)
library(caret)

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

# Parameters for this run:
#rowRemoval <- "" # Array of rows to remove. NA otherwise
columnRemoval <- c(1,3,4,5) # Array of columns to remove. NA otherwise
ceiling <- 100000
threshold <- 0
nonScaleColumns <- c("rid", "diagnosis")
k <- 3
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

# Projection creation
trainingProjectionsObj <- extractFactors(handledData, data.output.address, indices, k)
w <- trainingProjectionsObj$w
h <- trainingProjectionsObj$h

# NMF train and test dataset
nmfTrain <- trainingProjectionsObj$train
nmfTest <- trainingProjectionsObj$test

# Hierarchical clustering
hierarchicalClustering(t(h), k, labels, title = "NMF Clustering", file = "nmfcluster", visualizations.output.address)

# Traditional machine learning
baseML(train, test, predictor = "diagnosis", models.output.address)