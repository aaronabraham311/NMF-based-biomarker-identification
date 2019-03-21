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
explain.address <- "scripts/explainML.R"

source(data.cleaning.address)
source(data.handling.address) 
source(nmf.projection.address)
source(visualizations.address)
source(ml.address)
source(statistics.address)
source(explain.address)

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
nonScaleColumns <- c("RID", "diagnosis")
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
trainingProjectionsObj <- extractFactors(handledData, data.output.address, indices)
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
normalModels <- baseML(train, test, predictor = "diagnosis", paste(models.output.address, "fineTune/", sep = ''))

# NMF machine learning
nmfModels <- baseML(nmfTrain, nmfTest, predictor = "diagnosis", paste(models.output.address,"fineTune/", "nmf.", sep = ''))

# PCA machine learning
pcaModels <- baseML(pcaTrain, pcaTest, predictor = "diagnosis", paste(models.output.address, "fineTune/pca.", sep = ""))

# tSNE machine learning
tsneModels <- baseML(tsneTrain, tsneTest, predictor = "diagnosis", paste(models.output.address, "fineTune/tsne.", sep = ""))

normalRf <- readRDS(paste(models.output.address, "fineTune/rf .RDS", sep = ""))
normalKnn <- readRDS(paste(models.output.address, "knn .RDS", sep = ""))
normalXgb <- readRDS(paste(models.output.address, "xgbLinear .RDS"))
normalSvm <- readRDS(paste(models.output.address, "svmRadial .RDS"))
normalEnsembleModel <- readRDS(paste(models.output.address, "ensemble.RDS"))

nmfRf <- readRDS(paste(models.output.address, "nmf. rf .RDS"))
nmfKnn <- readRDS(paste(models.output.address, "nmf. knn .RDS"))
nmfXgb <- readRDS(paste(models.output.address, "nmf. xgbLinear .RDS"))
nmfSvm <- readRDS(paste(models.output.address, "nmf. svmRadial .RDS"))
nmfEnsembleModel <- readRDS(paste(models.output.address, "nmf. ensemble.RDS"))

pcaRf <- readRDS(paste(models.output.address, "pca. rf .RDS"))
pcaKnn <- readRDS(paste(models.output.address, "pca. Knn .RDS"))
pcaXgb <- readRDS(paste(models.output.address, "pca. xgbLinear .RDS"))
pcaSvm <- readRDS(paste(models.output.address, "pca. svmRadial .RDS"))
pcaEnsemble <- readRDS(paste(models.output.address, "pca. ensemble.RDS"))

# Creating model based on specific metabolites
metaboliteList <- c("RID", "Eotaxin.1..pg.mL.", "Brain.Natriuretic.Peptide...BNP...pg.ml.", "Pancreatic.Polypeptide..PPP...pg.ml.",
                   "Heparin.Binding.EGF.Like.Growth.Factor....pg.mL.", "Apolipoprotein.D..Apo.D...ug.ml.", "Vitronectin..ug.ml.",
                   "Vascular.Endothelial.Growth.Factor..VEGF..pg.mL.", "Fibrinogen..mg.mL.", "Cystatin.C..ng.ml." ,
                   "Vascular.Cell.Adhesion.Molecule.1..VCAM...ng.mL.", "Thrombopoietin..ng.mL.", "diagnosis")
metaboliteList_new <- c("RID", "Apolipoprotein.A.II..Apo.A.II...ng.ml.", "Macrophage.Derived.Chemokine..MDC...pg.mL.","Apolipoprotein.D..Apo.D...ug.ml.",
                    "Vascular.Endothelial.Growth.Factor..VEGF..pg.mL.", "Complement.Factor.H..ug.ml.", "Fibrinogen..mg.mL.",
                    "Monokine.Induced.by.Gamma.Interferon..MI..pg.ml.", "Serum.Glutamic.Oxaloacetic.Transaminase..ug.mL.","Cystatin.C..ng.ml.",
                    "Fetuin.A..ug.ml.", "Vascular.Cell.Adhesion.Molecule.1..VCAM...ng.mL.","Agouti.Related.Protein..AGRP...pg.mL.", "diagnosis")
specific_data <- handledData %>% dplyr::select(metaboliteList)

indices <- createDataPartition(specific_data$RID, p = 0.7, list = FALSE) # Setting 70/30 split between training and testing
train <- specific_data[indices,]
test <- specific_data[-indices,]

train$RID <- NULL
test$RID <- NULL

specific_models <- baseML(train, test, predictor = "diagnosis", paste(models.output.address, "fineTune/specific", sep = ''))


# Machine learning explainability
baseExplainFunction(nmfTrain, nmfTest, nmfRf, feature = "X1", labels, visualizations.output.address,
                    file = "nmf.rf.")
baseExplainFunction(nmfTrain, nmfTest, nmfKnn, feature = "X1", labels, visualizations.output.address,
                    file = "nmf.knn.")
baseExplainFunction(nmfTrain, nmfTest, nmfSvm, feature = "X1", labels, visualizations.output.address,
                    file = "nmf.svm.")

baseExplainFunction(pcaTrain, pcaTest, pcaRf, feature = "PC1", labels, visualizations.output.address,
                    file = "pca.rf.")