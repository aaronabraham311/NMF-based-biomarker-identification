# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Main script to start project

# Importing files into main script
data.cleaning.address <- "scripts/dataClean.R"
data.handling.address <- "scripts/handle.R"
nmf.projection.address <- "scripts/project.R"
statistics.address <- "scripts/statistics.R"
ml.address <- ""
data.address <- "./data/raw/Biomarkers Consortium ADNI CSF QC Multiplex data.csv"
key.address <- "./data/raw/DXSUM_PDXCONV_ADNIALL.csv"
data.output.address <- "./data/cleaned/"

source(data.cleaning.address)
source(data.handling.address) 
source(nmf.projection.address)
source(statistics.address)
#source(ml.address)

# Parameters for this run:
#rowRemoval <- "" # Array of rows to remove. NA otherwise
columnRemoval <- c(2,162,163) # Array of columns to remove. NA otherwise
ceiling <- 1000
threshold <- -1000
nonScaleColumns <- c("rid", "diagnosis")

## Calling functions

# Cleaning data
cleanedDataObject <- cleanData(data.address, key.address, columnRemoval, data.output.address)
cleanedData <- cleanedDataObject$cleanedData
rowNumber <- cleanedDataObject$numRows
columnNumber <- cleanedDataObject$numCols

# Handling data 
handledData <- handle(cleanedData, ceiling, threshold, nonScaleColumns, data.output.address)
