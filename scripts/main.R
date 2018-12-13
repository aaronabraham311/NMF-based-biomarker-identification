# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Main script to start project

# Importing files into main script
data.cleaning.address <- "dataClean.R"
data.handling.address <- "normalize.R"
nmf.projection.address <- "project.R"
statistics.address <- "statistics.R"
ml.address <- ""
data.address <- "../data/raw/Biomarkers Consortium ADNI CSF Multiplex Raw Data.csv"
key.address <- "../data/raw/Biomarkers Consortium ADNI QC Multiplex data.csv"
data.output.address <- "../data/cleaned"

source(data.cleaning.address)
source(data.handling.address) 
source(nmf.projection.address)
source(statistics.address)
source(ml.address)

# Parameters for this run:
rowRemoval <- NA # Array of rows to remove. NA otherwise
columnRemoval <- c(2,162,163) # Array of columns to remove. NA otherwise

# Calling functions
cleanedDataObject <- cleanData(data.address,key.address, rowRemoval, columnRemoval, outputDirectory)
cleanedData <- cleanData[[1]]
rowNumber <- cleanData[[2]]
columnNumber <- cleanData[[3]]