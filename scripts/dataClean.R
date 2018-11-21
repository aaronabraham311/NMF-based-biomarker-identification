# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Cleaning all data into usable form

cleanData <- function (
  data.address,
  key.address,
  rowRemoval,
  columnRemoval, 
  output.address){
  raw <- read.csv(data.address);
  
  noNullandNA <- eliminateNullandNA(raw, output.address)
  labelledData <- labelData(noNullandNA, key.address, output.address)
  
  cleanData <- noNullandNA[!rowRemoval, !columnRemoval]
  write.csv(cleanData, output.address)
  return (c(cleanData, nrow(cleanData), ncol(cleanData)))
}

# Removes all rows with NA and null. Remove 0"
eliminateNullandNA <- function (
  data) {
   noNull <- na.omit(data)
   
   nonZeroColumns <- apply(noNull, 2, function(col) all(col != 0)) # Creates list of columns that have non-zero columns
   noZero <- noNull[, nonZeroColumns] # Subsetting data
   print(c("Number of removed columns due to non-zero conditions: ", ncol(data) - length(nonZeroColumns))) # Outputting number of removed columns
   
   return(noZero)
}

labelData <- function(
  data,
  key.address) {
  
  # Downloading key data
  keyData <- read.csv(key.address)
  keyData <- keyData[,c("RID", "DXCURREN")] #RID serves as joiner, DXCURREN is diagnosis indicator. Possible extension: use columns that indicate diagnosis change
  colnames(keyData) <- c("rid", "diagnosis") # Renaming columns
  
  labelledData <- merge(data, keyData, by = "rid")
  
  return (labelledData)
}