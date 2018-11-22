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
  
  cleanData <- noNullandNA[!rowRemoval, !columnRemoval]
  
  noNullandNA <- eliminateNullandNA(raw, output.address)
  labelledData <- labelData(noNullandNA, key.address, output.address)
  
  write.csv(cleanData, output.address)
  return (c(cleanData, nrow(cleanData), ncol(cleanData)))
}

# Removes all rows with NA and null. Remove 0 and non-number symbols
eliminateNullandNA <- function (
  data,
  output.address) {
   nonZeroColumns <- apply(data, 2, function(col) all(col != 0)) # Creates list of columns that have non-zero columns
   noZero <- data[, nonZeroColumns] # Subsetting data
   
   noPeriods <- apply(noZero, 2, function(col) gsub(".", NA, col)) # Replaces period with NA 
   
   noNull <- na.omit(noPeriods)
   cleaned <- noNull
   
   print(c("Number of removed columns due to non-zero and non-numeric conditions: ", ncol(data) - ncol(cleaned))) # Outputting number of removed columns
   write.csv(cleaned, output.address)
   
   return(cleaned)
}

labelData <- function(
  data,
  key.address, 
  output.address) {
  
  # Downloading key data
  keyData <- read.csv(key.address)
  keyData <- keyData[,c("RID", "DXCURREN")] #RID serves as joiner, DXCURREN is diagnosis indicator. Possible extension: use columns that indicate diagnosis change
  colnames(keyData) <- c("rid", "diagnosis") # Renaming columns
  
  labelledData <- merge(data, keyData, by = "rid")
  write.csv(labelledData, output.address)
  
  return (labelledData)
}