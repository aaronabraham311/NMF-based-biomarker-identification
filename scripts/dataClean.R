# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Cleaning all data into usable form

cleanData <- function (
  data.address,
  key.address,
  columnRemoval, 
  output.address){
  raw <- read.csv(data.address);
  
  print(c("Running cleaning function on: ", data.address))
  
  removedRowsAndCols <- raw[-c(columnRemoval)]
  
  noNullandNA <- eliminateNullandNA(removedRowsAndCols)
  labelledData <- labelData(noNullandNA, key.address, output.address)
  
  write.csv(cleanData, paste(output.address, "cleaned.csv"))
  return (c(cleanData, nrow(cleanData), ncol(cleanData)))
}

# Removes all rows with NA and null. Remove 0 and non-number symbols
eliminateNullandNA <- function (
  data) {
   noZero <- data[,apply(data, 2, function(col) !all(col == 0))]
   noFactors <- noZero[, !sapply(data, is.factor)]
   
   noNull <- na.omit(noFactors)
   cleaned <- noNull
   
   print(c("Number of removed columns due to non-zero and non-numeric conditions: ", ncol(data) - ncol(cleaned))) # Outputting number of removed columns
   return(cleaned)
}

labelData <- function(
  data,
  key.address, 
  output.address) {
  
  # Downloading key data
  keyData <- read.csv(key.address)
  keyData <- keyData[c("RID", "DXCURREN")] #RID serves as joiner, DXCURREN is diagnosis indicator. Possible extension: use columns that indicate diagnosis change
  colnames(keyData) <- c("rid", "diagnosis") # Renaming columns
  
  labelledData <- merge(data, keyData, by = "rid")
  
  return (labelledData)
}