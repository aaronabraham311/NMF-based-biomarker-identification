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
  
  removedRowsAndCols <- subset(raw, select = -c(columnRemoval))
  
  noNullandNA <- eliminateNullandNA(removedRowsAndCols)
  
  # Removing duplicates
  uniqueNoNullandNA <- noNullandNA %>% group_by(RID) %>% mutate_all(funs(mean), -1) %>% distinct
  
  labelledData <- labelData(uniqueNoNullandNA, key.address, output.address)
  
  write.csv(labelledData, paste(output.address, "cleaned.csv"), sep = "", row.names = FALSE)
  
  returnValues <- list("cleanedData" = labelledData, "numRows" = nrow(labelledData), "numCols" = ncol(labelledData))
  return (returnValues)
}

# Removes all rows with NA and null. Remove 0 and non-number symbols
eliminateNullandNA <- function (
  data) {
   noZero <- data[,apply(data, 2, function(col) !all(col == 0))]
   noFactors <- noZero[, !sapply(data, is.factor)]
   
   # Remove NA
   noNull <- na.omit(noFactors)
   cleaned <- noNull
   cleaned <- cleaned[complete.cases(cleaned),]
   
   
   print(c("Number of removed columns due to non-zero and non-numeric conditions: ", ncol(data) - ncol(cleaned))) # Outputting number of removed columns
   print(c("Number of removed rows due to non-zero and non-numeric conditions:", nrow(data) - nrow(cleaned))) # Outputting number of removed rows
   return(cleaned)
}

labelData <- function(
  data,
  key.address, 
  output.address) {
  
  # Downloading key data
  keyData <- read.csv(key.address)
  keyData <- keyData[c("RID", "DXCURREN")] #RID serves as joiner, DXCURREN is diagnosis indicator. Possible extension: use columns that indicate diagnosis change
  colnames(keyData) <- c("RID", "diagnosis") # Renaming columns
  
  # Removing duplicate diagnoses:
  keyData <- eliminateNullandNA(keyData)
  keyData <- keyData[!duplicated(keyData$RID),]
  
  labelledData <- merge(data, keyData, by = "RID")
  
  return (labelledData)
}