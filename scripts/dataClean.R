# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Cleaning all data into usable form

cleanData <- function (
  data.address,
  rowNumber,
  columnNumber,
  key.address){
  raw <- read.csv(data.address);
  
  noNullandNA <- eliminateNullandNA(raw)
  cleanData <- noNullandNA[!rowNumber, !columnData]
  return (c(cleanData, nrow(cleanData), ncol(cleanData)))
}

# Removes all rows with NA and null. Remove 0"
eliminateNullandNA <- function (
  data) {
   noNull <- na.omit(data)
   
   nonZeroRows <- apply(noNull, 1, function(row) all(row != 0))
   noZero <- noNull[nonZeroRows, ]
   
   return(noZero)
}