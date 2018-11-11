# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 11, 2018

# Cleaning all data into usable form

# TO DO:
# Eliminate NA or NULL
# Remove extraneous columns/rows (identifiers)
# Output total number of rows and columns

cleanData <- function (
  data.address ){
  raw <- read.csv(data.address);
  
  noNullandNA <- eliminateNullandNA(raw)
}

# Removes all rows with NA and null. Remove 0"
eliminateNullandNA <- function (
  data) {
   noNullandNA <- na.omit(data)
   return(noNullandNA)
}