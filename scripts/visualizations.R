# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: January 1, 2019

# TO DO:
# Hierarchical clustering
# Violin plots of certain variables
# SHAP
# Heatmaps

# Libraries
library(dendextend)

hierarchicalClustering <- function(
  data,
  k,
  labels,
  title,
  file,
  output.address) {
  # Cluster via distance function
  dend <- data %>%
          dist %>%
          hclust(method = "complete") %>% 
          as.dendrogram # Note: method can be changed
  
  # Plotting and changing visuals
  labels(dend) <- data[,labels]
  plot <- dend %>%
          color_branches(k = 3)
  
  #Intializing file
  png(filename = paste(output.address, file))
  
  plot(plot, main = title)

  #Saving plot
  dev.off()
}