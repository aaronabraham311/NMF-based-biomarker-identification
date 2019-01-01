# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: January 1, 2019

# TO DO:
# Hierarchical clustering
# Violin plots of certain variables
# SHAP
# Heatmaps

# Libraries


hierarchicalClustering <- function(
  data,
  k,
  labels,
  title,
  file,
  output.address) {
  # Cluster via distance function
  cluster <- hclust(dist(data)) # Note: method can be changed
  
  # Plotting function
  hclust <- plot(cluster, labels = data[,labels], main = title)
  
  #Saving plot
  png(filename = paste(output.address, file))
  dev.off()
}