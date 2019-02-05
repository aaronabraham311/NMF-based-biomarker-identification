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
          color_labels(k = 3)
  
  #Intializing file
  png(filename = paste(output.address, file))
  
  plot(plot, main = title)

  #Saving plot
  dev.off()
}

metaboliteBoxplot <- function (data, metabolite, title, output.address)
{
  #Initalizing file 
  png(filename = paste(output.address, metabolite))
  
  data$diagnosis <- as.factor(data$diagnosis)
  levels(data$diagnosis) <- c("Control", "MCI", "AD")
  boxplot(data[,metabolite] ~ data$diagnosis, data = data, main = title, 
                   col = c("lightgreen", "lightblue", "red3"),
                   ylab = metabolite)
  
  # Saving plot
  dev.off()
}