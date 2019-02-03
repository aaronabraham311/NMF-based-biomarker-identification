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

metaboliteViolinplot <- function (data, metabolite, title, output.address)
{
  data$diagnosis <- as.factor(data$diagnosis)
  levels(data$diagnosis) <- c("Control", "MCI", "AD")
  ggplot(data, aes(x = diagnosis, y = metabolite, fill = diagnosis)) +
    geom_violin(draw_quantiles = TRUE) +
    geom_boxplot(width = 0.1) + 
    labs(title = c(metabolite, " concentrations among AD, MCI, and Control"))
}