# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: January 1, 2019

# TO DO:
# Hierarchical clustering
# Violin plots of certain variables
# PCA and LDA visualizations: https://gist.github.com/thigm85/8424654
# SHAP
# Heatmaps

# Libraries
library(dendextend)
library(reshape2)

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

metaboliteCorrelation <- function (data, metaboliteList, title, output.address)
{
  data <- data[,metaboliteList]
  data$RID <- NULL
  data$diagnosis <- NULL
  cormat <- round(cor(data),2) #Correlation matrix
  
  get_lower_tri <- function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return (cormat)
  }
  
  get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return (cormat)
  }
  
  png(filename = paste(output.address,metabolite,".png", sep = ""))
  
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() +
    ggtitle(title)
  
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

metaboliteViolinPlot <- function(data, metabolite, title, output.address)
{
  #Intializing file
  png(filename = paste(output.address,title,".png", sep = ""))
  
  ggplot(data, aes(x = diagnosis, y = metabolite, fill = diagnosis)) + geom_violin(draw_quantiles = TRUE) + 
    geom_boxplot(width = 0.1) +
    labs(title = title)
  
  dev.off()
}

metaboliteDensityPlot <- function(data, metabolite, title, output.address)
{
  density_data <- data %>% dplyr::select("diagnosis", metabolite)
  density_data[,"diagnosis"] <- as.factor(density_data[,"diagnosis"])
  density_data[,"Condition"] <- density_data[,"diagnosis"]
  ggplot(density_data, aes(x = metabolite, color = Condition)) + geom_density(alpha = 0.4) +
    xlab(NULL) + ylab(NULL)
}

modelAccuracyBarplots <- function(data, title, output.address)
{
  plotObject <- ggplot(data, aes(x = models, y = accuracy, fill = decomp)) + 
    geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
    labs(title = title) + xlab("Models") + ylab("Accuracy") + 
    scale_fill_discrete(name = "Technique")
  
  png(filename = paste(output.address, "accuracy.png", sep = ""))
  plot(plotObject)
  dev.off()
}