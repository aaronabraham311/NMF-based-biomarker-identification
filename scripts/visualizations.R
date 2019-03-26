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
library(ROCR)
library(pROC)

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
  labels_colors(dend) <- 1:3
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
  
  plotObject <- ggplot(data, aes(x = model, y = mean, fill = reduction)) + 
    geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
    labs(title = title) + xlab("Models") + ylab("F1 Accuracy Score") + 
    scale_fill_discrete(name = "Technique") +
    geom_errorbar(aes(ymin=mean-(2 *std), ymax=mean+(2*std)), width=.2,
                  position=position_dodge(.9)) 
  
  png(filename = paste(output.address, "accuracy.png", sep = ""))
  plot(plotObject)
  dev.off()
}

rocCurves <- function(model1, model2, model3, model4, model5, data, indices, output.address, predictor,
                      model1Name, model2Name, model3Name, model4Name, model5Name)
{
  test <- data
  
  # Getting predictions
  model1Predictions <- predict(model1, test, type = "prob")
  model2Predictions <- predict(model2, test, type = "prob")
  model3Predictions <- predict(model3, test, type = "prob")
  model4Predictions <- predict(model4, test, type = "prob")
  
  # Making prediction dataset for ensemble model
  ensembleTest <- data.frame(model1Predictions, model3Predictions, model2Predictions,
                       model4Predictions, diagnosis = test$diagnosis)
  model5Predictions <- predict(model5, ensembleTest)
  
  # Making roc objects for each model
  rocModel1 <- multiclass.roc(test[,predictor], model1Predictions)
  rocModel2 <- multiclass.roc(test[,predictor], model2Predictions[,1])
  rocModel3 <- multiclass.roc(test[,predictor], model3Predictions[,1])
  rocModel4 <- multiclass.roc(test[,predictor], model4Predictions[,1])
  rocModel5 <- multiclass.roc(test[,predictor], model5Predictions[,1])
  
  # Creating ROC curve image and saving in directory
  png(filename = paste(output.address, "rocCurve.png", sep = ""))
  
  ggroc(list(model1Name = rocModel1, model2Name = rocModel2, model3Name = rocModel3,
             model4Name = rocModel4, model5Name = rocModel5)) +
    ggtitle("ROC Curves of All Models")
  
  dev.off()
  
  # Creating collection of all AUC scores
  aucScores <- c(auc(rocModel1),auc(rocModel2), auc(rocModel3), auc(rocModel4), auc(rocModel5))
  aucDataFrame <- data.frame(Models = c(model1Name, model2Name, model3Name, model4Name, model5Name), AUC = aucScores)
  
  
  # Creating bar plot of all AUC scores
  png (filename = paste(output.address, "aucScores.png", sep = ""))
  
  ggplot(data = aucDataFrame, aes(x = Models, y = AUC)) +
    geom_bar(fill = "steelblue") +
    geom_text(aes(label = AUC), vjust = 1.6, color = "white", size = 3.5) +
    theme_minimal()
  
  dev.off()
}