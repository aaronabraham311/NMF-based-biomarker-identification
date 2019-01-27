# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# Statistics to do:
# 2. Perform ANOVA
# 3. Visualize top metabolites using violin plots

metaboliteANOVA <- function (metabolite, data) {
  metaboliteData <- data %>% dplyr::select(metabolite, diagnosis)
  metaboliteData <- metaboliteData %>% group_by(diagnosis)
  
  anova_results <- aov(diagnosis ~ metabolite, data = metaboliteData)
  pVal <- summary(anova_results)[[1]][["Pr(>F)"]]
}