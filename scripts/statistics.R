# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# Statistics to do:
# 1. Perform ANOVA
# 2. Perform t-test for Control vs. AD, MCI vs. AD and Control vs AD
# 3. Visualize top metabolites using violin plots

statisticsMain <- function(metaboliteList, data, output.address) {
  print (c("Running ANOVA and Mann-Whitney tests to be outputted at: ", output.address))
  
  ad_control_data <- data %>% select(diagnosis == "1" && diagnosis == "3")
  ad_mci_data <- data %>% select(diagnosis == "2" && diagnosis == "3")
  mci_control_data <- data %>% select (diagnosis == "1" && diagnosis == "2")
  
  # Writing date into output filename
  date.string <- date()
  date.string2 <- paste(unlist(strsplit(date.string, " ")), sep="_", collapse="_")
  date.string3 <- paste(unlist(strsplit(date.string2, ":")), sep="_", collapse="_")
  params.file <- paste(model.address, date.string3, ".statistics.txt", sep="")
  
  write(c("Statistics on ", date.string2), file= params.file, ncolumns=100, append=F)
  write(c("  "), file= params.file, ncolumns=100, append=T)
  
  for(i in metaboliteList) {
    pANOVA <- metaboliteANOVA(i, data)
    pMannWhitney_AD_Control <- metaboliteMW(i, ad_control_data)
    pMannWhitney_AD_MCI <- metaboliteMW(i, ad_mci_data)
    pMannWhitney_MCI_Conrtol <- metaboliteMW(i, mci_control_data)
    
    write.table(c(i, " ANOVA Result: ", pANOVA), file = params.file, append = T)
    write.table(c(i, " MW AD and Control Result: ", pMannWhitney_AD_Control), file = params.file, append = T)
    write.table(c(i, " MW AD and MCI Result: ", pMannWhitney_AD_MCI), file = params.file, append = T)
    write.table(c(i, " MW MCI and Control Result: ", pMannWhitney_MCI_Conrtol), file = params.file, append = T)
  }
}

metaboliteANOVA <- function (metabolite, data) {
  metaboliteData <- data %>% dplyr::select(metabolite, diagnosis)
  metaboliteData <- metaboliteData %>% group_by(diagnosis)
  
  anova_results <- aov(diagnosis ~ metabolite, data = metaboliteData)
  pVal <- summary(anova_results)[[1]][["Pr(>F)"]]
  
  return(pVal)
}

metaboliteMW <- function (metabolite, data) {
  mannWhitney_results <- wilcox.test(diagnosis ~ metabolite, data = data)
  pVal <- mannWhitney_results$p.value
  
  return(pVal)
}
