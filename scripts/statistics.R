# NMF-based machine learning to identify biomarkers for Alzheimer's disease diagnosis
# Author: Aaron Abraham
# Date: November 13, 2018

# REPLACE MANN WHITNEY WITH TUKEY ANALYSIS

statisticsMain <- function(metaboliteList, data, output.address) {
  print (c("Running ANOVA and Mann-Whitney tests to be outputted at: ", output.address))
  
  data$diagnosis <- as.factor(data$diagnosis)
  
  ad_control_data <- data %>% filter(diagnosis != 2)
  ad_mci_data <- data %>% filter(diagnosis != 1)
  mci_control_data <- data %>% filter(diagnosis != 3)
  
  # Writing date into output filename
  date.string <- date()
  date.string2 <- paste(unlist(strsplit(date.string, " ")), sep="_", collapse="_")
  date.string3 <- paste(unlist(strsplit(date.string2, ":")), sep="_", collapse="_")
  params.file <- paste(output.address, date.string3, ".statistics.txt", sep="")
  
  write(c("Statistics on ", date.string2), file= params.file, ncolumns=100, append=F)
  write(c("  "), file= params.file, ncolumns=100, append=T)
  
  for(i in metaboliteList) {
    pANOVA <- metaboliteANOVA(i, data)
    pMannWhitney_AD_Control <- metaboliteMW(i, data, 2)
    pMannWhitney_AD_MCI <- metaboliteMW(i, data, 1)
    pMannWhitney_MCI_Conrtol <- metaboliteMW(i, data, 3)
    
    write.table(c(i, " ANOVA Result: ", pANOVA), file = params.file, append = T)
    write.table(c(i, " MW AD and Control Result: ", pMannWhitney_AD_Control), file = params.file, append = T)
    write.table(c(i, " MW AD and MCI Result: ", pMannWhitney_AD_MCI), file = params.file, append = T)
    write.table(c(i, " MW MCI and Control Result: ", pMannWhitney_MCI_Conrtol), file = params.file, append = T)
  }
}

metaboliteANOVA <- function (metabolite, data) {
  metaboliteData <- data %>% dplyr::select(metabolite, diagnosis)
  metaboliteData$diagnosis <- as.factor(metaboliteData$diagnosis)
  metaboliteData <- metaboliteData[order(metaboliteData$diagnosis), ]
  
  colnames(metaboliteData) <- c("metabolite", "diagnosis")
  
  anv <- aov(metabolite ~ diagnosis, data = metaboliteData)
  pVal <- summary(anv)[[1]][["Pr(>F)"]]
  
  return(pVal[1])
}

metaboliteTukey <- function (anv, data) {
  tukeyModel <- TukeyHSD(anv, which = "diagnosis", conf.level = 0.95)
  result <- tukeyModel$diagnosis
  
}

metaboliteMW <- function (metabolite, data, noDiagnosisLevel) {
  metaboliteData <- data %>% dplyr::select(metabolite, diagnosis)
  
  mwData <- metaboliteData %>% filter(diagnosis != noDiagnosisLevel)
  mwData$diagnosis <- as.factor(mwData$diagnosis)
  
  mannWhitney_results <- wilcox.test(mwData[,metabolite] ~ mwData$diagnosis, data = mwData)
  pVal <- mannWhitney_results$p.value
  
  return(pVal)
}
