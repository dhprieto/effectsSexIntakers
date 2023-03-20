### SCRIPT FOR WORKING ### 

## made w/o removing bad vars... check it 


##### Package names
# 
# packages <- c("tidyverse", "scales", "reshape2", "caret")
# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))

# load scripts ----

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R")
source("scripts/10.testingImputation.R")

# functions ----

mainPredict <- function(pathToTable, pathTableName, predictorReg){
  
  message(paste("##### Starting analysis of ", sub(".csv",sub("data/","",pathTableName)), " table #####"))
  
  
  set.seed(123)
  
  table1.0 <- readingFillingGrouping(pathToTable)
  table1.1 <- anthroSex(table1.0)
  table1.2 <- normalizingNumeric(table1.1)
  table1.3 <- timingCleanFeatures(table1.2, pathTableName)
  
  # prepoccesing 
  
  table1.3$Sex <- factor(table1.3$Sex)
  table1.3$Sweetener <- factor(table1.3$Sweetener)
  table1.3$Time <- factor(table1.3$Time)
  table1.3 <- table1.3 %>% select (-c(numVol, grouping, Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
  
  # imputation 
  
  table1.3_nona <- na.omit(table1.3)
  impObject <- factoringImputating(table1.3)
  table1.4_mice <- impObject[[2]]
  table1.4_srandom <- softRandomFilling(table1.3)
  table1.4_frandom <- fullRandomFilling(table1.3)
  
  message(paste("End of preprocessing and imputation"))
  message(paste("##### Starting prediction of ", sub(".csv",sub("data/","",pathTableName)), " table #####"))
  
  message("##### MICE imputation #####")
  
  
  outcome_mice <- predictionTest(table1.4_mice, "Sex", predictorReg)
  
  message("##### Full Random imputation #####")
  
  
  outcome_frand <- predictionTest(table1.4_frandom, "Sex", predictorReg)
  
  message("##### Soft Random imputation #####")
  
  
  outcome_srand <- predictionTest(table1.4_srandom, "Sex", predictorReg)
  
  message("##### NoNA procedure #####")
  
  
  outcome_nona <- predictionTest(table1.3_nona, "Sex", predictorReg)
  
  message("##### Saving results #####")
  
  
  outcomePred <- list(outcome_mice,
                      outcome_frand,
                      outcome_srand,
                      outcome_nona)
  
  metricsPred <- metricsOutcome(outcome_mice, outcome_f = outcome_frand, outcome_s = outcome_srand)
  
  saveRDS(impObject, file = paste0("temp/impObject_", sub(".csv","", sub("temp/","",pathTableName)),".RDS"))
  saveRDS(outcomePred, file = paste0("temp/outocomePRed_", sub(".csv","", sub("temp/","",pathTableName)),".RDS"))
  saveRDS(metricsPred, file = paste0("temp/metricsPRed_", sub(".csv","", sub("temp/","",pathTableName)),".RDS"))
  
}



# running -----

mainPredict("data/chronicPlasmAnt.csv", "data/chronicPlasmAnt.csv", "DHPAA")
mainPredict("data/chronicPlasmFlav.csv", "data/chronicPlasmFlav.csv", "HE.G")
mainPredict("data/chronicUrineFlav.csv", "data/chronicUrineFlav.csv", "HE")
mainPredict("data/chronicUrineAnt.csv", "data/chronicUrineAnt.csv", "CA.G")



