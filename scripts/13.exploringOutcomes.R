#### SCRIPT FOR EXPLORING 

# scripts ----

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R")

# impObject_2 <- readRDS("temp/impObject_chronicPlasmAnt.RDS")
# metrics_PlasmAnt <- readRDS("temp/metricsPRed_chronicPlasmAnt.RDS")
# metrics_PlasmFlav <- readRDS("temp/metricsPRed_chronicPlasmFlav.RDS")

outcomes_PF <- readRDS("temp/outcomePRed_chronicPlasmFlav.RDS")
outcomes_PA <- readRDS("temp/outcomePRed_chronicPlasmAnt.RDS")
outcomes_UF <- readRDS("temp/outcomePRed_chronicUrineFlav.RDS")
outcomes_UA <- readRDS("temp/outcomePRed_chronicUrineAnt.RDS")

metrics_PF <- readRDS("temp/metricsPRed_chronicPlasmFlav.RDS")
metrics_PA <- readRDS("temp/metricsPRed_chronicPlasmAnt.RDS")
metrics_UF <- readRDS("temp/metricsPRed_chronicUrineFlav.RDS")
metrics_UA <- readRDS("temp/metricsPRed_chronicUrineAnt.RDS")

ggpubr::ggarrange(metrics_PF$boxplot.class, metrics_PA$boxplot.class, metrics_UF$boxplot.class,metrics_UA$boxplot.class)
ggpubr::ggarrange(metrics_PF$boxplot.reg, metrics_PA$boxplot.reg, metrics_UF$boxplot.reg,metrics_UA$boxplot.reg)


writeResults(metrics_PF)
writeResults(metrics_PA)
writeResults(metrics_UF)
writeResults(metrics_UA)


metricsDataFrame <- function(outcomes.list){
  
  outcome_1 <- outcomes.list[[1]]
  outcome_2 <- outcomes.list[[2]]
  outcome_3 <- outcomes.list[[3]]
  
  df.outcomes <- data.frame(KNN = rbind(MICE = outcome_1[[1]]$results[rownames(outcome_1[[1]]$bestTune),
                                    c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                                    Soft = outcome_2[[1]]$results[rownames(outcome_2[[1]]$bestTune), 
                                                                  c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                                    Full = outcome_3[[1]]$results[rownames(outcome_3[[1]]$bestTune),
                                                                  c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")]),
             RF = rbind(MICE = outcome_1[[2]]$results[rownames(outcome_1[[2]]$bestTune), 
                                                    c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                      Soft = outcome_2[[2]]$results[rownames(outcome_2[[2]]$bestTune),
                                                    c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                      Full =  outcome_3[[2]]$results[rownames(outcome_3[[2]]$bestTune),
                                                     c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")]),
             XGBoost = rbind(MICE = outcome_1[[3]]$results[rownames(outcome_1[[3]]$bestTune),
                                                         c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                           Soft = outcome_2[[3]]$results[rownames(outcome_2[[3]]$bestTune),
                                                         c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                           Full =  outcome_3[[3]]$results[rownames(outcome_3[[3]]$bestTune),
                                                          c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")]),
             NNet = rbind(MICE = outcome_1[[4]]$results[rownames(outcome_1[[4]]$bestTune),
                                                      c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                        Soft = outcome_2[[4]]$results[rownames(outcome_2[[4]]$bestTune),
                                                      c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")],
                        Full = outcome_3[[4]]$results[rownames(outcome_3[[4]]$bestTune),
                                                      c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD")]))
  data.table::fwrite(x = df.outcomes, file = paste0("temp/df_", deparse(substitute(outcomes.list)), ".csv"), row.names = T)
  return(df.outcomes)
}

df_outcomes_PF <- metricsDataFrame(outcomes_PF)
df_outcomes_PA <- metricsDataFrame(outcomes_PA)
df_outcomes_UF <- metricsDataFrame(outcomes_UF)
df_outcomes_UA <- metricsDataFrame(outcomes_UA)



