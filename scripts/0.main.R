### MAIN SCRIPT ----

# libraries

library(tidyverse)
library(scales)
library(reshape2)
library(rstatix)
library(clValid)
library(mclust)
library(factoextra)
library(ggpubr)

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R") #timingCleanFeatures
source("scripts/6.anova.R") #timingCleanFeatures
source("scripts/7.boxplot.R") #timingCleanFeatures
source("scripts/8.pairwisettest.R") #timingCleanFeatures
source("scripts/9.clustering.R") #timingCleanFeatures




# test -----

mainPreproc <- function(pathToTable, anthro = T){

              table1.0 <- readingFillingGrouping(pathToTable)
              table1.1 <- anthroSex(table1.0)
              table1.2 <- normalizingNumeric(table1.1)
              table1.3 <- timingCleanFeatures(table1.2, pathToTable)
              table1.4 <- factoringImputating(table1.3)

              if (anthro == FALSE){
                table1.4 <- table1.4 %>% select (-c(Weight,BMI,Fat, CVRI,Bpmin, Bpmax, Frec))
                
              }
          return (table1.4)
}


anovaTests <- function(tabla) {
  
    aov_loop(tabla)
    pairwiseTTest(tabla)
  
  
}


clusteringAnalysis <- function(tabla){
  
  
}

# test ----
# plasmAnt <- mainPreproc("data/chronicPlasmAnt.csv", FALSE)
# plasmFlav <- mainPreproc("data/chronicPlasmFlav.csv" , FALSE)
# urineAnt <- mainPreproc("data/chronicUrineAnt.csv", FALSE)
# urineFlav <- mainPreproc("data/chronicUrineFlav.csv", FALSE)
# 
# 
# table0.1 <- read.csv("data/chronicUrineAnt.csv", sep=";",dec="." )
# 
# table1.0 <- readingFillingGrouping("data/chronicUrineAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# table1.4 <- timingCleanFeatures(table1.3)



