# improvement by imputation

# libraries ----

library(tidyverse)
library(scales)
library(reshape2)
library(rstatix)
library(clValid)
library(mclust)
library(factoextra)
library(ggpubr)

# scripts ----

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R")
source("scripts/6.anova.R")
# initializing

set.seed(123)

table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
table1.1 <- anthroSex(table1.0)
table1.2 <- normalizingNumeric(table1.1)
table1.3 <- timingCleanFeatures(table1.2, "data/chronicPlasmAnt.csv")

# preprocessing ----

table1.3$Sex <- factor(table1.3$Sex)
table1.3$Sweetener <- factor(table1.3$Sweetener)
table1.3$Time <- factor(table1.3$Time)
table1.3.1 <- table1.3 %>% select (-c(numVol, grouping, Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
table1.3.2 <- cbind(table1.3.1, numVol = table1.3$numVol)
table1.4 <- factoringImputating(table1.3.1)
table1.4.1 <- cbind(table1.4, numVol = table1.3$numVol)
# setting up the tests

# ANOVA  ----

aov_loop(table1.3)

aov_loop(table1.4.1)

# Función para realizar la anova de tres vías sobre una variable
# Imprime por pantalla el resultado

aov_test <- function(tabla, variable){
  
  tablaVar <- tabla %>% select(numVol, Sweetener, Sex, Time, variable)
  
  # tablaVar <- tablaVar[!tablaVar[[5]] %in% boxplot.stats(tablaVar[[5]])$out,]
  
  res.aov <- anova_test(data = ungroup(tablaVar), dv=variable, wid=numVol, 
                        between = c(Sex, Sweetener), within= Time)
  
  tablaAnova <- get_anova_table(res.aov, correction = "auto")
  
  print(tablaAnova)
}

# Función para hacer en bucle el análisis anova a lo largo de una tabla

aov_loop <- function(tabla){
  
  message(paste("Tabla analizada: ", deparse(substitute(tabla))))
  for (i in colnames(tabla)[-1]){
    
    if (is.numeric(tabla[,i])){
      
      message(paste("Variable analizada: ", i))
      aov_test(tabla,i)
    }
    
    
  }
}
