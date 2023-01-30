# testing imputation

# libraries

library(tidyverse)
library(scales)
library(reshape2)
library(rstatix)
library(clValid)
library(mclust)
library(factoextra)
library(ggpubr)
library(randomForest)
library(e1071)
library(h2o)

# scripts

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R")

# initializing

table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
table1.1 <- anthroSex(table1.0)
table1.2 <- normalizingNumeric(table1.1)
table1.3 <- timingCleanFeatures(table1.2, "data/chronicPlasmAnt.csv")

# test for random filling ----

randomFilling <- function(tabla){
  
  for (var in names(tabla)){
  cat(var,"\n") # Print the variable name for debug purposes
  flush.console()
  
  if (class(tabla[[var]]) == "numeric"){
    print(length(tabla[is.na(tabla[[var]]), var]))
    # random number into the var variance
  
    for (i in rev(seq(1, length(tabla[is.na(tabla[[var]]), var])))){
      
      tabla[is.na(tabla[[var]]), var][i] <- runif(1,
                                             min= min(na.omit(tabla[[var]])), 
                                             max= max(na.omit(tabla[[var]])))
    
    
    }
    
    }
  
  
  } 
  
  return(tabla)
}


table1.4_random <- randomFilling(table1.3)




# pre-imputating test


table1.3$Sex <- factor(table1.3$Sex)
table1.3$Sweetener <- factor(table1.3$Sweetener)
table1.3$Time <- factor(table1.3$Time)
table1.3 <- table1.3 %>% select (-c(numVol, grouping, Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))


# rf

rf_noimp <- randomForest(Sex ~., data = table1.3, na.action = na.omit)
  
table1.4 <- factoringImputating(table1.3)

rf_imp <- randomForest(Sex ~., data = table1.4)

rf_impRan <- randomForest(Sex ~ ., data = table1.4_random)

# svm ----

# noimp

svm_tuning_noimp <- tune("svm", Sex ~ ., data = na.omit(table1.3), kernel = "linear",
                  ranges = list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                  scale = TRUE)

ggplot(data = svm_tuning_noimp$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_noimp$best.performance

modelo_svc <- svm(Sex ~ ., data = na.omit(table1.3), 
                  kernel = "linear", 
                  cost = 10, 
                  scale = TRUE)

predicciones = predict(modelo_svc, na.omit(table1.3))

sex_uwu <- na.omit(table1.3)
sex_NoNa <- sex_uwu$Sex

confusionMatrix(predicciones, sex_NoNa)

# random imp

set.seed(123)

table1.4_random <- randomFilling(table1.3)

svm_tuning_imp_ran <- tune("svm", Sex ~ ., data = table1.4_random, kernel = "linear",
                       ranges= list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                       scale = TRUE)
ggplot(data = svm_tuning_imp$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_imp_ran$best.parameters

modelo_svc_imp_ran <- svm(Sex ~ ., data = table1.4_random, 
                      kernel = "linear", 
                      cost = 100, 
                      scale = TRUE)

predicciones_imp_ran = predict(modelo_svc_imp_ran, table1.4_random)

confusionMatrix(predicciones_imp_ran, table1.4_random$Sex)


# MICE imp

table1.4 <- factoringImputating(table1.3)

svm_tuning_imp <- tune("svm", Sex ~ ., data = table1.4, kernel = "linear",
                       ranges= list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                       scale = TRUE)
ggplot(data = svm_tuning_imp$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_imp$best.parameters

modelo_svc_imp <- svm(Sex ~ ., data = table1.4, 
                      kernel = "linear", 
                      cost = 100, 
                      scale = TRUE)

predicciones_imp = predict(modelo_svc_imp, table1.4)

confusionMatrix(predicciones_imp, table1.4$Sex)

# ann






# main test function

mainTest <- function(pathToTable){
  
  
  table1.0 <- readingFillingGrouping(pathToTable)
  table1.1 <- anthroSex(table1.0)
  table1.2 <- normalizingNumeric(table1.1)
  table1.3 <- timingCleanFeatures(table1.2, pathToTable)
  
  table1.3$Sex <- factor(table1.3$Sex)
  
  table1.3 <- table1.3 %>% select (-c(Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
  
  rf_noimp <- randomForest(Sex ~., data = table1.3
                           , na.action = na.omit)
  svm_noimp <- 
  
  table1.4 <- factoringImputating(table1.3)
  
  rf_imp <- randomForest(Sex ~., data = table1.4, na.action = na.omit)
  svm
  
  return(list(rf_noimp, rf_imp))
}


rf_plasmAnt <- mainTest("data/chronicPlasmAnt.csv")

rf_plasmAnt[[1]]
rf_plasmAnt[[2]]


rf_plasmFlav <- mainTest("data/chronicPlasmFlav.csv")

rf_plasmFlav[[1]]
rf_plasmFlav[[2]]

rf_urineFlav <- mainTest("data/chronicUrineFlav.csv")

rf_urineFlav[[1]]
rf_urineFlav[[2]]

rf_urineAnt <- mainTest("data/chronicUrineAnt.csv")

rf_urineAnt[[1]]
rf_urineAnt[[2]]

