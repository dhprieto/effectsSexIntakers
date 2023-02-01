# testing imputation

# libraries ----

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
library(nnet)

# scripts ----

source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
source("scripts/3.descriptiveStatistics.R") 
source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
source("scripts/5.imputating.R")

# initializing

set.seed(123)

table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
table1.1 <- anthroSex(table1.0)
table1.2 <- normalizingNumeric(table1.1)
table1.3 <- timingCleanFeatures(table1.2, "data/chronicPlasmAnt.csv")

# function random filling ----

softRandomFilling <- function(tabla){
  
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

zeroOneRandomFilling <- function(tabla){
  
  for (var in names(tabla)){
    cat(var,"\n") # Print the variable name for debug purposes
    flush.console()
    
    if (class(tabla[[var]]) == "numeric"){
      print(length(tabla[is.na(tabla[[var]]), var]))
      # random number into the var variance
      
      for (i in rev(seq(1, length(tabla[is.na(tabla[[var]]), var])))){
        
        tabla[is.na(tabla[[var]]), var][i] <- runif(1, min= 0,max= 1)
        
        
      }
      
    }
    
    
  } 
  
  return(tabla)
}



# preprocessing ----

table1.3$Sex <- factor(table1.3$Sex)
table1.3$Sweetener <- factor(table1.3$Sweetener)
table1.3$Time <- factor(table1.3$Time)
table1.3 <- table1.3 %>% select (-c(numVol, grouping, Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
table1.3_nona <- na.omit(table1.3)
table1.4 <- factoringImputating(table1.3)
table1.4_srandom <- softRandomFilling(table1.3)
table1.4_frandom <- zeroOneRandomFilling(table1.3)

## training/test set ----

tr.id_nona <- createDataPartition(table1.3_nona$Sex, p = 0.7, list=F)
tr.id_imp <- createDataPartition(table1.4$Sex, p = 0.7, list=F)
tr.id_srandom <- createDataPartition(table1.4_srandom$Sex, p = 0.7, list=F)
tr.id_frandom <- createDataPartition(table1.4_frandom$Sex, p = 0.7, list=F)




hist(table1.4_frandom$VA)
hist(table1.4_srandom$VA)

sapply(table1.3, function(y) sum(length(which(is.na(y)))))


#


# rf ----

# no imp, no na

rf_noimp <- randomForest(Sex ~., data = table1.3_nona[tr.id_nona,], na.action = na.omit)

confusionMatrix(predict(rf_noimp, table1.3_nona[-tr.id_nona,]), 
                table1.3_nona[-tr.id_nona,]$Sex)
 
# imp soft random

rf_impsRan <- randomForest(Sex ~ ., data = table1.4_srandom[tr.id_srandom,])

confusionMatrix(predict(rf_impsRan, table1.4_srandom[-tr.id_srandom,]), 
                table1.4_srandom[-tr.id_srandom,]$Sex)

# imp full random


rf_impfRan  <- randomForest(Sex ~ ., data = table1.4_frandom[tr.id_frandom,])

confusionMatrix(predict(rf_impfRan, table1.4_frandom[-tr.id_frandom,]), 
                table1.4_frandom[-tr.id_frandom,]$Sex)

# imp mice

rf_imp <- randomForest(Sex ~., data = table1.4[tr.id_imp,])

confusionMatrix(predict(rf_imp, table1.4[-tr.id_imp,]), table1.4[-tr.id_imp,]$Sex)


# svm ----

# noimp

svm_tuning_noimp <- tune("svm", Sex ~ ., data = table1.3_nona[tr.id_nona,], kernel = "linear",
                  ranges = list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                  scale = TRUE)

ggplot(data = svm_tuning_noimp$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_noimp$best.parameters[[1]]

modelo_svc <- svm(Sex ~ ., data = table1.3_nona[tr.id_nona,], 
                  kernel = "linear", 
                  cost = 100, 
                  scale = TRUE)

predicciones <- predict(modelo_svc, table1.3_nona[-tr.id_nona,])

confusionMatrix(predicciones, table1.3_nona[-tr.id_nona,]$Sex)

# soft random imp

svm_tuning_imp_sran <- tune("svm", Sex ~ ., data = table1.4_srandom[tr.id_srandom,], 
                            kernel = "linear",
                            ranges= list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                            scale = TRUE)
ggplot(data = svm_tuning_imp_sran$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_imp_sran$best.parameters

modelo_svc_imp_sran <- svm(Sex ~ ., data = table1.4_random[tr.id_srandom,], 
                      kernel = "linear", 
                      cost = 100, 
                      scale = TRUE)

predicciones_imp_sran = predict(modelo_svc_imp_sran, table1.4_srandom[-tr.id_srandom,])

confusionMatrix(predicciones_imp_sran, table1.4_srandom[-tr.id_srandom,]$Sex)

# full random imp

svm_tuning_imp_fran <- tune("svm", Sex ~ ., data = table1.4_frandom[tr.id_frandom,],
                            kernel = "linear",
                            ranges= list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                            scale = TRUE)
ggplot(data = svm_tuning_imp_fran$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_imp_fran$best.parameters

modelo_svc_imp_fran <- svm(Sex ~ ., data = table1.4_frandom[tr.id_frandom,], 
                          kernel = "linear", 
                          cost = 100, 
                          scale = TRUE)

predicciones_imp_fran <- predict(modelo_svc_imp_fran, table1.4_frandom[-tr.id_frandom,])

confusionMatrix(predicciones_imp_fran, table1.4_frandom[-tr.id_frandom,]$Sex)


# MICE imp

svm_tuning_imp <- tune("svm", Sex ~ ., data = table1.4[tr.id_imp,], kernel = "linear",
                       ranges= list(cost = c(0.0001, 0.0005, 0.001, 0.01, 0.1, 1, 10, 100)),
                       scale = TRUE)
ggplot(data = svm_tuning_imp$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

svm_tuning_imp$best.parameters

modelo_svc_imp <- svm(Sex ~ ., data = table1.4[tr.id_imp,], 
                      kernel = "linear", 
                      cost = 10, 
                      scale = TRUE)

predicciones_imp <- predict(modelo_svc_imp, table1.4[-tr.id_imp,])

confusionMatrix(predicciones_imp, table1.4[-tr.id_imp,]$Sex)

# ann ----

nn_noimp <- nnet(Sex ~ ., data = table1.3,
                 size = 3, maxit=10000,
                 decay = .001, rang = 0.1,
                 na.action = na.omit, skip = T)

pred_noimp <- predict(nn_noimp, newdata = na.omit(table1.3), type = "class")

confusionMatrix(factor(pred_noimp), na.omit(table1.3)$Sex)

# random imp

nn_random_imp <- nnet(Sex ~ ., data = table1.4_random,
                 size = 3, maxit=10000,
                 decay = .001, rang = 0.1,
                 na.action = na.omit, skip = T)



pred_random_imp <- predict(nn_random_imp, newdata = table1.4_random, type = "class")

confusionMatrix(factor(pred_random_imp), table1.4_random$Sex)

# mice imp

nn_mice_imp <- nnet(Sex ~ ., data = table1.4,
                    size = 3, maxit=10000,
                    decay = .001, rang = 0.1,
                    na.action = na.omit, skip = T)

pred_mice_imp <- predict(nn_mice_imp, newdata = table1.4, type = "class")

confusionMatrix(factor(pred_mice_imp), table1.4$Sex)

# main test function -----

mainTest <- function(pathToTable){
  
  
  table1.0 <- readingFillingGrouping(pathToTable)
  table1.1 <- anthroSex(table1.0)
  table1.2 <- normalizingNumeric(table1.1)
  table1.3 <- timingCleanFeatures(table1.2, pathToTable)
  
  table1.3$Sex <- factor(table1.3$Sex)
  
  table1.3 <- table1.3 %>% select (-c(Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
  
  rf_noimp <- randomForest(Sex ~., data = table1.3
                           , na.action = na.omit)

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

