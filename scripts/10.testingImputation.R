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
library(Metrics)
library(randomForestSRC)
library(class)
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
        
        tabla[is.na(tabla[[var]]), var][i] <- runif(1, min= -1,max= 2)
        
        
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

# rf ----

library("randomForestSRC")

rf_prueba_nona_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.3_nona, seed = 123)
rf_prueba_mice_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.4, seed = 123)
rf_prueba_fran_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.4_frandom, seed = 123)

rf_prueba_nona_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.3_nona, seed = 123)
rf_prueba_mice_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.4, seed = 123)
rf_prueba_fran_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.4_frandom, seed = 123)


rf_test_predict_reg_nona <- predict(rf_prueba_nona_reg, 
                                    newdata = table1.3_nona[-tr.id_nona, !(colnames(table1.3_nona) %in% "DHPAA")])

rf_test_predict_reg_fran <- predict(rf_prueba_fran_reg, 
                                    newdata = table1.4_frandom[-tr.id_frandom,!(colnames(table1.4_frandom) %in% "DHPAA")],)

rf_test_predict_reg_mice <- predict(rf_prueba_mice_reg, 
                                    newdata = table1.4[-tr.id_imp,!(colnames(table1.4) %in% "DHPAA")],)





mae(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)
rmse(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)

mae(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)
rmse(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)

mae(table1.4$DHPAA, rf_prueba_mice$predicted)
rmse(table1.4$DHPAA, rf_prueba_mice$predicted)

caret::recall(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
caret::recall(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
caret::recall(table1.4$Sex, rf_prueba_mice_class$class.oob)

caret::precision(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
caret::precision(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
caret::precision(table1.4$Sex, rf_prueba_mice_class$class.oob)

caret::confusionMatrix(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
caret::confusionMatrix(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
caret::confusionMatrix(table1.4$Sex, rf_prueba_mice_class$class.oob)



accuracy(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)

library(class)

train_nona = table1.3_nona[tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time"))]
train_nona_labels = table1.3_nona[tr.id_nona,"Sex"]
test_nona = table1.3_nona[-tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time"))]
test_nona_labels = table1.3_nona[-tr.id_nona,"Sex"]

train_frandom = table1.4_frandom[tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time"))]
train_frandom_labels = table1.4_frandom[tr.id_frandom,"Sex"]
test_frandom = table1.4_frandom[-tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time"))]
test_frandom_labels = table1.4_frandom[-tr.id_frandom,"Sex"]



train_mice = table1.4[tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time"))]
train_mice_labels = table1.4[tr.id_imp,"Sex"]
test_mice = table1.4[-tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time"))]
test_mice_labels = table1.4[-tr.id_imp,"Sex"]


knn_nona_class <- knn(train = train_nona, test = test_nona, cl=train_nona_labels, k = 6)
knn_frandom_class <- knn(train = train_frandom, test = test_frandom, cl=train_frandom_labels, k = 13)
knn_mice_class <- knn(train = train_mice, test = test_mice, cl=train_mice_labels, k = 13)

caret::recall(test_nona_labels, knn_nona_class)
caret::recall(test_frandom_labels, knn_frandom_class)
caret::recall(test_mice_labels, knn_mice_class)

caret::confusionMatrix(test_nona_labels, knn_nona_class)
caret::confusionMatrix(test_frandom_labels, knn_frandom_class)
caret::confusionMatrix(test_mice_labels, knn_mice_class)

caret::precision(test_nona_labels, knn_nona_class)
caret::precision(test_frandom_labels, knn_frandom_class)
caret::precision(test_mice_labels, knn_mice_class)

### reg ----
train_nona = table1.3_nona[tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
train_nona_labels = table1.3_nona[tr.id_nona,"DHPAA"]
test_nona = table1.3_nona[-tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
test_nona_labels = table1.3_nona[-tr.id_nona,"DHPAA"]

train_frandom = table1.4_frandom[tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
train_frandom_labels = table1.4_frandom[tr.id_frandom,"DHPAA"]
test_frandom = table1.4_frandom[-tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
test_frandom_labels = table1.4_frandom[-tr.id_frandom,"DHPAA"]

train_mice = table1.4[tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
train_mice_labels = table1.4[tr.id_imp,"DHPAA"]
test_mice = table1.4[-tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
test_mice_labels = table1.4[-tr.id_imp,"DHPAA"]

knn_nona_reg <- knnreg(x = train_nona, y=train_nona_labels)
ytest_nona <- predict(knn_nona_reg, test_nona)


knn_frandom_reg <- knnreg(x = train_frandom, y=train_frandom_labels)
ytest_frandom <-predict(knn_frandom_reg, test_frandom)


knn_mice_reg <- knnreg(x = train_mice, y=train_mice_labels)
ytest_mice <-predict(knn_mice_reg, test_mice)


mae(test_nona_labels, ytest_nona)
rmse(test_nona_labels, ytest_nona)

mae(test_frandom_labels, ytest_frandom)
rmse(test_frandom_labels, ytest_frandom)

mae(test_mice_labels, ytest_mice)
rmse(test_mice_labels, ytest_mice)


test_individuals = 1:length(ytest_mice)
test_nona_individuals = 1:length(test_nona_labels)

#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
### plot reg ----
df = cbind.data.frame(test_individuals, test_mice_labels, ytest_mice, rf_test_predict_reg_mice$predicted)
ggplot(data = df, aes(test_individuals, y = test_mice_labels)) + geom_point(colour = "red") + 
  geom_boxplot(colour= "blue",aes(test_individuals, ytest_mice)) +
  geom_boxplot(colour= "green",aes(test_individuals, rf_test_predict_reg_mice$predicted)) 

# resid function ----

mae(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)
rmse(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)

mae(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)
rmse(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)

mae(table1.4$DHPAA, rf_prueba_mice$predicted)
rmse(table1.4$DHPAA, rf_prueba_mice$predicted)




df1 = data.frame(MICE_KNN = test_mice_labels-ytest_mice, 
                 frandom_KNN = test_frandom_labels-ytest_frandom)
df3_RF = data.frame(MICE_RF = table1.4$DHPAA - rf_prueba_mice$predicted,
                 frandom_RF = table1.4_frandom$DHPAA - rf_prueba_fran_reg$predicted)
df4_RF = data.frame(NoNA_RF = table1.3_nona$DHPAA - rf_prueba_nona_reg$predicted)

par(mfrow=c(1,1))
boxplot(df1)
melt(df1)

ggplot(data = df1, aes(test_mice_labels, y = test_mice_labels-ytest_mice)) + 
  geom_boxplot(colour = "red")+ 
  geom_boxplot(colour= "blue",aes(test_frandom_labels, test_mice_labels-ytest_frandom)) + 
  geom_boxplot(colour= "green",aes(test_nona_labels, rf_test_predict_reg_mice$predicted)) 

p1 <- ggplot(df1, aes(factor(variable,level = unique(df1$variable)),as.numeric(value))) +
  geom_boxplot()+
  ggtitle(paste("A.Time Flavanones-Plasma"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
  labs(y = "residues", x = "variables", fill = "Time")

p2 <- ggplot(df2, aes(factor(variable,level = unique(df2$variable)),as.numeric(value))) +
  geom_boxplot()+
  ggtitle(paste("A.Time Flavanones-Plasma"))+ expand_limits(y = c(0.6,-0.4))
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
  labs(y = "residues", x = "variables", fill = "Time")

df2 = data.frame(NoNa = test_nona_labels - ytest_nona)
par(mfrow = c(3,1))

ggarrange(p1,p2)

plot.new()

set.seed(123)

# Fondo gris claro
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Añadimos un grid blanco
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)

lista_boxplot <- list (NoNa_KNN = df2$NoNa, Random_KNN = df1$frandom, MICE_KNN =df1$mice,
                       NoNa_RF = df4_RF$NoNA_RF, Random_RF = df3_RF$frandom_RF, 
                       MICE_RF =df3_RF$MICE_RF)

# Boxplot
par(new = TRUE)
boxplot(lista_boxplot, # Datos
        horizontal = FALSE, # Horizontal o vertical
        lwd = 2, # Lines width
        col = rainbow(length(lista_boxplot)), # Color
        xlab = "Procedure_Algorithm",  # Etiqueta eje X
        ylab = "Residues",  # Etiqueta eje Y
        main = "Residues from DHPAA concentration prediction with 
        different models and imputations", # Título
        notch = TRUE, # Añade intervalos de confianza para la mediana
        border = "black",  # Color del borde del boxplot
        outpch = 25,       # Símbolo para los outliers
        outbg = "green",   # Color de los datos atípicos
        whiskcol = "blue", # Color de los bigotes
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 1) # Tipo de línea (caja y mediana)


boxplot(list (NoNa = df2$NoNa, Random = df1$frandom, MICE =df1$mice))




plot(test_individuals, test_mice_labels, col = "red", type = "p", lwd=2,
     main = "A. DHPAA values prediction with MICE imputation")
points(test_individuals, ytest_mice, col = "blue", lwd=2)
points(test_individuals, rf_test_predict_reg_mice$predicted, col = "green", lwd=2)
legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF"), 
       fill = c("red", "blue", "green"), cex=0.3)
grid()

plot(test_nona_individuals, test_nona_labels, col = "red", type = "p", lwd=2,
     main = "B. DHPAA values prediction with NoNa procedure")
points(test_nona_individuals, ytest_nona, col = "blue", lwd=2) 
points(test_nona_individuals, rf_test_predict_reg_nona$predicted, col = "green", lwd=2)
legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF"), 
       fill = c("red", "blue", "green"),cex=0.3)
grid()

plot(test_individuals, test_frandom_labels, col = "red", type = "p", lwd=2,
     main = "C. DHPAA values prediction with Random imputation")
points(test_individuals, ytest_frandom, col = "blue", lwd=2)
points(test_individuals, rf_test_predict_reg_fran$predicted, col = "green", lwd=2)
legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF" ), 
       fill = c("red", "blue", "green"), cex=0.3)
grid()



# no imp, no na

rf_noimp <- randomForest(Sex ~., x = table1.3_nona[tr.id_nona,!(colnames(table1.3_nona) %in% "Sex")], 
                         y = table1.3_nona[tr.id_nona,"Sex"],
                         xtest = table1.3_nona[!tr.id_nona,!(colnames(table1.3_nona) %in% "Sex")],
                         ytest = table1.3_nona[!tr.id_nona,"Sex"])


x = table1.3_nona[tr.id_nona,!(colnames(table1.3_nona) %in% "Sex")]
y = table1.3_nona[tr.id_nona,"Sex"]
xtest = table1.3_nona[-tr.id_nona,!(colnames(table1.3_nona) %in% "Sex")]
ytest = table1.3_nona[-tr.id_nona,"Sex"]

confusionMatrix(predict(rf_noimp, table1.3_nona[-tr.id_nona,]), 
                table1.3_nona[-tr.id_nona,]$Sex)
 
# imp soft random

rf_impsRan <- randomForest(Sex ~ ., data = table1.4_srandom[tr.id_srandom,], )
tuneRF()
confusionMatrix(predict(rf_impsRan, table1.4_srandom[-tr.id_srandom,]), 
                table1.4_srandom[-tr.id_srandom,]$Sex)


rf_impsRan <- randomForest(Sex ~., 
                           x = table1.4_srandom[tr.id_srandom,!(colnames(table1.4_srandom) %in% c("Sex", "Time", "Sweetener"))], 
                         y = table1.4_srandom[tr.id_srandom,"Sex"],
                         xtest = table1.4_srandom[-tr.id_srandom,!(colnames(table1.4_srandom) %in% c
                                                                   ("Sex", "Time", "Sweetener"))],
                         ytest = table1.4_srandom[-tr.id_srandom,"Sex"])

x = table1.4_srandom[tr.id_srandom,!(colnames(table1.4_srandom) %in% "Sex")]
y = table1.4_srandom[tr.id_srandom,"Sex"]
xtest = table1.4_srandom[-tr.id_srandom,!(colnames(table1.4_srandom) %in% "Sex")]
ytest = table1.4_srandom[-tr.id_srandom,"Sex"]


set.seed(123)
rf_impsRan <- randomForest(Sex ~., data = table1.4, subset = tr.id_srandom, mtry=21, importance = T,
                           xtest = table1.4[-tr.id_srandom,!(colnames(table1.4_srandom) %in% "Sex")],
                           ytest = table1.4[-tr.id_srandom,"Sex"], replace = F)

rf_impsRan <- randomForest(Sex ~., data = table1.4_srandom, subset = tr.id_srandom, mtry=21, importance = T,
                           xtest = table1.4_srandom[-tr.id_srandom,!(colnames(table1.4_srandom) %in% "Sex")],
                           ytest = table1.4_srandom[-tr.id_srandom,"Sex"], replace = F)


predict(rf_impsRan, x)

randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE, 
             xtest=subset(testB, select=-medv), ytest=testB$medv)

# floor(sqrt(ncol(table1.4_srandom[tr.id_srandom,!(colnames(table1.4_srandom) %in% "Sex")])))
# 
# if (!is.null(y) && !is.factor(y))
#   max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),

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

# naivebayes ----

### nona

model_nB_nona <- naiveBayes(Sex ~ ., data = table1.3_nona[tr.id_nona,])
predicciones_nona <- predict(model_nB_nona, table1.3_nona[-tr.id_nona,])

confusionMatrix(predicciones_nona, table1.3_nona[-tr.id_nona,]$Sex)

### soft random

model_nB_sran <- naiveBayes(Sex ~ ., data = table1.4_srandom[tr.id_srandom,])
predicciones_sran <- predict(model_nB_sran, table1.4_srandom[-tr.id_srandom,])

confusionMatrix(predicciones_sran, table1.4_srandom[-tr.id_srandom,]$Sex)

### full random

model_nB_fran <- naiveBayes(Sex ~ ., data = table1.4_frandom[tr.id_frandom,])
predicciones_fran <- predict(model_nB_fran, table1.4_frandom[-tr.id_frandom,])

confusionMatrix(predicciones_fran, table1.4_frandom[-tr.id_frandom,]$Sex)

### mice

model_nB_mice <- naiveBayes(Sex ~ ., data = table1.4[tr.id_imp,])
predicciones_mice <- predict(model_nB_mice, table1.4[-tr.id_imp,])

confusionMatrix(predicciones_mice, table1.4[-tr.id_imp,]$Sex)


# ann ----

set.seed(123)

### nona

model_nn_nona <- nnet(Sex ~ ., data = table1.3_nona[tr.id_nona,],
                 size = 3, maxit=10000,
                 decay = .001, rang = 0.1,
                 na.action = na.omit, skip = T)

pred_nn_nona <- predict(model_nn_nona, newdata = table1.3_nona[-tr.id_nona,], type = "class")

confusionMatrix(factor(pred_nn_nona), table1.3_nona[-tr.id_nona,]$Sex)

# soft random

model_nn_sran <- nnet(Sex ~ ., data = table1.4_srandom[tr.id_srandom,],
                 size = 3, maxit=10000,
                 decay = .001, rang = 0.1,
                 na.action = na.omit, skip = T)

pred_nn_sran <- predict(model_nn_sran, newdata = table1.4_srandom[-tr.id_srandom,], type = "class")

confusionMatrix(factor(pred_nn_sran), table1.4_srandom[-tr.id_srandom,]$Sex)

##### accuracy 0.5 -> not the expected result

# full random

model_nn_fran <- nnet(Sex ~ ., data = table1.4_frandom[tr.id_frandom,],
                      size = 3, maxit=10000,
                      decay = .001, rang = 0.1,
                      na.action = na.omit, skip = T)

pred_nn_fran <- predict(model_nn_fran, newdata = table1.4_frandom[-tr.id_frandom,], type = "class")

confusionMatrix(factor(pred_nn_fran), table1.4_frandom[-tr.id_frandom,]$Sex)

# mice imp

model_nn_mice <- nnet(Sex ~ ., data = table1.4[tr.id_imp,],
                    size = 3, maxit=10000,
                    decay = .001, rang = 0.1,
                    na.action = na.omit, skip = T)

pred_nn_mice <- predict(model_nn_mice, newdata = table1.4[-tr.id_imp,], type = "class")

confusionMatrix(factor(pred_nn_mice), table1.4[-tr.id_imp,]$Sex)

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

