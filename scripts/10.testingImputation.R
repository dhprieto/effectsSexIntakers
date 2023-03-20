# testing imputation##### !!!!! IMPUTATION MODIFIED to be RF  !!!!!!!!!! #####

# libraries ----
# 
# library(tidyverse)
# library(scales)
# library(reshape2)
# library(rstatix)
# library(clValid)
# library(mclust)
# library(factoextra)
# library(ggpubr)
# library(randomForest)
# library(e1071)
# library(nnet)
# library(Metrics)
# library(randomForestSRC)
# library(class)
# library(lares)
# 
# # scripts ----
# 
# source("scripts/1.readingFilling.R") #readingFillingGrouping & anthroSex
# source("scripts/2.normalizingFactoring.R") #normalizingNumeric & factoringImputating
# source("scripts/3.descriptiveStatistics.R") 
# source("scripts/4.variablesAnalysis.R") #timingCleanFeatures
# source("scripts/5.imputating.R")

# initializing

# set.seed(123)
# 
# table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- timingCleanFeatures(table1.2, "data/chronicPlasmAnt.csv")
# 
# # prepoccesing ----
# 
# table1.3$Sex <- factor(table1.3$Sex)
# table1.3$Sweetener <- factor(table1.3$Sweetener)
# table1.3$Time <- factor(table1.3$Time)
# table1.3 <- table1.3 %>% select (-c(numVol, grouping, Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
# 
# # imputation ----
# 
# table1.3_nona <- na.omit(table1.3)
# impObject <- factoringImputating(table1.3)
# table1.4_mice <- impObject[[2]]
# table1.4_srandom <- softRandomFilling(table1.3)
# table1.4_frandom <- fullRandomFilling(table1.3)

# function test ----

predictionTest <- function(tabla, predictorClass, predictorReg){

  # set train/test 
  
  set.seed(123)
    
  tr.id.class <- createDataPartition(tabla[,predictorClass], p = 0.7, list = F)
  tr.id.reg <- createDataPartition(tabla[,predictorReg], p = 0.7, list = F)
  
  
  # class
  
  ctrl <- trainControl(method="repeatedcv", repeats = 3, classProbs=TRUE, summaryFunction = twoClassSummary)
  
  predictorForm1 <- as.formula(paste(predictorClass, "~."))
  
  message("##### KNN Classification #####")
  
  
  knnFit.class <- train(predictorForm1, data = tabla, 
                       method = "knn", trControl = ctrl, tuneLength = 20)
  
  message("##### RF Classification #####")
  
  
  rfFit.class <- train(predictorForm1, data = tabla, 
                     method = "ranger", trControl = ctrl, tuneLength = 20)
  
  tune_grid <- expand.grid(nrounds = 200,
                           max_depth = 5,
                           eta = 0.05,
                           gamma = 0.01,
                           colsample_bytree = 0.75,
                           min_child_weight = 0,
                           subsample = 0.5)
  
  message("##### XGBoost Classification #####")
  
  
  xgbFit.class <- train(predictorForm1, data = tabla, method = "xgbTree",
                  trControl=ctrl,
                  tuneGrid = tune_grid,
                  tuneLength = 20)
  
  message("##### NNets Classification #####")
  
  nnFit.class <- train(predictorForm1, data = tabla, 
                       method = "nnet", trControl = ctrl, trace = F)
  
  
  # reg
  
  ctrl.reg <- trainControl(method="repeatedcv", repeats = 3)
  
  predictorForm2 <- as.formula(paste(predictorReg, "~."))
  
  message("##### KNN Regression #####")
  
  knnFit.reg <- train(predictorForm2, data = tabla, 
                  method = "knn", trControl = ctrl.reg, tuneLength = 20)
  
  message("##### RF Regression #####")
  
  
  rfFit.reg <- train(predictorForm2, data = tabla, 
                method = "ranger", trControl = ctrl.reg, tuneLength = 20)
  
  # tune_grid <- expand.grid(nrounds = 200,
  #                          max_depth = 5,
  #                          eta = 0.05,
  #                          gamma = 0.01,
  #                          colsample_bytree = 0.75,
  #                          min_child_weight = 0,
  #                          subsample = 0.5)
  
  message("##### XGBoost Regression #####")
  
  
  xgbFit.reg <- train(predictorForm2, data = tabla, method = "xgbTree",
                        trControl=ctrl.reg,
                        tuneGrid = tune_grid,
                        tuneLength = 20)
  
  message("##### NNets Regression #####")
  
  
  nnFit.reg <- train(predictorForm2, data = tabla, 
                       method = "nnet", trControl = ctrl.reg, trace = F)
  
  
  return(list(knnFit.class, rfFit.class, xgbFit.class,nnFit.class,
              knnFit.reg, rfFit.reg, xgbFit.reg, nnFit.reg))
}

# outcome_1 <- predictionTest(table1.4_mice, "Sex", "DHPAA")
# 
# outcome_2 <- predictionTest(table1.4_frandom, "Sex", "DHPAA")
# 
# outcome_3 <- predictionTest(table1.4_srandom, "Sex", "DHPAA")
# 
# outcome_4 <- predictionTest(table1.3_nona, "Sex", "DHPAA")

# 
# uwu <- data.frame(rbind("KNN" = c(outcome_1[[1]]$results[rownames(outcome_1[[1]]$bestTune),],
#                                     outcome_2[[1]]$results[rownames(outcome_2[[1]]$bestTune),],
#                                     outcome_3[[1]]$results[rownames(outcome_3[[1]]$bestTune),]),
#            "RF" = c(outcome_1[[2]]$results[rownames(outcome_1[[2]]$bestTune),],
#                                     outcome_2[[2]]$results[rownames(outcome_2[[2]]$bestTune),],
#                                     outcome_3[[2]]$results[rownames(outcome_3[[2]]$bestTune),]),
#            "XGBoost" = c(outcome_1[[3]]$results[rownames(outcome_1[[3]]$bestTune),],
#                                     outcome_2[[3]]$results[rownames(outcome_2[[3]]$bestTune),],
#                                     outcome_3[[3]]$results[rownames(outcome_3[[3]]$bestTune),]),
#            "NNet" = c(outcome_1[[4]]$results[rownames(outcome_1[[4]]$bestTune),],
#                                     outcome_2[[4]]$results[rownames(outcome_2[[4]]$bestTune),],
#                                     outcome_3[[4]]$results[rownames(outcome_3[[4]]$bestTune),])))
# 
# uwu$ROC

metricsOutcome <- function(outcome_mice, outcome_f, outcome_s, outcome_n = NULL){

  # return(list(knnFit.class, rfFit.class, xgbFit.class,nnFit.class,
  #             knnFit.reg, rfFit.reg, xgbFit.reg, nnFit.reg))
  
  # table of classification results
  
  df.results.class <- data.frame(mice_KNN = outcome_mice[[1]]$resample, mice_RF = outcome_mice[[2]]$resample, 
                                 mice_XGB = outcome_mice[[3]]$resample, mice_NN = outcome_mice[[4]]$resample,
                                 frand_KNN = outcome_f[[1]]$resample, frand_RF = outcome_f[[2]]$resample, 
                                 frand_XBG = outcome_f[[3]]$resample, frand_NN = outcome_f[[4]]$resample,
                                 srand_KNN = outcome_s[[1]]$resample, srand_RF = outcome_s[[2]]$resample, 
                                 srand_XGB = outcome_s[[3]]$resample, srand_NN = outcome_s[[4]]$resample)
  
  
  
  # plots to compare classification models
  
  results.class <- resamples(list(mice_KNN = outcome_mice[[1]], mice_RF = outcome_mice[[2]], 
                                  mice_XGB = outcome_mice[[3]], mice_NN = outcome_mice[[4]],
                                  frand_KNN = outcome_f[[1]], frand_RF = outcome_f[[2]], 
                                  frand_XBG = outcome_f[[3]], frand_NN = outcome_f[[4]],
                                  srand_KNN = outcome_s[[1]], srand_RF = outcome_s[[2]], 
                                  srand_XGB = outcome_s[[3]], srand_NN = outcome_s[[4]]))
  
  summary.class <- summary(results.class)
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  bxp.class <- bwplot(results.class, scales=scales,
                      main = "Classification performance metrics for different combinations of imputations and models")
  densp.class <- densityplot(results.class, scales=scales, pch = "|")
  dotplot.class <- dotplot(results.class, scales=scales)
  
  
    
  # table of regression results
  
  
  results.reg <- resamples(list(mice_KNN = outcome_mice[[5]], mice_RF = outcome_mice[[6]], 
                                  mice_XGB = outcome_mice[[7]], mice_NN = outcome_mice[[8]],
                                  frand_KNN = outcome_f[[5]], frand_RF = outcome_f[[6]], 
                                  frand_XBG = outcome_f[[7]], frand_NN = outcome_f[[8]],
                                  srand_KNN = outcome_s[[5]], srand_RF = outcome_s[[6]], 
                                  srand_XGB = outcome_s[[7]], srand_NN = outcome_s[[8]]))
  
  summary.reg <- summary(results.reg)
  
  
  # plots to compare regression models
  
  bxp.class <- bwplot(results.reg, scales=scales, 
                      main = "Regression performance metrics for different combinations of imputations and models")
  
  

    
  return(list(df.results.class, results.class,summary.class, bxp.class, densp.class, dotplot.class, 
              results.reg, summary.reg))
        
}

metrics_1 <- metricsOutcome(outcome_1, outcome_f = outcome_2, outcome_s = outcome_3)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bxp.class <- bwplot(metrics_1[[7]], scales=scales, metric = c("Rsquared","MAE", "RMSE"),
                    main = "Performance metrics for different combinations of imputations and models")




# # testing ----
# 
# 
# ### training/test set ----
# 
# set.seed(123)
# 
# tr.id_nona <- createDataPartition(table1.3_nona$Sex, p = 0.7, list=F)
# tr.id_imp <- createDataPartition(table1.4$Sex, p = 0.7, list=F)
# tr.id_srandom <- createDataPartition(table1.4_srandom$Sex, p = 0.7, list=F)
# tr.id_frandom <- createDataPartition(table1.4_frandom$Sex, p = 0.7, list=F)
# 
# ## rf ----
# 
# #### class ----
# 
# rf_prueba_nona_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.3_nona, seed = 321)
# rf_prueba_mice_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.4, seed = 321)
# rf_prueba_fran_class <- randomForestSRC::rfsrc(Sex ~ ., data = table1.4_frandom, seed = 321)
# 
# caret::recall(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
# caret::recall(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
# caret::recall(table1.4$Sex, rf_prueba_mice_class$class.oob)
# 
# caret::precision(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
# caret::precision(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
# caret::precision(table1.4$Sex, rf_prueba_mice_class$class.oob)
# 
# caret::confusionMatrix(table1.3_nona$Sex, rf_prueba_nona_class$class.oob)
# caret::confusionMatrix(table1.4_frandom$Sex, rf_prueba_fran_class$class.oob)
# caret::confusionMatrix(table1.4$Sex, rf_prueba_mice_class$class.oob)
# 
# #### reg ----
# 
# rf_prueba_nona_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.3_nona, seed = 123)
# rf_prueba_mice_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.4, seed = 123)
# rf_prueba_fran_reg <- randomForestSRC::rfsrc(DHPAA ~ ., data = table1.4_frandom, seed = 123)
# 
# 
# rf_test_predict_reg_nona <- predict(rf_prueba_nona_reg, 
#                                     newdata = table1.3_nona[-tr.id_nona, !(colnames(table1.3_nona) %in% "DHPAA")])
# 
# rf_test_predict_reg_fran <- predict(rf_prueba_fran_reg, 
#                                     newdata = table1.4_frandom[-tr.id_frandom,!(colnames(table1.4_frandom) %in% "DHPAA")],)
# 
# rf_test_predict_reg_mice <- predict(rf_prueba_mice_reg, 
#                                     newdata = table1.4[-tr.id_imp,!(colnames(table1.4) %in% "DHPAA")],)
# 
# mae(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)
# rmse(table1.3_nona$DHPAA, rf_prueba_nona_reg$predicted)
# 
# mae(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)
# rmse(table1.4_frandom$DHPAA, rf_prueba_fran_reg$predicted)
# 
# mae(table1.4$DHPAA, rf_prueba_mice_reg$predicted)
# rmse(table1.4$DHPAA, rf_prueba_mice_reg$predicted)
# 
# # caret ----
# 
# #### class ----
# 
# set.seed(400)
# 
# ctrl <- trainControl(method="repeatedcv",repeats = 3, classProbs=TRUE,summaryFunction = twoClassSummary, seeds = 123)
# 
# knnFit_nona <- train(Sex ~ ., data = table1.3_nona[tr.id_nona,], 
#                      method = "knn", trControl = ctrl, tuneLength = 20)
# 
# knnFit_mice <- train(Sex ~ ., data = table1.4[tr.id_imp,], 
#                      method = "knn", trControl = ctrl, tuneLength = 20)
# knnFit_frandom <- train(Sex ~ ., data = table1.4_frandom[tr.id_frandom,], 
#                         method = "knn", trControl = ctrl, tuneLength = 20)
# 
# knnFit_nona$results[rownames(knnFit_nona$bestTune),]
# knnFit_frandom$results[rownames(knnFit_frandom$bestTune),]
# knnFit_mice$results[rownames(knnFit_mice$bestTune),]
# 
# 
# RFFit_nona<- train(Sex ~ ., data = table1.3_nona[tr.id_nona,], 
#              method = "rf", preProcess = c("center","scale"), trControl = ctrl, tuneLength = 20, )
# 
# RFFit_mice <- train(Sex ~ ., data = table1.4[tr.id_imp,], method = "rf", 
#                     trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
# RFFit_frandom <- train(Sex ~ ., data = table1.4_frandom[tr.id_frandom,], method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
# 
# RFFit_nona$results[rownames(RFFit_nona$bestTune),]
# RFFit_frandom$results[rownames(RFFit_frandom$bestTune),]
# RFFit_mice$results[rownames(RFFit_mice$bestTune),]
# 
# RFFit_frandom$finalModel
# 
# 
# results <- resamples(list(RF_mice=RFFit_mice, RF_frandom = RFFit_frandom,
#                           KNN_mice = knnFit_mice, KNN_frandom = knnFit_frandom))
# # box and whisker plots to compare models
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# bwplot(results, scales=scales, main = "Performance metrics for different combinations of imputations and models")
# densityplot(results, scales=scales, pch = "|")
# dotplot(results, scales=scales)
# 
# 
# #Get the confusion matrix to see accuracy value and other parameter values
# precision(predict(knnFit_mice,newdata=table1.4[-tr.id_imp,] ), table1.4[-tr.id_imp,]$Sex )
# precision(predict(knnFit_frandom,newdata=table1.4_frandom[-tr.id_frandom,] ), 
#                 table1.4_frandom[-tr.id_frandom,]$Sex )
# recall(predict(knnFit_mice,newdata=table1.4[-tr.id_imp,] ), table1.4[-tr.id_imp,]$Sex )
# recall(predict(knnFit_frandom,newdata=table1.4_frandom[-tr.id_frandom,] ), 
#           table1.4_frandom[-tr.id_frandom,]$Sex )
# 
# 
# ### knn reg ----
# 
# train_nona = table1.3_nona[tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# train_nona_labels = table1.3_nona[tr.id_nona,"DHPAA"]
# test_nona = table1.3_nona[-tr.id_nona,!(colnames(table1.3_nona) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# test_nona_labels = table1.3_nona[-tr.id_nona,"DHPAA"]
# 
# train_frandom = table1.4_frandom[tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# train_frandom_labels = table1.4_frandom[tr.id_frandom,"DHPAA"]
# test_frandom = table1.4_frandom[-tr.id_frandom,!(colnames(table1.4_frandom) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# test_frandom_labels = table1.4_frandom[-tr.id_frandom,"DHPAA"]
# 
# train_mice = table1.4[tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# train_mice_labels = table1.4[tr.id_imp,"DHPAA"]
# test_mice = table1.4[-tr.id_imp,!(colnames(table1.4) %in% c("Sweetener","Sex", "Time", "DHPAA"))]
# test_mice_labels = table1.4[-tr.id_imp,"DHPAA"]
# 
# knn_nona_reg <- knnreg(x = train_nona, y=train_nona_labels)
# ytest_nona <- predict(knn_nona_reg, test_nona)
# 
# 
# knn_frandom_reg <- knnreg(x = train_frandom, y=train_frandom_labels)
# ytest_frandom <-predict(knn_frandom_reg, test_frandom)
# 
# 
# knn_mice_reg <- knnreg(x = train_mice, y=train_mice_labels)
# ytest_mice <-predict(knn_mice_reg, test_mice)
# 
# 
# mae(test_nona_labels, ytest_nona)
# rmse(test_nona_labels, ytest_nona)
# 
# mae(test_frandom_labels, ytest_frandom)
# rmse(test_frandom_labels, ytest_frandom)
# 
# mae(test_mice_labels, ytest_mice)
# rmse(test_mice_labels, ytest_mice)
# 
# 
# test_individuals = 1:length(ytest_mice)
# test_nona_individuals = 1:length(test_nona_labels)
# 
# 
# # plot reg ----
# 
# df = cbind.data.frame(test_individuals, test_mice_labels, ytest_mice, rf_test_predict_reg_mice$predicted)
# ggplot(data = df, aes(test_individuals, y = test_mice_labels)) + geom_point(colour = "red") + 
#   geom_boxplot(colour= "blue",aes(test_individuals, ytest_mice)) +
#   geom_boxplot(colour= "green",aes(test_individuals, rf_test_predict_reg_mice$predicted)) 
# 
# # resid function ----
# 
# df1 = data.frame(MICE_KNN = test_mice_labels-ytest_mice, 
#                  frandom_KNN = test_frandom_labels-ytest_frandom)
# df3_RF = data.frame(MICE_RF = table1.4$DHPAA - rf_prueba_mice$predicted,
#                  frandom_RF = table1.4_frandom$DHPAA - rf_prueba_fran_reg$predicted)
# df4_RF = data.frame(NoNA_RF = table1.3_nona$DHPAA - rf_prueba_nona_reg$predicted)
# 
# par(mfrow=c(1,1))
# boxplot(df1)
# melt(df1)
# 
# ggplot(data = df1, aes(test_mice_labels, y = test_mice_labels-ytest_mice)) + 
#   geom_boxplot(colour = "red")+ 
#   geom_boxplot(colour= "blue",aes(test_frandom_labels, test_mice_labels-ytest_frandom)) + 
#   geom_boxplot(colour= "green",aes(test_nona_labels, rf_test_predict_reg_mice$predicted)) 
# 
# 
# 
# 
# p1 <- ggplot(df1, aes(factor(variable,level = unique(df1$variable)),as.numeric(value))) +
#   geom_boxplot()+
#   ggtitle(paste("A.Time Flavanones-Plasma"))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
#   labs(y = "residues", x = "variables", fill = "Time")
# 
# p2 <- ggplot(df2, aes(factor(variable,level = unique(df2$variable)),as.numeric(value))) +
#   geom_boxplot()+
#   ggtitle(paste("A.Time Flavanones-Plasma"))+ expand_limits(y = c(0.6,-0.4))
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
#   labs(y = "residues", x = "variables", fill = "Time")
# 
# df2 = data.frame(NoNa = test_nona_labels - ytest_nona)
# par(mfrow = c(3,1))
# 
# ggarrange(p1,p2)
# 
# plot.new()
# 
# set.seed(123)
# 
# # Fondo gris claro
# rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
#      col = "#ebebeb")
# 
# # Añadimos un grid blanco
# grid(nx = NULL, ny = NULL, col = "white", lty = 1,
#      lwd = par("lwd"), equilogs = TRUE)
# 
# lista_boxplot <- list (NoNa_KNN = df2$NoNa, Random_KNN = df1$frandom, MICE_KNN =df1$mice,
#                        NoNa_RF = df4_RF$NoNA_RF, Random_RF = df3_RF$frandom_RF, 
#                        MICE_RF =df3_RF$MICE_RF)
# 
# # Boxplot
# par(new = TRUE)
# boxplot(lista_boxplot, # Datos
#         horizontal = FALSE, # Horizontal o vertical
#         lwd = 2, # Lines width
#         col = rainbow(length(lista_boxplot)), # Color
#         xlab = "Procedure_Algorithm",  # Etiqueta eje X
#         ylab = "Residues",  # Etiqueta eje Y
#         main = "Residues from DHPAA concentration prediction with 
#         different models and imputations", # Título
#         notch = TRUE, # Añade intervalos de confianza para la mediana
#         border = "black",  # Color del borde del boxplot
#         outpch = 25,       # Símbolo para los outliers
#         outbg = "green",   # Color de los datos atípicos
#         whiskcol = "blue", # Color de los bigotes
#         whisklty = 2,      # Tipo de línea para los bigotes
#         lty = 1) # Tipo de línea (caja y mediana)
# 
# 
# boxplot(list (NoNa = df2$NoNa, Random = df1$frandom, MICE =df1$mice))
# 
# 
# 
# 
# plot(test_individuals, test_mice_labels, col = "red", type = "p", lwd=2,
#      main = "A. DHPAA values prediction with MICE imputation")
# points(test_individuals, ytest_mice, col = "blue", lwd=2)
# points(test_individuals, rf_test_predict_reg_mice$predicted, col = "green", lwd=2)
# legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF"), 
#        fill = c("red", "blue", "green"), cex=0.3)
# grid()
# 
# plot(test_nona_individuals, test_nona_labels, col = "red", type = "p", lwd=2,
#      main = "B. DHPAA values prediction with NoNa procedure")
# points(test_nona_individuals, ytest_nona, col = "blue", lwd=2) 
# points(test_nona_individuals, rf_test_predict_reg_nona$predicted, col = "green", lwd=2)
# legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF"), 
#        fill = c("red", "blue", "green"),cex=0.3)
# grid()
# 
# plot(test_individuals, test_frandom_labels, col = "red", type = "p", lwd=2,
#      main = "C. DHPAA values prediction with Random imputation")
# points(test_individuals, ytest_frandom, col = "blue", lwd=2)
# points(test_individuals, rf_test_predict_reg_fran$predicted, col = "green", lwd=2)
# legend("topright",  legend = c("original", "predicted-KNN", "predicted-RF" ), 
#        fill = c("red", "blue", "green"), cex=0.3)
# grid()
# 
# 
# 
# # main test function -----
# 
# mainTest <- function(pathToTable){
#   
#   
#   table1.0 <- readingFillingGrouping(pathToTable)
#   table1.1 <- anthroSex(table1.0)
#   table1.2 <- normalizingNumeric(table1.1)
#   table1.3 <- timingCleanFeatures(table1.2, pathToTable)
#   
#   table1.3$Sex <- factor(table1.3$Sex)
#   
#   table1.3 <- table1.3 %>% select (-c(Weight, BMI, Fat, CVRI, Bpmin, Bpmax, Frec))
#   
#   rf_noimp <- randomForest(Sex ~., data = table1.3
#                            , na.action = na.omit)
# 
#   table1.4 <- factoringImputating(table1.3)
#   
#   rf_imp <- randomForest(Sex ~., data = table1.4, na.action = na.omit)
#   svm
#   
#   return(list(rf_noimp, rf_imp))
# }
# 
# 
# rf_plasmAnt <- mainTest("data/chronicPlasmAnt.csv")
# 
# rf_plasmAnt[[1]]
# rf_plasmAnt[[2]]
# 
# 
# rf_plasmFlav <- mainTest("data/chronicPlasmFlav.csv")
# 
# rf_plasmFlav[[1]]
# rf_plasmFlav[[2]]
# 
# rf_urineFlav <- mainTest("data/chronicUrineFlav.csv")
# 
# rf_urineFlav[[1]]
# rf_urineFlav[[2]]
# 
# rf_urineAnt <- mainTest("data/chronicUrineAnt.csv")
# 
# rf_urineAnt[[1]]
# rf_urineAnt[[2]]

