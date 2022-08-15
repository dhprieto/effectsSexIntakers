## feature analysis

# source("scripts/main.R")
library(caret)
library(AppliedPredictiveModeling)


# timing the features

timingCleanFeatures <- function(tabla){

  for (i in seq(1:nrow(tabla))){
    if (tabla$Time[i] == "Initial"){
      tabla$Peso[i] = tabla$Peso.inicial[i]
      tabla$IMC[i] = tabla$IMC.Inicial[i]
      tabla$Grasa[i] = tabla$Grasa.inicial[i]
      tabla$IRCV[i] = tabla$IRCV.inicial[i]
      tabla$Bpmin[i] = tabla$Bpmin.inicial[i]
      tabla$Bpmax[i] = tabla$Bpmax.inicial[i]
      tabla$Frec[i] = tabla$Frec.inicial[i]
      
    }
    
    else if (tabla$Time[i] == "Final"){
      tabla$Peso[i] = tabla$Peso.final[i]
      tabla$IMC[i] = tabla$IMC.Final[i]
      tabla$Grasa[i] = tabla$Grasa.final[i]
      tabla$IRCV[i] = tabla$IRCV.Final[i]
      tabla$Bpmin[i] = tabla$Bpmin.final[i]
      tabla$Bpmax[i] = tabla$Bpmax.final[i]
      tabla$Frec[i] = tabla$Frec.final[i]
    }
    
    
  }
  
  # cleaning of non relevant variables
  
  subset(tabla, select =-c(Peso.inicial, Peso.final, Delta.Peso, Talla, IMC.Inicial, IMC.Final, 
                           Delta.IMC, Grasa.inicial, Grasa.final, Delta.Grasa, IRCV.Final, IRCV.inicial, 
                           Bpmin.final, Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  
}




anthro <- c("Peso", "IMC", "Grasa", "IRCV", 
            "Bpmin", "Bpmax", "Frec")

correlatedFeatures <- function (tabla) {
  
  
  numericFeatures <- tabla[,unlist(lapply(tabla,is.numeric))]
  
  
  corrMatrix <- cor(numericFeatures)
  highcorr <- findCorrelation(corrMatrix, 0.5)
  
  print(highcorr)
  
}

## concrete test

# # correlation all vars
# 
# correlatedFeatures(table1.4)
# 
# colnames(table1.4[,unlist(lapply(table1.4,is.numeric))])
# 
# # correlation only met
# 
# correlatedFeatures(table1.4 %>% select (-all_of(anthro)))
# colnames(table1.4[,unlist(lapply(table1.4,is.numeric))]%>% select (-all_of(anthro)))
# 
# # correlation only anthro
# 
# correlatedFeatures(table1.4 %>% select (all_of(anthro)))
# colnames(table1.4[,unlist(lapply(table1.4,is.numeric))]%>% select (all_of(anthro)))

# #test2
# # feature importance about Sex ----
# 
# # only metabolic features
# 
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(Sex~., data=table1.4%>%select(-all_of(anthro), -numVol, -grouping) %>% filter (Time == "Final"), method="lvq", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
# 
# # all features
# 
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(Sex~., data=table1.4%>%select( -numVol, -grouping) %>% filter (Time == "Final"), method="lvq", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)


# # test
# table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# table1.4 <- timingCleanFeatures(table1.3)
