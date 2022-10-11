## feature analysis

# source("scripts/main.R")
library(caret)
library(AppliedPredictiveModeling)


# timing the features

timingCleanFeatures <- function(tabla, pathToTable){

  for (i in seq(1:nrow(tabla))){
    if (tabla$Time[i] == "Initial"){
      tabla$Weight[i] = tabla$Peso.inicial[i]
      tabla$BMI[i] = tabla$IMC.Inicial[i]
      tabla$Fat[i] = tabla$Grasa.inicial[i]
      tabla$CVRI[i] = tabla$IRCV.inicial[i]
      tabla$Bpmin[i] = tabla$Bpmin.inicial[i]
      tabla$Bpmax[i] = tabla$Bpmax.inicial[i]
      tabla$Frec[i] = tabla$Frec.inicial[i]
      
    }
    
    else if (tabla$Time[i] == "Final"){
      tabla$Weight[i] = tabla$Peso.final[i]
      tabla$BMI[i] = tabla$IMC.Final[i]
      tabla$Fat[i] = tabla$Grasa.final[i]
      tabla$CVRI[i] = tabla$IRCV.Final[i]
      tabla$Bpmin[i] = tabla$Bpmin.final[i]
      tabla$Bpmax[i] = tabla$Bpmax.final[i]
      tabla$Frec[i] = tabla$Frec.final[i]
    }
    
  # return(tabla)  
  }
  
  # cleaning of non relevant variables
  
  tabla <- tabla %>% select(-c(Peso.inicial, Peso.final, Delta.Peso, Talla, IMC.Inicial, IMC.Final, 
                           Delta.IMC, Grasa.inicial, Grasa.final, Delta.Grasa, IRCV.Final, IRCV.inicial, 
                           Bpmin.final, Bpmin.inicial, Bpmax.final, Bpmax.inicial, Frec.final, Frec.inicial))
  
  
  if (pathToTable == "data/chronicPlasmAnt.csv"){
  rename(tabla, CA = Caffeic.Acid..CA..1, CA.Gluc = CA.Gluc.1, CA.Sulfate = CA.Sulfate.1, Total.CA = TOTAL.CA.1,
         DHPAA = X3.4.Dihidroxiphenilacetic.acid..DHPAA..1, DHPAA.Gluc = DHPAA.Gluc.1, DHPAA.di.Gluc = DHPAA.di.Gluc.1,
         DHPAA.Gluc.Sulfate = DHPAA.Gluc.sulfate.1, DHPAA.di.Sulfate = DHPAA.di.Sulfate.1, Total.DHPAA = TOTAL.DHPAA.1,
         TFA.Gluc = TFA.Gluc.1, TFA.Sulfate = TFA.Sulfate.1, Total.TFA = TOTAL.TFA.1, VA = Vanillic.Acid..VA..1,
         VA.GG = VA.GG.1, VA.Sulfate = VA.Sulfate.1, VA.Gluc.Sulfate = VA.Gluc.sulfate.1, 
         VA.di.Sulfate = VA.di.sulfate.1, Total.VA = Total.VA.1)

  }
  
  else if(pathToTable == "data/chronicPlasmFlav.csv"){
    rename(tabla, E = Eriodictiol..E..1, ES = ES.1 , Total.E = TOTAL.E.1, HE.G = HE.G.1, NG = NG.1)
  }
  
  else if(pathToTable == "data/chronicUrineFlav.csv"){
    rename(tabla, E = Eriodyctiol..E..1, EG = ES.1, ES = ES.1 , Total.E = TOTAL.E.1, HE = HE.1, HE.G = HE.G.1, 
           HE.GG = HE.GG.1, Total.HE = TOTAL.HE.1, N = Naringenine..N..1, NG = NG.1, NGG = NGG.1, NS = NS.1, 
           Total.N = TOTAL.N.1)
  }
  
  else if (pathToTable == "data/chronicUrineAnt.csv"){
    rename(tabla, CA = Caffeic.acid..CA..1, CA.Gluc = CA.Gluc.1, CA.Sulfate = CA.Sulfate.1, 
           CA.Gluc.Sulfate = CA.Gluc.sulfate.1, Total.CA = TOTAL.CA.1,
           DHPAA = X3.4...Dihidroxiphenilacetic.acid..DHPAA..1, 
           DHPAA.Gluc = DHPAA.Gluc.1, DHPAA.di.Gluc = DHPAA.di.Gluc.1, DHPAA.Gluc.Sulfate = DHPAA.Gluc.sulfate.1, 
           DHPAA.di.Sulfate = DHPAA.di.Sulfate.1, Total.DHPAA = TOTAL.DHPAA.1, TFA.Gluc = TFA.Gluc.1, 
           TFA.Sulfate = TFA.Sulfate.1, Total.TFA = TOTAL.TFA.1, VA = Vanillic.Acid..VA..1, VA.GG = VA.GG.1, 
           VA.Gluc.Sulfate = VA.Gluc.sulfate.1, VA.di.Sulfate = VA.di.sulfate.1, Total.VA = Total.VA.1)
  }

    
  # return(tabla)
  }
  

# test

# table1.0 <- readingFillingGrouping("data/chronicPlasmFlav.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- timingCleanFeatures(table1.2, "data/chronicPlasmFlav.csv")
# table1.4 <- factoringImputating(table1.3)



# anthro <- c("Weight", "BMI", "Fat", "CVRI",
            # "Bpmin", "Bpmax", "Frec")
# 
# correlatedFeatures <- function (tabla) {
#   
#   
#   numericFeatures <- tabla[,unlist(lapply(tabla,is.numeric))]
#   
#   
#   corrMatrix <- cor(numericFeatures)
#   highcorr <- findCorrelation(corrMatrix, 0.5)
#   
#   print(highcorr)
#   
# }

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





# 
# # only metabolic features
# 
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(Sex~., data=table1.4%>%select(-all_of(anthro), -numVol, -grouping), method="lvq", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
