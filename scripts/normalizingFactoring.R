# exploring the data and variables

source("scripts/readingFilling.R")
library(scales)
library(mice)


# normalizing

normalizingNumeric <- function(tableComplete) {
  
  for (i in colnames(tableComplete)){
   
    if (is.numeric(tableComplete[,i]) && i != "numVol"){

      tableComplete[,i] <- scales::rescale(tableComplete[,i])
      
    }
     
  }
return (tableComplete)    
}


factoringImputating <- function(tableNorm){

  tableNorm$Sweetener <- factor(tableNorm$Sweetener) 
  tableNorm$Sex <- factor(tableNorm$Sex)
  tableNorm$Time <- factor(tableNorm$Time)
  
  imp <- mice(tableNorm)
  
  tableImp <- complete(imp)
  
  
  return(tableImp)   
  
}





#test ----


# table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
