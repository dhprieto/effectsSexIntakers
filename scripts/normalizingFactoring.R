# exploring the data and variables

source("scripts/readingFilling.R")
library(scales)

table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")

# normalizing

normalizingNumeric <- function(tableComplete) {
  
  for (i in colnames(tableComplete)){
   
    if (is.numeric(tableComplete[,i]) && tableComplete[,i] != "numVol"){
      print("hecho")
      print(i)
      tableComplete[,i] <- rescale(tableComplete[,i])
      
    }
     
  }
return (tableComplete)    
}

table1.1 <- normalizingNumeric(table1.0)


table1.0$DHPAA.di.Sulfate.1
