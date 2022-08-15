# reading and filling of tables

# library load

library(tidyverse)

# reading table

readingFillingGrouping <- function(tablaPath){
  tabla <- read.csv(tablaPath, sep = ";", dec = ".")
  
  # remove raw data from spectometry
  
  for (i in colnames(tabla)){
    if(substr(i, nchar(i), nchar(i)) != "1" & i != "grouping"){
      tabla[,i] <- NULL
    }
  }
  
  # adding sweetener
  
  for (i in seq(1, nrow(tabla))){
    
    if (grepl(pattern = "A", x = tabla$grouping[i])){
      tabla$Sweetener[i] <- "ST"
    }
    
    else if (grepl(pattern = "B", x = tabla$grouping[i])){
      tabla$Sweetener[i] <- "SU"
    }
    
    else if (grepl(pattern = "C", x = tabla$grouping[i])){
      tabla$Sweetener[i] <- "SA"
    }
  
    # adding numVol
    
    if (length(tabla$grouping[i]) == 4){
    tabla$numVol[i] <- as.numeric(substr(tabla$grouping[i],0,1))
    if (tabla$Sweetener[i] == "SU"){
      tabla$numVol[i] = tabla$numVol[i] + 50
    }
    
    else if (tabla$Sweetener[i] == "SA"){
      tabla$numVol[i] = tabla$numVol[i] + 100
      }
    
    }
    
    else {
    tabla$numVol[i] <- as.numeric(substr(tabla$grouping[i],0,2))
    
    if (tabla$Sweetener[i] == "SU"){
      tabla$numVol[i] = tabla$numVol[i] + 50
    }
    
    else if (tabla$Sweetener[i] == "SA"){
      tabla$numVol[i] = tabla$numVol[i] + 100
    }
    
    }
    
    if (grepl(pattern = "F", x = tabla$grouping[i])){
      tabla$Time[i] <- "Final"
    }
    else if (grepl(pattern = "([A-C])0", x = tabla$grouping[i])){
      tabla$Time[i] <- "Initial"
      
    }
    
      
    }    
  

return(tabla)}


# adding anthro/cardiovascular values and sexes

anthroSex <- function(tableRFG){
  
  tableAnthro <- read.csv("data/chronicAnthropometricCardiovascularData.csv", sep = ";", dec = ",")
  tableSex <- read.csv("data/chronicSexVolunteers.csv", sep = ";", dec = ",")

  tableAnthroSex <- merge(tableAnthro, tableSex, by = "numVol")
  
  tableComplete <- merge(tableRFG, tableAnthroSex, by = "numVol")
  return(tableComplete)
}

# test ----

#table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
#table1.1 <- anthroSex(table1.0)

