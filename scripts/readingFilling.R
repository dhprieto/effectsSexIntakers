# reading and filling of tables

# library load

library(tidyverse)

# reading table

readingFillingGrouping <- function(tablaPath){
  tabla <- read.csv(tablaPath, sep = ";", dec = ".")
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



