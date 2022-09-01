### MAIN SCRIPT ----

source("scripts/readingFilling.R")
source("scripts/normalizingFactoring.R")
source("scripts/descriptiveStatistics.R")
source("scripts/variablesAnalysis.R")

# test -----

mainPreproc <- function(pathToTable, anthro = T){

              table1.0 <- readingFillingGrouping(pathToTable)
              table1.1 <- anthroSex(table1.0)
              table1.2 <- normalizingNumeric(table1.1)
              table1.3 <- factoringImputating(table1.2)
              table1.4 <- timingCleanFeatures(table1.3, pathToTable)

              if (anthro == FALSE){
                
                table1.4 <- table1.4 %>% select (-c(Weight,BMI,Fat, CVRI,Bpmin, Bpmax, Frec))
                
              }
          return (table1.4)
}



# test ----
# plasmAnt <- mainPreproc("data/chronicPlasmAnt.csv", FALSE)
# plasmFlav <- mainPreproc("data/chronicPlasmFlav.csv" , FALSE)
# urineAnt <- mainPreproc("data/chronicUrineAnt.csv", FALSE)
# urineFlav <- mainPreproc("data/chronicUrineFlav.csv", FALSE)
# 
# 
# table0.1 <- read.csv("data/chronicUrineAnt.csv", sep=";",dec="." )
# 
# table1.0 <- readingFillingGrouping("data/chronicUrineAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# table1.4 <- timingCleanFeatures(table1.3)



