### MAIN SCRIPT ----

source("scripts/readingFilling.R")
source("scripts/normalizingFactoring.R")
source("scripts/descriptiveStatistics.R")
source("scripts/variablesAnalysis.R")

# test -----
table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
table1.1 <- anthroSex(table1.0)
table1.2 <- normalizingNumeric(table1.1)
table1.3 <- factoringImputating(table1.2)
table1.4 <- timingCleanFeatures(table1.3)
