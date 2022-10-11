# script anovas ----

library(rstatix)

# Función para realizar la anova de tres vías sobre una variable
# Imprime por pantalla el resultado

aov_test <- function(tabla, variable){
  
  tablaVar <- tabla %>% select(numVol, Sweetener, Sex, Time, variable)

  # tablaVar <- tablaVar[!tablaVar[[5]] %in% boxplot.stats(tablaVar[[5]])$out,]
   
  res.aov <- anova_test(data = ungroup(tablaVar), dv=variable, wid=numVol, 
                        between = c(Sex, Sweetener), within= Time)
  
  tablaAnova <- get_anova_table(res.aov, correction = "auto")
  
  print(tablaAnova)
}

# Función para hacer en bucle el análisis anova a lo largo de una tabla

aov_loop <- function(tabla){
  
  message(paste("Tabla analizada: ", deparse(substitute(tabla))))
  for (i in colnames(tabla)[-1]){

    if (is.numeric(tabla[,i])){
    
      message(paste("Variable analizada: ", i))
      aov_test(tabla,i)
    }
    
    
  }
}

# test ----

# table1.0 <- readingFillingGrouping("data/chronicUrineAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# table1.4 <- timingCleanFeatures(table1.3)

