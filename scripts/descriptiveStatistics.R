# descriptive statistics
source("scripts/normalizingFactoring.R")
source("scripts/readingFilling.R")
library("psych")
library("modeest")
library(skimr)  

estadisticosDescriptivos <- function (tabla) {

  #Encabezados de cada estadístico como un vector
  nombres <- c("Mínimo", "Q1", "Media", "Media recortada", "Mediana", "Moda",
               "Varianza", "Desviación Estándar", "Q3", "Máximo", "Simetría", "Curtosis")
  
  descr2 <- data.frame(matrix(ncol = length(nombres), nrow = 0))
  
    
  for (i in colnames(tabla)) {
    if (is.numeric(tabla[, i]) & i != "numVol"){
    
    min <- min(tabla[, i], na.rm = TRUE)
    q1 <- quantile(tabla[, i], probs = 0.25, na.rm = TRUE)
    media <- mean.default(tabla[, i], na.rm = TRUE)
    media_rec <- mean.default(tabla[, i], trim = 0.025, na.rm = TRUE)
    mediana <- median.default(tabla[, i], na.rm = TRUE)
    moda <- mfv1(tabla[, i])
    var <- var(tabla[, i], na.rm = TRUE)
    desvest <- sd(tabla[, i], na.rm = TRUE)
    q3 <- quantile(tabla[, i], probs = 0.75, na.rm = TRUE)
    max <- max(tabla[, i], na.rm = TRUE)
    s <- skew(tabla[, i])
    c <- kurtosi(tabla[, i])
    
    
    #Valores de estadísticos como vector
    descriptivos <- as.numeric(c(min, q1, media, media_rec, mediana, moda,
                                 var, desvest, q3, max, s, c))

    descr2 <- as.data.frame(rbind(descr2, descriptivos))
    
    
    colnames(descr2) <- nombres
    

  }
  
    
  
  }
  
  rownames(descr2) <- colnames(table1.3)[!(colnames(table1.3) %in% c("numVol", "grouping","Sex", "Time", "Sweetener"))] 
  
  return (descr2)
  
}


#test ----

# 
# table1.0 <- readingFillingGrouping("data/chronicPlasmAnt.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# 
# descriptive <- estadisticosDescriptivos(table1.3)
# descriptive2 <- skim(table1.3)