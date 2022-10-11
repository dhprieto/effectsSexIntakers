## Script for the pairwise (maybe called threewise) analysis


pairwiseTTest <- function(tabla){
  
  
  # remover no duplicados
  
  if (deparse(substitute(tabla)) == "urineAnt"){
  counts <- data.frame(table(tabla$numVol))
  tabla <- tabla[tabla$numVol %in% counts$Var1[counts$Freq > 1],]
  }
  for (i in colnames(tabla)){
    
    if (is.numeric(tabla[,i])){
      
      
      message(paste("Analized variable: ", i))
      
      message(paste("Time comparisons ", i)) 
      
      print(pairwise_t_test(data = tabla , formula = as.formula(paste(sym(i),"~ Time")),
                            paired = T, p.adjust.method = "bonferroni")
            %>% dplyr::select(-df, -statistic))
      
      tablaGr <- group_by(tabla, Sweetener, Sex)
      
      message(paste("Time-Sweetener-Sex comparisons", i))
      
      print(pairwise_t_test(data = tablaGr , formula = as.formula(paste(sym(i),"~ Time")),
                            paired = T, p.adjust.method = "bonferroni") %>% dplyr::select(-df, -statistic))
      
      
      tablaGr <- group_by(tabla, Sweetener)
      
      message(paste("Time-Sweetener comparisons", i))
      
      print(pairwise_t_test(data = tablaGr , formula = as.formula(paste(sym(i),"~ Time")),
                            paired = T, p.adjust.method = "bonferroni") %>% dplyr::select(-df, -statistic))
      
      
      tablaGr <- group_by(tabla, Sex)
      
      message(paste("Time-Sex comparisons", i))
      
      print(pairwise_t_test(data = tablaGr , formula = as.formula(paste(sym(i),"~ Time")),
                            paired = T, p.adjust.method = "bonferroni") %>% dplyr::select(-df, -statistic))
    }
  }
}


# test
# 
# table1.0 <- readingFillingGrouping("data/chronicUrineFlav.csv")
# table1.1 <- anthroSex(table1.0)
# table1.2 <- normalizingNumeric(table1.1)
# table1.3 <- factoringImputating(table1.2)
# table1.4 <- timingCleanFeatures(table1.3)
# 
# pairwiseTTest(table1.4)
# 
# 
# pairwiseTTest(plasmAnt)
# pairwiseTTest(plasmFlav)
# pairwiseTTest(urineAnt)
# pairwiseTTest(urineFlav)
