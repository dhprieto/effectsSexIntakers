#### boxplots tendencia anovas

boxplotBias <- function(vars, pathToTable, factore, removeOutliers = F ){
  
  # reading table
  
  table1.0 <- mainPreproc(pathToTable, F)
  
  table1.1 <- table1.0 %>% select (all_of(vars), Time, Sex, Sweetener, -c(numVol, grouping))
  
  
  
  if (removeOutliers){
    
    for (i in colnames(table1.1)) {
      
      if (is.numeric(table1.1[,i])){
        
        table1.1 <- table1.1[!table1.1[, i] %in% boxplot.stats(table1.1[,i])$out,]
        
      }
    } 
    
    # return(table1.1)
    
  }
  

  # long table format
  
  table1.2 <- melt(table1.1, id = c("Time", "Sex", "Sweetener"))

  # plot factors
  
  bxp <- function(longTable, factore){

    
    
    if (factore == "Time") {
      
      ggplot(longTable, aes(factor(variable, 
                                   level = unique(longTable$variable)),as.numeric(value), 
                            fill=factor(longTable[,factore], 
                                        levels = c("Initial", "Final")))) +
        geom_boxplot()+
        ggtitle(paste("A.Time Flavanones-Plasma"))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
        labs(y = "standarized value", x = "variables", fill = "Time")
      
    }
    
    else if (factore == "Sex") {
      ggplot(longTable, aes(factor(variable, 
                                   level = unique(longTable$variable)),as.numeric(value), 
                            fill=factor(longTable[,factore]))) +
        geom_boxplot()+
        ggtitle(paste("B. Sex:Time Anthocyanin-Urine"))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
        labs(y = "standarized value", x = "variables", fill = "Sex")+
        scale_fill_brewer(palette = "Reds")+  
        facet_wrap(~factor(Time, levels = c("Initial", "Final")))
      
      
    }
    
    else{
      ggplot(longTable, aes(factor(variable, 
                                   level = unique(longTable$variable)),as.numeric(value), 
                            fill=factor(longTable[,factore])), colour = "Sweetener") +
        geom_boxplot()+
        ggtitle(paste("B. Sweetener:Time Flavanones-urine"))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
        labs(y = "standarized value", x = "variables", fill = "Sweetener")+
        facet_wrap(~factor(Time, levels = c("Initial", "Final")))
      
      
    }
  }
  

  bxp(table1.2, factore)

}


# test

# boxplotBias(vars = c("Total.CA","DHPAA.di.Sulfate", "TFA.Gluc", "TFA.Sulfate", "Total.TFA"), 
#             pathToTable = "data/chronicUrineAnt.csv", 
#             factore = "Sweetener",
#             removeOutliers = F)

