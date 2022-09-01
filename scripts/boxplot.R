
#### boxplots tendencia anovas

## carga librerías y scripts

source("scripts/main.R")

library(reshape2)

boxplotBias <- function(vars, pathToTable, factore){
  
  # reading table
  
  table1.0 <- mainPreproc(pathToTable, F)
  
  table1.1 <- table1.0 %>% select (-vars, -c(numVol, grouping))
  
  
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
        labs(y = "standarized value", x = "variables", fill = "Sweetener")+
        facet_wrap(~factor(Time, levels = c("Initial", "Final")))
      
      
    }
  }
  

  bxp(table1.2, factore)

}

colnames(table1.0)

boxplotBias(c("TOTAL.CA.1","DHPAA.di.Sulfate.1", "TFA.Gluc.1", "TFA.Sulfate.1", "TOTAL.TFA.1"), 
            "data/chronicUrineAnt.csv", "Time")

# scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+