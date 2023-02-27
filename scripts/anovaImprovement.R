
library(dplyr)
library(reshape2)

table_ges <- read.csv("data/gesPlasmAnt.csv")[-47]

table_ges_grouped <- table_ges %>% rename(factore = factor) %>% mutate(imp=recode(imp, NO = "Original", SI = "Imputed")) #%>% dplyr::group_by(factore,imp) %>% summarise(ges=mean(ges))

ggplot(table_ges_grouped, aes(x = factore, y = ges, fill = factor(imp, levels = c("Original", "Imputed")))) + geom_boxplot() +
  ggtitle("Variation in generalized eta squared due to imputation")+
  labs(x = "Factors", fill = "Imputation")


longTable <- melt(table_ges, id = c("met", "factor", "imp"))

  
    ggplot(longTable, aes(factor(variable, 
                                 level = unique(longTable$variable)),as.numeric(value), 
                          fill=factor(longTable[,factore], 
                                      levels = c("Initial", "Final")))) +
      geom_boxplot()+
      ggtitle(paste("A.Time Flavanones-Plasma"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
      labs(y = "standarized value", x = "variables", fill = "Time")
    
# anova index ----
    
    
anova_index <- function(anova_results){
  
  anova_index_calc <- function(actual_value, past_value){
    anova_index_value <- ((1/(actual_value - past_value))^2)/100
    
  }
  
  apply(anova_results, MARGIN = 1, anova_index_calc)
  
  
  
  return <- list(anova_per_compound, anova_per_factor)
}
  
index_test <- read.csv("data/newIndexComparison.csv")[,-c(6,7)]    
    
index_long <- melt(index_test, id = c("compound", "imp"))

index_group <- index_long %>% dplyr::group_by(compound, variable) %>% mutate(substraction = value[imp == "Original"] - 
                                                                               value[imp == "Imputed"])
index_group$imp  

index_group <- index_long %>% dplyr::group_by(compound, variable) %>% nest(.key = "grupos") %>%
               mutate(dims = purrr::map(grupos, nrow)) %>% filter (dims == 2) %>% 
               mutate(substraction = purrr::map(grupos, substract)) %>% unnest(cols = substraction)

index_group$grupos[[2]][[2]][1] - index_group$grupos[[2]][[2]][2]

substract <- function(x){
  result <- x[[2]][1]-x[[2]][2]
}


index_group$grupos

#%>% purrr::map(data, Original - Imputed)
    


mutate(index_substract = data[[1]]-data[[2]])

index_group[[3]][[1]][[2]]
