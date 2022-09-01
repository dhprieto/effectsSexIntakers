# Clustering

# other scripts

source("scripts/main.R")

# libraries

library(clValid)
library(mclust)
library(factoextra)
library(ggpubr)
#####


clusteringTest <- function(pathToTable){
  
  table1.4 <- mainPreproc(pathToTable, F)
  
  browser()
  comparisonTI <- clValid(
    obj        = table1.4 %>% filter(Time == "Initial") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)) %>% 
                 mutate_if(is.double,as.numeric),
    nClust     = 2:6,
    clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
    validation = c("stability", "internal")
  )
  
  comparisonTF <- clValid(
    obj        = table1.4 %>% filter(Time == "Final") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)) %>% 
                 mutate_if(is.double,as.numeric),
    nClust     = 2:6,
    clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
    validation = c("stability", "internal")
  )
  
  
  ### model-based clustering
  
  tabla1.5 <- table1.4 %>% filter(Time == "Initial") %>% select(-c(grouping, Sweetener, Sex, numVol, Time) %>% 
                                                                  mutate_if(is.double,as.numeric))
  
  model_clustering_OF <- Mclust(tabla1.5)
  
  p1I <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                    title = paste("Model Selection ", pathToTable, "Initial Time")) + scale_x_discrete(limits = c(1:10))
  p2I <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                    title = paste("Clusters Plot ", pathToTable, "Initial Time") , pallete = "jco")

  tabla1.5 <- table1.4 %>% filter(Time == "Final") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)) %>% 
    mutate_if(is.double,as.numeric)
  
  model_clustering_OF <- Mclust(tabla1.5)
  
  p1F <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                    title = paste("Model Selection ", pathToTable, "Final Time")) + scale_x_discrete(limits = c(1:10))
  p2F <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                    title = paste("Clusters Plot ", pathToTable, "Final Time"), pallete = "jco")
  
  return(list(comparisonTI, comparisonTF, p1I, p2I, p1F, p2F))
}

plasmFlavClustering <- clusteringTest("data/chronicPlasmFlav.csv")
plasmAntClustering <- clusteringTest("data/chronicPlasmAnt.csv")
urineFlavClustering <- clusteringTest("data/chronicUrineFlav.csv")
urineAntClustering <- clusteringTest("data/chronicUrineAnt.csv")

tabla2.0 <- mainPreproc("data/chronicUrineAnt.csv", F)
tabla2.1 <- mainPreproc("data/chronicUrineFlav.csv")

tabla2.0.1 <- tabla2.0 %>% filter(Time == "Initial") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)) %>% mutate_if(is.double,as.numeric)

ggarrange(plasmFlavClustering[[5]],plasmFlavClustering[[6]])













### pruebas ----
table1.4 <- mainPreproc("data/chronicPlasmAnt.csv", F)

comparisonTI <- clValid(
  obj        = table1.4 %>% filter(Time == "Initial") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)),
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparisonTI)


comparisonTF <- clValid(
  obj        = table1.4 %>% filter(Time == "Final") %>% select(-c(grouping, Sweetener, Sex, numVol, Time)),
  nClust     = 2:6,
  clMethods  = c( "hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara","agnes"),
  validation = c("stability", "internal")
)

summary(comparisonTF)


tabla1.5 <- table1.4 %>% filter(Time == "Final") %>% select(-c(grouping, Sweetener, Sex, numVol, Time))

model_clustering_OF <- Mclust(tabla1.5)

p1F <- fviz_mclust(object = model_clustering_OF, what = "BIC", pallete = "jco",  
                  title = "Model Selection Orina Flav") + scale_x_discrete(limits = c(1:10))
p2F <- fviz_mclust(model_clustering_OF, what = "classification", geom = "point",
                  title = "Clusters Plot Orina Flav", pallete = "jco")
ggarrange(p1,p2)
