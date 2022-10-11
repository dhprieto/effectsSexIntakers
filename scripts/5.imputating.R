# imputating


factoringImputating <- function(tableNorm){
  
  tableNorm$Sweetener <- factor(tableNorm$Sweetener) 
  tableNorm$Sex <- factor(tableNorm$Sex)
  tableNorm$Time <- factor(tableNorm$Time)
  
  imp <- mice(tableNorm)
  
  tableImp <- complete(imp)
  
  
  return(tableImp)   
  
}