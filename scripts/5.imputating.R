# imputating


factoringImputating <- function(tableNorm){
  
  tableNorm$Sweetener <- factor(tableNorm$Sweetener) 
  tableNorm$Sex <- factor(tableNorm$Sex)
  tableNorm$Time <- factor(tableNorm$Time)
  
  imp <- mice(tableNorm, method = "rf", m = 10)
  
  tableImp <- complete(imp)
  
  
  return(list(imp,tableImp))   
  
}