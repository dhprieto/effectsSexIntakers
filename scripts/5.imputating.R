# imputating


factoringImputating <- function(tableNorm){
  
  tableNorm$Sweetener <- factor(tableNorm$Sweetener) 
  tableNorm$Sex <- factor(tableNorm$Sex)
  tableNorm$Time <- factor(tableNorm$Time)
  
  imp <- mice(tableNorm, method = "rf", m = 10, printFlag = F, remove_collinear = F)
  
  tableImp <- complete(imp)
  
  
  return(list(imp,tableImp))   
  
}