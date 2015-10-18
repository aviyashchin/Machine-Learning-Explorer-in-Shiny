impute.na <- function(x){
  return (sapply(x, function(f){is.na(f)<-which(f == '');f}))
}


col.mean<- function(x){
  return (sapply(x, function(f) mean(f)))
}




# idata <- titanic3
# library(caret)
# 
# sum(is.na(idata))
# 
# titanic.processed = data.frame(sapply(idata, function(x) impute.na(x)))
# 
# 
# titanic.bool = data.frame(sapply(idata, function(x) ifelse(class(x) == "factor",TRUE, FALSE)))
# 
# 
# col.check= data.frame(sapply(idata, function(x) impute.na(x)))
# 
# 
# missing = sapply(titanic.processed, function(x) mean(x, na.rm = TRUE))
# missing


