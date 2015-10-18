impute.na <- function(x){
  return (sapply(x, function(f){is.na(f)<-which(f == '');f}))
}
