runSQL <- function(con,Query){

  print(paste("Calling SQL Query:",Query))
  rs <- dbSendQuery(con, Query)
  tempsql <- fetch(rs,n=-1)
  dbClearResult(rs)
  return(tempsql);
}
