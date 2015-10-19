getConnectionGoogleMySql <- function() {
  ################################################################################################################################
  #
  # Get Connections to Google Account
  #
  ################################################################################################################################
  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(max.con = 1), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(max.con = 1), user="root" , password="uLFZ2WoB" , dbname="test" , host="130.211.154.93")
  }
  return(.connection)
}