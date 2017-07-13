#' Makes connection to MYSQL database
#' 
#' @param username is the username
#' @param password is password
#' @param host is host
#' @param port is port
#' @details Use dbDisconnect(con) to close connection
#' @return Connection to database
#' @examples
#' \dontrun{make_connection()}


#' @export
#' @importFrom RODBC odbcConnectAccess2007 odbcConnect


make_connection<-function(username = "", password = "", host = "127.0.0.1", port = 3306){
  con <- dbConnect (RMySQL::MySQL(),
                    username = username,
                    password = password,
                    host = host,
                    port = port,
                    dbname = "seedclimComm")
  con
  #
}
