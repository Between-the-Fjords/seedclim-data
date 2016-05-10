#' Imports Community data from source files
#' 
#' @param username is the username
#' @param password is password
#' @param host is host
#' @param port is port
#' @param moss Load bryophyte data?
#' @description Runs source files to load community data into memory. 
#' @details Uses RODBC so only works with 32 bit version of windows.
#' Currently loads
#' \itemize{
#'   \item{Cover data}
#'   \item{Subturf frequency data}
#'   \item{Trait data}
#'   \item{Turf level environmet data}
#'   \item{Bryophyte subturf frequency data (optional)}
#'   \item{seedClim colours and symbols (not yet finalised)}
#' }
#' @return Loads data into memory. No return value
#' @examples
#' \dontrun{load_community_data()}


#' @export
#' @importFrom RODBC odbcConnectAccess2007 odbcConnect


load_community_data<-function(username = "", password = "", host = "127.0.0.1", port = 3306, moss = FALSE){
  con <- dbConnect (RMySQL::MySQL(),
                    username = username,
                    password = password,
                    host = host,
                    port = port,
                    dbname = "seedclimComm")
  con<<-con#ugly hack otherwise not found in scope
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadCover.r"))
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadSubplotfreq.r"))
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadOtherData.r"),encoding="UTF-8")
  if(moss){
    source(file.path(system.file(package = "seedclimComm"), "importSource", "loadBryophytes.r"))
  }
  source(file.path(system.file(package = "seedclimComm"), "importSource", "seedclimColours.r"))
  
  close(con)

}
