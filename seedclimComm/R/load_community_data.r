#' Imports Community data from source files
#' 
#' @param con connection to database
#' @param moss Load bryophyte data?
#' @param closeCon Close connection after loading data
#' @description Runs source files to load community data into memory. 
#' @details use make_connection() to get connection to database
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
#' @importFrom BDI dbDisconnect


load_community_data<-function(con, closeCon = TRUE, moss = FALSE){
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadCover.r"))
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadSubplotfreq.r"))
  source(file.path(system.file(package = "seedclimComm"), "importSource", "loadOtherData.r"),encoding="UTF-8")
  if(moss){
    source(file.path(system.file(package = "seedclimComm"), "importSource", "loadBryophytes.r"))
  }
  source(file.path(system.file(package = "seedclimComm"), "importSource", "seedclimColours.r"))
  
  if(closeCon){
    dbDisconnect(con)
  }

}
