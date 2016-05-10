#' Imports Community data from source files
#' 
#' @param db The path to the database or DNS
#' @param isDNS Is db a DNS?
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


load_community_data<-function(db = "D:/downloadeddata/seedclim_2014-12-19.mdb", isDNS=FALSE, moss = FALSE){
  if(version$arch!="i386"){stop("Need to use 32 bit version of R for RODBC package")}
  if(isDNS){  
    con<-odbcConnect(db)
  }else{
    con<-odbcConnectAccess2007(db)
  }
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
