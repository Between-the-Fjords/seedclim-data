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
  source("inst/importSource/loadCover.r")
  source("inst/importSource/loadSubplotfreq.r")
  source("inst/importSource/loadOtherData.r", encoding="UTF-8")
  if(moss){
    source("inst/importSource/loadBryophytes.r")
  }
  source("inst/importSource/seedclimColours.r")
  
  if(closeCon){
    dbDisconnect(con)
  }

}
