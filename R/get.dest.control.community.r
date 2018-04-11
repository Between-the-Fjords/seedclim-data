#'Get destination control community
#'@description Get the species list of the destination control community
#'@param tu turfID
#'@param TT1 get species list of TT1 turf 
#'@param TTC get species list of TTC turf
#'@param year which years to use
#'@description Using TT1=TRUE and TTC=TRUE and year=2009:2013 gets the maximal list of species
#'@export 
#'

get.dest.control.community<-function(tu,TT1=TRUE, TTC=TRUE, year=2009:2013){
  dest.control<-find.dest.control(tu)
  dest.control.com<-unlist(lapply(dest.control[c(TT1,TTC)],get.turf.community, year=year)) 
  return(dest.control.com)
}
