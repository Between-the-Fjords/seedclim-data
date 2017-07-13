#'Get Turf Community
#'@description Get names of species in turf community in given year(s)
#'@param tu TurfID
#'@param year years to include
#'@return character vector of species codes. "MISSING" is returned if no species are found (for example in a destroyed turf) 
#'@export

get.turf.community<-function(tu, year=2009:2013){
  x<-cover[cover.meta$turfID==tu&cover.meta$Year%in%year,]
  if(nrow(x)>0){
    x<-colSums(x)
    return(names(x[x>0]))
  }else{
    return("MISSING")
  }
}