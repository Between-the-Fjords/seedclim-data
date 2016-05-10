#'Find cover of species with trait 
#'@param usecover Use cover or no subplots.
#'@param keep logical vector of which species to include
#'@export
traitselector<-function(usecover=TRUE, keep){
  if(usecover){dat<-cover}
  else{dat<-fsubturf}
  dat<-dat[,alltaxa]
  keep[is.na(keep)]<-FALSE
  dat<-dat[,keep]
  
  sapply(as.character(turfs$turfID), function(tu){  # browser()
    sapply(c(2009, 2011, 2012, 2013),function(y){# browser()
      r<-dat[cover.meta$turfID==tu&cover.meta$Year==y,]
      if(nrow(r)>0){
        sum(r, na.rm=TRUE)
      }else{NA}
    })
  })
}
