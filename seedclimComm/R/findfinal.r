#' Find target number of species
#'@description Number of species needed to immigrate/go extinct to match target control
#'@param extinct extinct or immigrant
#'@export


findfinal<-function(extinct=TRUE){
  sapply(as.character(turfs$turfID), function(tu){  
    #find turfID of TT1 turf in destination block
    destID<-find.dest.control(tu)
    missingControl<-FALSE
    #find number of extinctions/immigrations needed for tu to match dest TT1 in 2013
    initial<-with(cover.meta,cover[turfID==tu&Year==2009,])
    target<- with(cover.meta,cover[turfID==destID["TT1"]&Year==2013,]) 
    if(nrow(target)==0){print("zero")#replace TT1 with TTC when TT1 missing
      target<- with(cover.meta,cover[turfID==destID["TTC"]&Year==2013,]) 
      missingControl<-TRUE
    }
    if(extinct){
      res<-sum(initial>0&target==0)
    }else{
      res<-sum(initial==0&target>0)
    }
    if(turfs$TTtreat[turfs$turfID==tu]=="TT1") res<-NA
    if(turfs$TTtreat[turfs$turfID==tu]=="TTC"&missingControl) res<-NA
    
    res
  })
  
}


