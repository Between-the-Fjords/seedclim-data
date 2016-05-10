#'Find target cover of trait
#'@description Find cover of species with trait in 2013 in destination control
#'@param usecover Use cover or no subplots.
#'@param keep logical vector of which species to include
#'@export
findfinaltrait<-function(usecover=TRUE, keep){
  if(usecover){dat<-cover}
  else{dat<-fsubturf}
  dat<-dat[,alltaxa]
  keep[is.na(keep)]<-FALSE
  dat<-dat[,keep]
  
  sapply(as.character(turfs$turfID), function(tu){  
    #find turfID of TT1 turf in destination block
    destID<-find.dest.control(tu)
    missingControl<-FALSE
    
    #find trait value needed to for tu to match dest TT1 in 2013
    final<-dat[cover.meta$turfID==destID["TT1"]&cover.meta$Year==2013,]
    if(nrow(final)==0){print("zero")
      #find trait value needed to for tu to match dest TT1 in 2013
      final<-dat[cover.meta$turfID==destID["TTC"]&cover.meta$Year==2013,]
      missingControl<-TRUE                 
    }
    
    
    res<-sum(final)
    
    if(turfs$TTtreat[turfs$turfID==tu]=="TT1") res<-NA
    if(turfs$TTtreat[turfs$turfID==tu]=="TTC"&missingControl) res<-NA
    
    res
  })
  
}
