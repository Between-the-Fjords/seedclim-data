#'Which taxa are winning and losing
#'@param turf turfID
#'@param year1 start year of comparison
#'@param year2 end year of comparison
#'@param rare what constitutes a rare species
#'@param relative proportion change needed not to be considered a generalist
#'@description best used in a loop

#'@export
#divide into 4 categories
#too rare: <4 subplots in both years
#extinct: falls to 0%
#losers: falls by more than 20% relative 
#generalists: changes by less than 20% relative
#winners: rises by more than 20% relative
#incommers: rises from 0%

winlose<-function(turf, year1, year2, rare=4, relative=.2){
  cover<-cover[,propertaxa]
  fsubturf<-fsubturf[,propertaxa]
  if(length(turf)==1) turf<-rep(turf,2)
  stopifnot(year1<=year2)
  cv<-rbind(cover[cover.meta$turfID==turf[1]&cover.meta$Year==year1,,drop=FALSE],cover[cover.meta$turfID==turf[2]&cover.meta$Year==year2,, drop=FALSE])
  fs<-rbind(fsubturf[cover.meta$turfID==turf[1]&cover.meta$Year==year1,,drop=FALSE],fsubturf[cover.meta$turfID==turf[2]&cover.meta$Year==year2,, drop=FALSE])
  
  #test
  if(FALSE){
    cv<-data.frame(absent=c(0,0), rare=c(10,10), extinct=c(10,0), looser=c(10,1), generalist=c(11,10),winner=c(5,10), immigrant=c(0,10))
    fs<-data.frame(absent=c(0,0), rare=c(3,3), extinct=c(10,0), looser=c(10,1), generalist=c(11,10),winner=c(5,10), immigrant=c(0,10))
    rare=4; relative=0.2
  }
  #remove absent species
  #fs<-fs[,colSums(cv)>0]
  cv<-cv[,colSums(cv)>0]
  rich<-sum(cv[1,]>0)
  
  #rare
  #rare<-names(cv)[apply(fs,2, function(sp)all(sp<rare))]
  
  #remove rare
  #fs<-fs[,!names(cv)%in%rare]
  #cv<-cv[,!names(cv)%in%rare]
  #crich<-ncol(cv)
  
  #extinct & immigrant
  extinct<-names(cv)[cv[2,]==0] 
  immigrant<-names(cv)[cv[1,]==0]
  
  #win loose generalist
  winner<-names(cv)[(cv[2,]/cv[1,])>1+relative&cv[1,]>0]
  losers<-names(cv)[(cv[2,]/cv[1,])<1-relative]
  generalist<-names(cv)[!names(cv)%in%c(winner,losers, immigrant)]
  
  if(nrow(cv)==1)list(rare=NA, extinct=NA, losers=NA, generalist=NA, winner=NA, immigrant=NA, rich=NA, Nextinct=NA, Nlosers=NA, Ngeneralist=NA, Nwinner=NA, Nimmigrant=NA)
  else list(rare=rare, extinct=extinct, losers=losers, generalist=generalist, winner=winner, immigrant=immigrant, rich=rich, Nextinct=length(extinct), Nlosers=length(losers), Ngeneralist=length(generalist), Nwinner=length(winner), Nimmigrant=length(immigrant))
}
