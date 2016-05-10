#'Directional Taxomonic Distance Moved By Turfs
#'@param X data.frame of cover or subturf frequencies  
#'
#'@description Finds Bray-Curtis distance moved by turf from its between original community towards its target community.
#'Uses destination TT1 turf as the target. If TT1 turf is missing, no value is given.
#'Relative distance show distance as a proportion of the original distance between turf community and the destination TT1 turf.
#'The all distances are to the TT1 community in the relevant year - i.e. to the current target, not the initial target.
#'@return matrix of distances travelled by each turf
#'\itemize{
#'  \item{d11 distance travelled towards target between 2009 and 2011}
#'  \item{d12 distance travelled towards target between 2009 and 2012}
#'  \item{d13 distance travelled towards target between 2009 and 2013}
#'  \item{r11 relative distance travelled towards target between 2009 and 2011}
#'  \item{r12 relative distance travelled towards target between 2009 and 2012}
#'  \item{r13 relative distance travelled towards target between 2009 and 2013}

#'}

#'@export


make.directional.dists<-function(X){
  if(max(cover.meta$Year)>2013){ stop("More years of data than expected. Edit code.")}
  res<-apply(turfs[turfs$TTtreat!="TT1",],1,function(x){  
    X<-X[,propertaxa]
    ttx<-X[cover.meta$TTtreat==x["TTtreat"]&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2009,, drop=FALSE]
    tt1<-X[cover.meta$TTtreat=="TT1"&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2009,, drop=FALSE]
    d09<-vegdist(rbind(ttx,tt1))[1]
    
    ttx<-X[cover.meta$TTtreat==x["TTtreat"]&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2011,, drop=FALSE]
    tt1<-X[cover.meta$TTtreat=="TT1"&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2011,, drop=FALSE]
    d11<-vegdist(rbind(ttx,tt1))[1]
    
    ttx<-X[cover.meta$TTtreat==x["TTtreat"]&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2012,, drop=FALSE]
    tt1<-X[cover.meta$TTtreat=="TT1"&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2012,, drop=FALSE]
    d12<-vegdist(rbind(ttx,tt1))[1]
    
    ttx<-X[cover.meta$TTtreat==x["TTtreat"]&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2013,, drop=FALSE]
    tt1<-X[cover.meta$TTtreat=="TT1"&cover.meta$destBlockID==x["destBlockID"]&cover.meta$Year==2013,, drop=FALSE]
    d13<-vegdist(rbind(ttx,tt1))[1]
    
    c(d11=d09-d11, d12=d09-d12, d13=d09-d13, r11=d11/d09, r12=d12/d09, r13=d13/d09,od=d09)
    
  })
  out<-cbind(turfs[turfs$TTtreat!="TT1",],t(res))
  class(out)<-c("towards.target","data.frame")
  out
}
