#'Taxomonic distance moved by turfs
#'@param x data.frame of cover or subturf frequencies  
#'
#'@description Finds Bray-Curtis distance between original community and the community in subsequent years.
#'@return matrix of distances travelled by each turf
#'\itemize{
#'  \item{d911 distance travelled between 2009 and 2011}
#'  \item{d912 distance travelled between 2009 and 2012}
#'  \item{d913 distance travelled between 2009 and 2013}
#'}

#'@export

make.dists<-function(x){
  res<-sapply(as.character(turfs$turfID),function(tu){   
    turf<-x[cover.meta$turfID==tu,propertaxa]
    res<-as.matrix(vegdist(turf))[-1,1]
    if(length(res)<3)res<-c(res,rep(NA, 3-length(res)))
    res
  })
  res<-t(res)
  if(ncol(res)>3){stop("More columns of data than expected. Need to edit code.")}
  colnames(res)<-c("d911","d912","d913")
  out<-cbind(turfs,res)
  class(out)<-c("distmoved","data.frame")
  out
}
