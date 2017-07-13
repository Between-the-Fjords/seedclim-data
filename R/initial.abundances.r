initial.abundances<-function(lis, year=2009, use.cover=TRUE){
  if(use.cover){
    dat<-cover
  }else{
    dat<-fsubturf
  }
  res<-mapply(function(tu, spp){dat[cover.meta$Year==year&cover.meta$turfID==tu,spp]}, spp=lis, tu=names(lis), SIMPLIFY=FALSE)
  unlist(res)
}
