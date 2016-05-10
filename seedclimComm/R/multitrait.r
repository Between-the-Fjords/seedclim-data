#' plot risk of extinction for different treatments
#' @param trait which trait
#' @param char which value of character traits
#' @param numeric is the trait numeric?
#' @param type which contrast to show: extinction, immigrant or persistant
#' @return multitrait object

#'@export
multitraits<-function(trait, char=NULL, numeric, type="extinction"){
  t2<-trait.extinction.plot(levels(turfs$siteID),treatment="TT2",char=char, trait=trait, numeric=numeric, plot=FALSE, type=type)
  t3<-trait.extinction.plot(levels(turfs$siteID),treatment="TT3",char=char, trait=trait, numeric=numeric, plot=FALSE, type=type)
  t4<-trait.extinction.plot(levels(turfs$siteID),treatment="TT4",char=char, trait=trait, numeric=numeric, plot=FALSE, type=type)
  
  res<-list(
    TT2=t2[,2]-t2[,1],
    TT3=t3[,2]-t3[,1],
    TT4=t4[,2]-t4[,1]
  )
  class(res)<-"multitrait"
  return(res)
}
