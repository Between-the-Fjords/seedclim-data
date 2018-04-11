#'Find cover of extinct or immigrant species over time
#'@param usecover use cover of no subplots
#'@param immigration use immigration or extinction
#'@details Immigrants are species no present in 2009. Extinct are not present in 2013.
#'
#'@export

exim<-function(usecover=TRUE, immigration=TRUE){
  if(usecover){dat<-cover}
  else{dat<-fsubturf}
  dat<-dat[,noNIDseedlings]
  if(immigration){year0<-2009;years<-2011:2013}
  else{year0<-2013;years<-c(2009, 2011, 2012)}
  sapply(as.character(turfs$turfID), function(tu){  # browser()
    c0<-dat[cover.meta$turfID==tu&cover.meta$Year==year0,]
    if(nrow(c0)>0){
      sapply(years, function(y){        #    browser()
        cx<-dat[cover.meta$turfID==tu&cover.meta$Year==y,]
        sum(cx[c0==0])
      })
    } else{rep(NA, length(years))}  
  })
}
