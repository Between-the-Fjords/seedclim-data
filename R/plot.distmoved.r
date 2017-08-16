#'Boxplots of distance moved
#'@param x output of make.dists() cbinded onto turfs.
#'@param temperature Split the data by temperature or precipitation
#'@param \dots currently unused
#'@export

plot.distmoved<-function(x, temperature = TRUE, ...) {
  #ylim=c(0.06,.85) 
  ylim<-range(cbind(x$d911, x$d912, x$d913),na.rm=TRUE)
  
  par(mfrow=c(3,1), mar=c(3,1.5,1,1), mgp=c(1.5,.5,0), oma=c(0,2,0,0))
  if(temperature){
    at<-c(1:19)[-c(4,8,12, 16)]
    boxplot(d911~Temperature_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2011 by temperature level", ylim=ylim, col=rep(TT.colours,each=3), at=at)
    boxplot(d912~Temperature_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2012 by temperature level", ylim=ylim, col=rep(TT.colours,each=3), at=at)
    boxplot(d913~Temperature_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2013 by temperature level", ylim=ylim, col=rep(TT.colours,each=3), at=at)
    
  }else{
    at<-c(1:24)[-c(5,10,15, 20)]
    boxplot(d911~Precipitation_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2011 by precipitation level", ylim=ylim, col=rep(TT.colours,each=4),at=at)
    boxplot(d912~Precipitation_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2012 by precipitation level", ylim=ylim, col=rep(TT.colours,each=4),at=at)
    boxplot(d913~Precipitation_level+TTtreat, data=x, las=2, notch=TRUE, main="2009-2013 by precipitation level", ylim=ylim, col=rep(TT.colours,each=4),at=at)
  }
  title( ylab="Distance from Origin", outer=TRUE, line=.5)
}
