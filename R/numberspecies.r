
#'Main winners and loosers plot
#'@param x result from winlose or trait selector (processed)
#'@param ylab y-axis label
#'@param main title
#'@param show turfs aggregated by "p" preciptiation and or "t" temperature levels
#'@param final result from findfinal or findfinaltrait
#'@param years which year are represented by the data
#'@param above plot the title above or below the plot border
#'@param inarow are the plots all in a row (gives yaxis labels only on first plot)
#'@param bottomrow if inarow, is this the bottomrow? If so, give axis labels etc.
#'@export

#main win lose plot
numberspecies<-function(x, ylab=NULL, main=NULL, show="p", final, years=2:4, above=TRUE, inarow=FALSE, bottomrow=FALSE){
  off=0.3
  if(inarow){
    oma<-c(2.6,2.6,0,.3)
    if(!all(par()$oma==oma)){
      par(oma=oma,mar=c(0,0,2,.3), xpd=NA)  
    }
  }else{
    par(mar=c(2,2.6,ifelse(above,1.5,.75),.3))
  }
  treats<-c("Control","Local", "Warmer", "Wetter", "Warmer & wetter")
  addfinal<-!missing(final)
  #pcols=c("cyan", "deepskyblue", "blue", "blue4")
  pcols<-rep("grey80",4)

  dat<-cbind(x, turfs)
  xlim=range(years)
  ylim=range(x, na.rm=TRUE)
  if(addfinal){
    dat<-cbind(dat, final=final)
    xlim=range(c(years,max(years)+off))
    ylim=range(c(x,final), na.rm=TRUE)
  }
  yaxt="s"
  
  by(dat, turfs$TTtreat, function(Y){   
    drawline<-function(variable,level,col, lty=1){lines(years,colMeans(Y[Y[,variable]==level,1:ncol(x)], na.rm=TRUE), lwd=2, col=col)}
    drawpoints<-function(variable,level,col, offset){ points(max(years)+off*offset,mean(Y$final[Y[,variable]==level], na.rm=TRUE), pch=16, col=col)}
    
    matplot(years,t(Y[,1:ncol(x)]), type="l", ylim=ylim,xlim=xlim, xaxt="n", xlab="", ylab="", yaxt="n",main="", lty=1, col=pcols[Y$Precipitation_level])
    title(main=treats[(levels(turfs$TTtreat)== Y$TTtreat[1])], line=ifelse(above, 0.2, -1))
    if(inarow&Y$TTtreat[1]=="TTC"){
      title(ylab=ylab, outer=FALSE)
      axis(2, outer=FALSE)
    }
    if(addfinal){
      points(rep(max(years)+off, nrow(Y)), Y$final, pch=20, cex=0.5, col="grey80")
    }
    if("p"%in%show){
      mapply(drawline,variable="Precipitation_level", level=1:4, col=ppt.colours)
    }
    if("t"%in%show){
      lty="62"
      mapply(drawline,variable="Temperature_level", lty=lty, level=1:3, col=tmp.colours)
    }
    if(addfinal){
      if("p"%in%show){
        mapply(drawpoints,variable="Precipitation_level", offset=0.9, level=1:4, col=ppt.colours)
      }
      if("t"%in%show){
        mapply(drawpoints,variable="Temperature_level", offset=1.1, level=1:3, col=tmp.colours)
      }
    }
    if(Y$TTtreat[1]=="TTC"){
      title(main=main, outer=FALSE,adj=0)
    }
    if(bottomrow|!inarow){
      if(addfinal){
        axis(side=1, at=c(years,max(years)+off), labels= c(years, ""))
        axis(side=1, at=c(years,max(years)+off), labels= c(rep("", length(years)), expression(infinity)), cex.axis=1.4)
        
      }else{
        axis(side=1, at=years, labels=years)
      }
    }
    if(bottomrow&inarow&Y$newTT[1]=="control"){
      title(xlab="Years after transplant", outer=TRUE)
    }
  
  
  })
}
