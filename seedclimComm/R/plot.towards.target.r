#'Boxplots of directional distance moved
#'@param x output of directional distance moved cbind() to turfs 
#'@param split Split the data by temperature/precipitation?
#'@param temperature If split == TRUE, make the split by temperature or precipitation 
#'@param draw which years to plot
#'@param notch logical add notches
#'@param addmain add main title to plot
#'@param setpar let function set graphical parameters
#'@param \dots currently unused
#'@export 

plot.towards.target<-function(x, split=TRUE, temperature=TRUE, draw=2011:2013,notch=TRUE, addmain=TRUE, setpar=TRUE,...){
  x$TTtreat<-factor(x$TTtreat)
  TT.colours=TT.colours[-2]
  ylim<-range(cbind(x$d11, x$d12, x$d13), na.rm=TRUE)
  if(setpar)par(mfrow=c(length(draw),1), mar=c(3,1.5,1.5,1), mgp=c(1.5,.5,0), oma=c(0,2,0,0))
  
  if(!split){                                                                      
    boxplot(d11~TTtreat, data=x, main="2009-2011", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim)
    abline(h=0);par()$usr
    boxplot(d12~TTtreat, data=x, main="2009-2012", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim)
    abline(h=0);par()$usr 
    boxplot(d13~TTtreat, data=x, main="2009-2013", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim) 
    abline(h=0);par()$usr
  }else{
    
    if(temperature){  
      TT.colours<-rep(TT.colours, each=3)
      at<-c(1:15)[-c(4,8,12)]
      boxplot(d11~Temperature_level+TTtreat, data=x, main="2009-2011", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim, at=at)
      abline(h=0)
      boxplot(d12~Temperature_level+TTtreat, data=x, main="2009-2012", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim, at=at)
      abline(h=0) 
      boxplot(d13~Temperature_level+TTtreat, data=x, main="2009-2013", notch=notch, ylab="", col=TT.colours, las=2,ylim=ylim, at=at)
      abline(h=0)
      
    }else{
      at<-c(1:17)[-c(5,10,14)]
      #col<-rep(TT.colours, c(4,4,3,3))
      col<-c(ppt.colours, ppt.colours, ppt.colours[1:3],ppt.colours[1:3])
      lev<-factor(paste(x$TTtreat, x$Precipitation_level))
      lev<-factor(lev, levels=levels(lev)[c(11:14,1:10)])
      adj<-c(.12,.41,.67,.98)
      
      plotme<-function(x,main){
        boxplot(d~lev, data=x, main=main, notch=notch, ylab="", col=col, las=2,ylim=ylim, at=at, xaxt="n")
        axis(1, at=at, labels=c(1:4, 1:4, 1:3, 1:3))
        axis(1, at=c(2.5, 7.5,12), labels=c("Control", "Warmer", "Wetter"), mgp=par()$mgp+c(0,1,0))
        axis(1, at=16, labels="Warmer \n& wetter", mgp=par()$mgp+c(0,2,0))
        
        #mapply(title, xlab=c("Control", "Warmer", "Wetter", "Warmer \n& wetter"), adj=adj, line=c(rep(par()$mgp[1],3),par()$mgp[1]+1))
        abline(h=0)
    }
    
      if(2011%in%draw){
        boxplot(d11~lev, data=x, main="2009-2011", notch=notch, ylab="", col=col, las=2,ylim=ylim, at=at, xaxt="n")
        axis(1, at=at, labels=c(1:4, 1:4, 1:3, 1:3))
        mapply(title, xlab=c("Control", "Warmer", "Wetter", "Warmer & wetter"), adj=adj)
        abline(h=0)
      }
      if(2012%in%draw){
        boxplot(d12~lev, data=x, main="2009-2012", notch=notch, ylab="", col=col, las=2,ylim=ylim, at=at, xaxt="n")
        axis(1, at=at, labels=c(1:4, 1:4, 1:3, 1:3))
        mapply(title, xlab=c("Control", "Warmer", "Wetter", "Warmer & wetter"), adj=adj)
        abline(h=0)
      }
    #  if(2013%in%draw){
    #    
    #    boxplot(d13~lev, data=x, main="2009-2013", notch=notch, ylab="", col=col, las=2,ylim=ylim, at=at, xaxt="n")
    #    axis(1, at=at, labels=c(1:4, 1:4, 1:3, 1:3))
    #    mapply(title, xlab=c("Control", "Warmer", "Wetter", "Warmer & wetter"), adj=adj)
    #    abline(h=0)
    #  }
      if(2013%in%draw){
        plotme(x=data.frame(lev=lev, d=x$d13), main=ifelse(addmain,"2009-2013",""))
      }
    }
  }  
  title(ylab="Distance moved towards destination", outer=TRUE, line=0.5, cex.lab=ifelse(length(draw)>1,1.2,1))
}
