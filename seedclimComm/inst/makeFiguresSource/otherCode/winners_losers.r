library(vegan)


source("load other data.r")

gram.cover<-rowSums(cover[,traits$functional=="graminoid"])
boxplot(gram.cover~Year+TTtreat, data=cover.meta, las=2, col=rep(bcol, each=4), notch=TRUE)



wl<-apply(turfs,1,function(R){   # browser()
     fat<-cover[cover.meta$turfID==R["turfID"],]
     meta<-cover.meta[cover.meta$turfID==R["turfID"],]
   #  print(all(diff(meta$Year)>0))
     win<-apply(fat[-1,],1,function(x)sum(fat[1,]==0&x>0))
     loss<-apply(fat[-1,],1,function(x)sum(fat[1,]>0&x==0))
     wingram<-apply(fat[-1,],1,function(x)sum(fat[1,]==0&x>0&traits$functional=="graminoid"))
     lossgram<-apply(fat[-1,],1,function(x)sum(fat[1,]>0&x==0&traits$functional=="graminoid"))
     res<-cbind(win,loss, wingram, lossgram)
     if(nrow(res)<3)res<-c(res, matrix(NA,ncol=ncol(res),nrow=3-nrow(res)))
     res
})

sapply(levels(turfs$siteID),function(S){#browser() 
   x11()
   meta<-turfs[turfs$siteID==S,]
   WL<-wl[,turfs$siteID==S]
   plot(NA, NA, xlim=c(1,3), ylim=c(-22,22),main=S, axes=F, xlab="Year comparison", ylab="Winners/loosers")
   axis(2)
   axis(1, at=1:3, labels=c("09-11", "09-12", "09-13"))
   mapply(function(x, col){
     lines(1:3,x[1:3],col=col)
     lines(1:3,-x[4:6],col=col)
     abline(h=0, lty=1)
     },x=as.data.frame(WL), col=bcol[meta$TTtreat]) 
})


sapply(levels(turfs$siteID),function(S){#browser() 
   meta<-turfs[turfs$siteID==S,]
   WL<-wl[,turfs$siteID==S]
   
   dat<-data.frame(WL=as.vector(WL)*c(1,1,1,-1,-1,-1),TTtreat=rep(meta$TTtreat,each=6),year=11:13,winloss=rep(c("w","l", "wg", "lg"),each=3)) 
   x11()
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="w",], col=bcol, ylab="loosers/winners", las=2, main=meta$siteID[1], ylim=c(-22,22))
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="l",], col=bcol, add=TRUE, las=2)
   abline(h=0)
   x11()
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="wg",], col=bcol, ylab="graminoid loosers/winners", las=2, main=paste(meta$siteID[1], "graminoid"), ylim=c(-22,22))
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="lg",], col=bcol, add=TRUE, las=2)
   abline(h=0)
   
})

sapply(levels(turfs$siteID),function(S){#browser() 
   x11()
   meta<-turfs[turfs$siteID==S,]
   WL<-wl[,turfs$siteID==S]
   
   
   dat<-data.frame(WL=as.vector(WL)*c(1,1,1,-1,-1,-1),TTtreat=rep(meta$TTtreat,each=6),year=11:13,winloss=rep(c("w","l"),each=3)) 
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="w",], col=bcol, ylab="loosers/winners", las=2, main=meta$siteID[1], ylim=c(-22,22))
   boxplot(WL~TTtreat+year,data=dat[dat$winloss=="l",], col=bcol, add=TRUE, las=2)
   abline(h=0)
})
   
#heatmap   by treatment

turfswl<-cbind(turfs,t(wl))

x11()
plot(NA,NA,xlim=c(1,3),ylim=c(1,4), xlab="Temperature level", ylab="Precipitation level", main="TTC")
text(expand.grid(1:3, 1:4), labels=apply(expand.grid(1:3, 1:4),1, function(R)median(turfswl[turfswl$TTtreat=="TTC"&turfswl$Temperature_level==R[1]&turfswl$Precipitation_level==R[2] ,"1"])))


x11()
plot(NA,NA,xlim=c(1,3),ylim=c(1,4), xlab="Temperature level", ylab="Precipitation level", main="TT1")
text(expand.grid(1:3, 1:4), labels=apply(expand.grid(1:3, 1:4),1, function(R)median(turfswl[turfswl$TTtreat=="TT1"&turfswl$Temperature_level==R[1]&turfswl$Precipitation_level==R[2] ,"1"])))


x11()
plot(NA,NA,xlim=c(1,3),ylim=c(1,4), xlab="Temperature level", ylab="Precipitation level", main="TT2")
text(expand.grid(1:3, 1:4), labels=apply(expand.grid(1:3, 1:4),1, function(R)median(turfswl[turfswl$TTtreat=="TT2"&turfswl$Temperature_level==R[1]&turfswl$Precipitation_level==R[2] ,"1"])))

x11()
plot(NA,NA,xlim=c(1,3),ylim=c(1,4), xlab="Temperature level", ylab="Precipitation level", main="TT3")
text(expand.grid(1:3, 1:4), labels=apply(expand.grid(1:3, 1:4),1, function(R)median(turfswl[turfswl$TTtreat=="TT3"&turfswl$Temperature_level==R[1]&turfswl$Precipitation_level==R[2] ,"1"])))

x11()
plot(NA,NA,xlim=c(1,3),ylim=c(1,4), xlab="Temperature level", ylab="Precipitation level", main="TT4")
text(expand.grid(1:3, 1:4), labels=apply(expand.grid(1:3, 1:4),1, function(R)median(turfswl[turfswl$TTtreat=="TT4"&turfswl$Temperature_level==R[1]&turfswl$Precipitation_level==R[2] ,"1"])))




#other metrics of win/loss
tmp<-cover[cover.meta$turfID=="145 TT3 231",][1:2,]
tmp<-tmp[,colSums(tmp>0)>0]
clm<-clamtest(tmp, spec=.5, alpha=.05)
plot(clm)


plot.clamtest<-function (x, xlab, ylab, main, pch = 21:24, col.points = 1:4, 
    col.lines = 2:4, lty = 1:3, position = "bottomright", ...) 
{
    summ <- summary(x)
    glabel <- summ$labels
    if (missing(main)) 
        main <- "Species Classification"
    if (missing(xlab)) 
        xlab <- paste(glabel[2], "(abundance + 1)")
    if (missing(ylab)) 
        ylab <- paste(glabel[1], "(abundance + 1)")
    Y <- x[, 2]
    X <- x[, 3]
    minval <- summ$minv
    rr <- range(X + 1, Y + 1)
    plot(X + 1, Y + 1, log = "xy", xaxt = "n", yaxt = "n", col = col.points[as.integer(x$Classes)], 
        pch = pch, xlab = xlab, ylab = ylab, 
        main = main, xlim = rr, ylim = rr, ...)
    axis(1, c(1, 10, 100, 1000, 10000))
    axis(2, c(1, 10, 100, 1000, 10000))
    Ymin <- minval[[1]][1, 2]
    Xmin <- minval[[2]][1, 1]
    lines(rep(Xmin, 2) + 1, c(0, 1) + 1, col = col.lines[1], 
        lty = lty[1])
    lines(c(0, 1) + 1, rep(Ymin, 2) + 1, col = col.lines[1], 
        lty = lty[1])
    tmp <- approx(c(Xmin, 1), c(1, Ymin))
    lines(tmp$x + 1, tmp$y + 1, col = col.lines[1], lty = lty[1])
    lines(minval[[1]] + 1, col = col.lines[2], lty = lty[2])
    lines(minval[[2]] + 1, col = col.lines[3], lty = lty[3])
    if (!is.null(position)) 
        legend(position, col = col.points, pch = pch, legend = rownames(summ$summary))
    invisible(x)
}


sapply(levels(cover.meta$siteID),function(S){
  x11()
  par(mfrow=c(3,2), mar=c(3,3,2,1), mgp=c(1.5,.5,0), oma=c(0,0,1,0))
  sapply(sort(unique(cover.meta$TTtreat[cover.meta$siteID==S])), function(TT){
    tmp<-cover[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    tmp<-tmp[,colSums(tmp>0)>0]
    meta<-cover.meta[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    clm<-clamtest(tmp,meta$Year, specialization =.5, alpha=.05)
    plot(clm, main=TT, position=NULL,pch=ifelse((traits$functionalGroup=="graminoid")[traits$species%in%colnames(tmp)],16,1))
    abline(0,1)
  })
  title(main=S, outer=TRUE)
})


sapply(levels(cover.meta$siteID),function(S){
  x11()
  par(mfrow=c(3,2), mar=c(3,3,2,1), mgp=c(1.5,.5,0), oma=c(0,0,1,0))
  sapply(sort(unique(cover.meta$TTtreat[cover.meta$siteID==S])), function(TT){
    tmp<-cover[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    tmp<-tmp[,colSums(tmp>0)>0]
    meta<-cover.meta[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    clm<-clamtest(tmp,meta$Year, specialization =.5)
    plot(clm, main=TT, position=NULL,pch=ifelse((traits$family=="Poaceae")[traits$species%in%colnames(tmp)],16,1))
    abline(0,1)
  })
  title(main=S, outer=TRUE)
})


#SIMPER
sapply(levels(cover.meta$siteID),function(S){
  sapply(sort(unique(cover.meta$TTtreat[cover.meta$siteID==S])), function(TT){
    tmp<-cover[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    meta<-cover.meta[cover.meta$siteID==S&cover.meta$TTtreat==TT&cover.meta$Year%in%c(2009, 2013),]
    res<-simper(tmp,meta$Year)
    res2<-res[[1]]$cusum
    res2[1]
  })
})




