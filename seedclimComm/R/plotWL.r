#'plot of winners and losers
#'@param W winners and losers
#'@param ylim y axis limits
#'@export
#barplots
plotWL<-function(W, ylim){
  W<-sapply(W,function(w)
    sapply(w, function(x)
      if(any(is.na(x))&length(x)==1){NA}else{length(x)}
    )
  )
  if(missing(ylim))  ylim=c(0,max(W, na.rm=TRUE))
  par(mfrow=c(3,1), mar=c(0,3,0,1), mgp=c(1.5,.5,0), oma=c(4,0,1,0))
  
  barplot(W["winner",]+W["immigrant",], ylim=ylim, xaxt="n",col="palegreen3")
  barplot(W["winner",], ylim=ylim, xaxt="n",add=TRUE, col="palegreen")
  
  barplot(-W["losers",], ylim=-rev(ylim), xaxt="n", col="tan2")
  barplot(-W["losers",]+W["extinct",], ylim=-rev(ylim), xaxt="n", add=TRUE, col="lightgoldenrod1")
  par(mar=c(0,3,1,1))
  barplot(W["generalist",], ylim=ylim, xaxt="n")
  axis(1, outer=TRUE)
}
