#'plots multitrait result
#'@param x multitrait object
#'@param \dots extra arguments
#'@export
plot.multitrait<-function(x,...){
  boxplot(x,col=TT.colours[3:5],xaxt="n",...)
  axis(side=1, at=1:2, labels=c("Warmer", "Wetter"))
  axis(side=1, at=3, labels="Warmer\n& wetter", mgp=par()$mgp+c(0,1,0))
  abline(h=0, lty=2, col="grey80")
  
}