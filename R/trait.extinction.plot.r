#' trait extinction plot
#' @param sites which sites to include
#' @param treatment which treatment to include
#' @param trait which trait to include
#' @param numeric is the trait numeric
#' @param char value of character trait
#' @param plot should the result be plotted/
#' @param type which contrast should be used: extinction, immigant or persistant
#' @return result that can be used by multitrait
#'@export

trait.extinction.plot<-function(sites, treatment, trait, numeric,char=NULL, plot=TRUE,type="extinction" ){
  if(type=="extinction")
    test<-c("potentialExtinctions",  "expectedExtinctions")
  if(type=="immigrant")
    test<-c("potentialImmigration",  "expectedImmigration")
  if(type=="persistant")
    test<-c("persistant", "unexpectedExtinctions")
  
  res<-sapply(sites, function(site){  print(site) 
                                      turfIDs<-turfs$turfID[turfs$newTT==treatment&turfs$siteID==site]
                                      if(length(turfIDs)==0){return(c(pot=NA, exp=NA))}                         
                                      exc<-extinctClasses(turfIDs)
                                      pot=traits.for.species(trait, unlist(exc[test[1],]), char)
                                      exp=traits.for.species(trait, unlist(exc[test[2],]), char)
                                      
                                      if(numeric){
                                        
                                        c(pot=mean(pot, na.rm=TRUE), exp=mean(exp, na.rm=TRUE))
                                      }else{
                                        if(length(pot)==0) pot=NA
                                        if(length(exp)==0) exp=NA
                                        c(pot=table(pot)[2]/length(pot), exp=table(exp)[2]/length(exp))
                                      }
                                      
  })
  res=t(res)
  if(numeric){
    lab= paste("Mean", trait, "of")
  }else{
    lab=paste("Proportion", trait, ifelse(missing(char),"", paste("=", char)), "in")
  }
  if(plot){
    plot(res, xlab=paste(lab, "potential extinctions"), ylab=paste(lab, "expected extinctions"), main=treatment)
    text(res, labels=rownames(res), pos=4)
    abline(0,1)
  }
  res
}
