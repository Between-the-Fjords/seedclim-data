#'Find origin control
#'@description Find turfID of controls in origin block
#'@param tu turfID
#'@return character vector with TT1 and TTC controls
#'@export

find.origin.control<-function(tu){
  
  #find turfID of TT1 turf in destination block
  if (tu == "162 TT2 200") {#no TTC or TT1 in GUD3 - use GUD5 controls instead
    return(c("508 TT1 174","506 TTC"))
  }
  originID.TT1<-as.character(with(turfs,turfID[TTtreat=="TT1"&as.character(blockID)==as.character(blockID[turfID==tu])]))
  originID.TTC<-as.character(with(turfs,turfID[TTtreat=="TTC"&as.character(blockID)==as.character(blockID[turfID==tu])]))
  
  return(c(TT1=originID.TT1,TTC=originID.TTC))
}
