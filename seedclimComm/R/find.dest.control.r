#'Find destination control
#'@description Find turfID of controls in the destination block
#'@param tu turfID
#'@return character vector with TT1 and TTC controls
#'@export

find.dest.control<-function(tu){
  #find turfID of TT1 turf in destination block
  destID.TT1<-as.character(with(turfs,turfID[TTtreat=="TT1"&as.character(blockID)==as.character(destBlockID[turfID==tu])]))
  destID.TTC<-as.character(with(turfs,turfID[TTtreat=="TTC"&as.character(blockID)==as.character(destBlockID[turfID==tu])]))
  return(c(TT1=destID.TT1,TTC=destID.TTC))
}
