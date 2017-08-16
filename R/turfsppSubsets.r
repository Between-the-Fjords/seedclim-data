#'Turf Species Subsets
#'@description Subsets of the community by their immigration and extinction status
#'@param tu turfID
#'@param year Years to use when compiling destination community
#'@param TT1 Use TT1 when compiling target community
#'@param TTC Use TTC when compiling target community
#'@return list of the different subsets of the community
#'\itemize{
#'  \item{origin Original species list}
#'  \item{final Final species list}
#'  \item{destination Destination block controls' species list}
#'  \item{potentialExtinctions Species initially present not found in destination controls}
#'  \item{realisedExtinctions Species that become extinct}
#'  \item{expectedExtinctions Species that become extinct and were potential extinctions}
#'  \item{unexpectedExtinctions Species that become extinct and were not potential extinctions}
#'  \item{persistant Species that persist}
#'  \item{potentialImmigration Species present in the destination controls not present in the original turf}
#'  \item{realisedImmigration Species that immigrate}
#'  \item{expectedImmigration Species that immigrate and were potentialImmigration}
#'  \item{unexpectedImmigration Species that immigrate and were not potentialImmigration}
#'}
#'@export

turfsppSubsets <- function(tu, year=2009:2013, TT1=TRUE, TTC=TRUE){
  res<-list()
  res$original<-get.turf.community(tu, year=2009)
  res$final<-get.turf.community(tu, year=2013)
  res$destination<-get.dest.control.community(tu=tu,TT1=TT1, TTC=TTC, year=year)
  
  res$potentialExtinctions<-setdiff(res$original,res$destination)
  res$realisedExtinctions<-setdiff(res$original, res$final)
  res$expectedExtinctions<-intersect(res$realisedExtinctions, res$potentialExtinctions)
  res$unexpectedExtinctions<-setdiff(res$realisedExtinctions, res$potentialExtinctions)
  res$persistant<-intersect(res$original,res$final)
  res$potentialImmigration<-setdiff(res$destination, res$original)
  res$realisedImmigration<-setdiff(res$final, res$original)
  res$expectedImmigration<-intersect(res$realisedImmigration, res$potentialImmigration)
  res$unexpectedImmigration<-setdiff(res$realisedImmigration, res$potentialImmigration)
  return(res)
}
