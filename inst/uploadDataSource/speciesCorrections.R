# making corrections
library(DBI)
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")

#load file
corrections <- read.csv("speciesCorrections.csv", sep = ";")

#check for taxon name anomalies
taxon <- dbGetQuery(con, paste("SELECT species FROM taxon;"))$species

stopifnot(
  all(corrections$old %in% taxon, na.rm = TRUE),
  all(corrections$new %in% taxon)
)


#Correcting for species that have undergone rapid evolution


plyr::a_ply(corrections, 1, function(r){
  if(is.na(r$old)){
    dbGetQuery(con, paste0("INSERT INTO turfCommunity (turfID, year, species, cover, cf) VALUES ('", r$turfID,"', ", r$year,", '", r$species,"', ", r$cover,", 0);" )) #check out blank
  } else{
    currentCover <- dbGetQuery(con, paste0("SELECT cover FROM turfCommunity WHERE year = ", r$year," AND turfID = '", r$turfID,"';"))$cover # current cover of new species before updates
    if(length(currentCover) == 0){
      dbGetQuery(con, paste0("UPDATE turfCommunity SET species = '", r$new,"' WHERE year = ", r$year," AND turfID = '", r$turfID,"' AND species = '", r$old,"';"))
      dbGetQuery(con, paste0("UPDATE subTurfCommunity SET species = '", r$new,"' WHERE year = ", r$year," AND turfID = '", r$turfID,"' AND species = '", r$old,"';"))
    } else{
      stop("unable to merge taxa")
    }
  }
})


