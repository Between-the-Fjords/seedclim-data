library(dplyr)
library(readxl)
library(DBI)
library(dbplyr)
con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")
setwd("/Users/fja062/Documents/seedclimComm/seedclimComm/")

gudfun2015 <- read_excel("/Volumes/fja062/PhD/Data/funcab/funcab_Gudmedalen.xlsx")
funcab_2015 <- read_delim("/Volumes/fja062/PhD/Data/funcab/funcab_composition_2015.csv", delim = ";")
funcab_2016 <- read_delim("/Volumes/fja062/PhD/Data/funcab/funcab_composition_2016.csv", delim = ";")


dat <- gudfun2015
spp <- cbind(dat[, c("turfID", "year", "Site", "Block", "TotalGraminoids", "totalForbs", "totalBryophytes", "totalVascular", "soil", "vegetationHeight")], dat[, (which(names(dat) == "recorder") + 1) : (which(names (dat) == "Nid herb")-1) ])[dat$Measure == "Cover",]
spp[, 5 : ncol(spp)] <- plyr::colwise(as.numeric)(spp[, 5 : ncol(spp)])

# replacing " " and "_" with "." in species names
names(spp)[-(1:10)] <- gsub("\\ ", ".", names(spp)[-(1:10)])
names(spp)[-(1:10)] <- gsub("\\_", ".", names(spp)[-(1:10)])

gudfun2015 <- spp
funcab_2015 <- spp
funcab_2016 <- spp

composition <- funcab_2016 %>% 
  full_join(funcab_2015) %>% 
  full_join(gudfun2015)

save(composition, file = "/Volumes/fja062/PhD/Data/funcab/funcabCompData.RData")

load(file = "/Volumes/fja062/PhD/Data/funcab/funcabCompData.RData")

composition <- composition %>% 
  rename(siteID = Site, Year = year, blockID = Block) %>% 
  mutate(blockID = as.character(blockID)) %>% 
  gather(species, cover, 11:285) %>% 
  mutate(turfID = plyr::mapvalues(turfID, from = "Alr4FGB", to = "Alr5C")) %>% 
  filter(!(blockID == 4 & Year == 2015 & siteID == "Alrust"))

mergedictionary <- tbl(con, "mergedictionary") %>% collect()

# update names to correct dictionary names
probfixes=function(df, old, new){
  for(i in 1:length(old)){
    df=replace_all(df,old[i],new[i])
  }
  return(df)
}

#A function that finds and replaces a string (using regex) in a dataframe
replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}

composition <- probfixes(composition, mergedictionary$oldID, mergedictionary$newID)

taxon <-tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

my.GR.data <- my.GR.data %>% 
  filter(Year > 2014)

comp2 <- composition %>% 
  filter(cover>0) %>% 
  left_join(taxon) %>% 
  full_join(my.GR.data)

comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "pteridophyte", to = "forb")
comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "woody", to = "forb")

comp2 <- comp2 %>% 
  select(-plotID, - TTtreat) %>% 
  group_by(turfID, Year, functionalGroup) %>% 
  mutate(sumCover = sum(cover, na.rm = TRUE))

save(comp2, file = "/Volumes/fja062/PhD/Data/funcab/funcabCompDataWithTTCs.RData")

load(file = "/Volumes/fja062/PhD/Data/funcab/funcabCompDataWithTTCs.RData")




mdat <- map_df(myfiles, ReadIniButtons)

save(mdat, file = "funcabCompData.RData")



xtras <- read_delim(file = "/Volumes/fja062/PhD/Data/funcab/funcab_composition_2015.csv", delim = ";")

xtras <- xtras %>% 
  filter(Treatment == "C", Measure == "Cover", grepl("TTC", turfID)) %>%
  select(turfID, Year = year, TotalGraminoids = `Total Graminoids`, totalForbs = `total Forbs`) %>% 
  mutate(totalForbs = as.numeric(totalForbs), TotalGraminoids = as.numeric(TotalGraminoids))

load("/Users/fja062/Desktop/funcabCompDataWithTTCshopethisworks.RData")

comp23 <- comp2 %>% 
  left_join(xtras, by = c("turfID", "Year"))

save(xtras, file = "/Users/fja062/Desktop/xtras.RData")

