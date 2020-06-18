##########################################
### Code to assemble data for analyses ###
### Seedclim climate difference paper  ###
### Lynn et al.                        ###
##########################################

# install packages
library(tidyverse)
library(DBI)# also needs RSQLite installed
library(car)
library(dplyr)
library(conflicted)

# set preferences with conflicted
conflict_prefer("filter", winner="dplyr")
conflict_prefer("select", winner="dplyr")

## ---- load_community data

#make database connection
con <- dbConnect(RSQLite::SQLite(), "database/seedclim_new.sqlite")

## tells you whats in the data base
DBI::dbListTables(con)

## Assemble community data set for the turf
comdat_all <- tbl(con, "turfCommunity") %>%
  collect() %>%
  left_join(tbl(con, "taxon"),by="species", copy=TRUE) %>%
  left_join(tbl(con, "turfs"), by="turfID", copy=TRUE) %>%
  left_join(tbl(con, "plots"), by=c("destinationPlotID"="plotID"), copy=TRUE) %>% 
  left_join(tbl(con, "blocks"), by="blockID", copy=TRUE) %>%
  left_join(tbl(con, "sites"), by="siteID", copy=TRUE) %>%
  select(siteID, blockID, plotID=destinationPlotID, turfID, siteCode, latitude,
         longitude,year,TTtreat, species,speciesName,family, cover) %>%
  collect()

# remove TT1 treatment- no longer surveyed 
comdat_all <-filter(comdat_all, !(TTtreat=="TT1"))

##separate by year
comdat2009 <- filter(comdat_all, year=="2009")
comdat2009 <- comdat2009 %>% rename(cover9 =cover)%>%
  select(siteID, blockID, plotID, turfID, siteCode, latitude,
         longitude,TTtreat, species,speciesName,family, cover9)

comdat2017 <- filter(comdat_all, year=="2017")
comdat2017 <- comdat2017 %>% rename(cover17 =cover) %>%
  select(siteID, blockID, plotID, turfID, siteCode, latitude,
         longitude,TTtreat, species,speciesName,family, cover17)

## Combine the years
comdat <- full_join(comdat2009, comdat2017,
                    by=c("siteID", "blockID", "plotID", "turfID", "siteCode", "latitude",
                         "longitude", "TTtreat", "species", "speciesName", "family"))
                         
# add zeros back in
comdat$cover9 <- comdat$cover9 %>% replace_na(0)
comdat$cover17 <- comdat$cover17 %>% replace_na(0)

comdat <- comdat %>% 
  filter(!(species %in% c("NID.seedling", "NID.gram","NID.rosett","X...","X....1","Cer",
                         "Åkerplante", "Alc.sp","Car.sp","Car.sp1","Cer.sp", "Dry.sp",
                         "Eup.sp", "Gen.sp", "Hie.sp", "Dry.sp","Pyr.sp","Ran...", "Ver.sp",
                         "Vio.sp", "NID.herb", "Luz.sp", "Sal.sp", "Epi.sp", "Sag.sp")))
  
## climate data from worldclim download at:
# https://www.worldclim.org/data/worldclim21.html

# required packages
library(rgdal);library(sp);library(ggplot2);library(rgeos);library(maptools)
library(rgdal);library(raster);library(rasterVis)

# load in the worldclim rasters
mat_wc <- raster("wc2_30seconds/wc2.0_bio_30s_01.tif") # MAT
sum_wc <- raster("wc2_30seconds/wc2.0_bio_30s_10.tif") # temp of warmest quarter
ap_wc <- raster("wc2_30seconds/wc2.0_bio_30s_12.tif") # AP (annual precipitation)

# pull out points for the sites
pts <- as.data.frame(cbind(comdat$longitude, comdat$latitude))
names(pts) <- c("lon", "lat")

# exreact climate dat 
matext <- raster::extract(mat_wc, pts, method="simple", df=TRUE)
sumext <- raster::extract(sum_wc, pts, method="simple", df=TRUE)
apext <- raster::extract(ap_wc, pts, method="simple", df=TRUE)

# add them to eachtoher
climdat <- left_join(matext, sumext, by="ID")
climdat <- left_join(climdat, apext, by="ID")

# combine the community and climate data sets
comdat <- bind_cols(comdat, climdat)
comdat <- comdat %>% rename(mat_site=wc2.0_bio_30s_01) %>%
  rename(summer_site=wc2.0_bio_30s_10) %>%
  rename(ap_site=wc2.0_bio_30s_12) 

## ## functions for change with function
# first function makes extinctions -1 and colonizations 1
# second is the change of cover metric
cordiff <- function(init, post){(post-init)/(init+post)}
deltcov <- function(init, post){log(post+1)-log(init+1)}

# add them to dataset
comdat$difcor <- cordiff(init=comdat$cover9, post=comdat$cover17) 
comdat$dif2 <- deltcov(comdat$cover9, comdat$cover17) 

## get treatments as factors
comdat$warm <- if_else(comdat$TTtreat == "TT2",1,
                       if_else(comdat$TTtreat== "TT4",1,
                               if_else(comdat$TTtreat== "TTC",0,
                                       if_else(comdat$TTtreat=="TT3",0,0))))

comdat$wet <- if_else(comdat$TTtreat == "TT2",0,
                      if_else(comdat$TTtreat== "TT4",1,
                              if_else(comdat$TTtreat== "TTC",0,
                                      if_else(comdat$TTtreat=="TT3",1,0))))

# Load in spp climate data
climsppsum <- read.csv("sppclimsum_postclean_wc2.csv")

# fix species that dont match wtih GBIF
comdat <- comdat %>%
  mutate(speciesName=case_when(speciesName=="Hieracium pilosella" ~"Pilosella officinarum",
                               speciesName=="Taraxacum sp."~"Taraxacum officinale",
                               speciesName=="Luzula multiflora"~"Luzula lutea",
                               speciesName=="Kobresia simpliciuscula"~"Carex simpliciuscula",
                               speciesName=="Empetrum hermaphroditum"~"Empetrum nigrum",
                               speciesName=="Arctous alpinus"~"Arctostaphylos alpinus",
                               speciesName=="Trientalis europaea"~"Lysimachia europaea",
                               speciesName=="Coeloglossum viride"~"Dactylorhiza viridis",
                               speciesName=="Minuartia biflora"~"Cherleria biflora",
                               speciesName=="Viscaria alpina"~"Silene suecica",
                               speciesName=="Loiseleuria procumbens"~"Kalmia procumbens",
                               speciesName=="Carex serotina"~"Carex oederi",
                               speciesName=="Deschampsia alpina"~"Deschampsia cespitosa",
                               TRUE~speciesName))

comdat <- left_join(comdat, climsppsum, by=c("speciesName"="species"))
print(comdat, width=Inf)

# remove NAs
comdat2 <- filter(comdat, !is.na(mean_mat_sub))
comdat2 <- filter(comdat2, !is.na(warm))

# construct the climate difference metric
comdat2$ap_meandiff <-  (comdat2$mean_ap_sub/1000)- (comdat2$ap_site/1000)
comdat2$mat_meandiff <-   (comdat2$mean_mat_sub)-(comdat2$mat_site)
comdat2$sum_meandiff <- (comdat2$mean_sum_sub)-(comdat2$summer_site) 

# Add extinction 
extinct <- 0
for(i in 1:length(comdat2$difcor)){
  extinct[i]<- if(comdat2$difcor[i] == -1) 1 else 0
}
comdat2$extinct <- extinct

# add colonization
colon <- 0
for(i in 1:length(comdat2$difcor)){
  colon[i]<- if(comdat2$difcor[i] == 1) 1 else 0
}
comdat2$colon <- colon

# take out turfs that are not present in both years
comdat2 <- filter(comdat2, !(turfID %in% c("297 TTC", "108 TT3 208")))

## remove uneeded columns
comdat2 <- comdat2 %>% select(siteID, blockID, plotID, turfID, siteCode, latitude, longitude, 
                              TTtreat, species, speciesName, family, cover9, cover17, 
                              mat_site, summer_site, ap_site, difcor, dif2, warm, wet,
                              mean_mat_sub, mean_sum_sub, mean_ap_sub, ahull, ap_meandiff,
                              mat_meandiff, sum_meandiff, colon, extinct)

write.csv(comdat2, "ClimDiff_LynnETAL_data.csv")
