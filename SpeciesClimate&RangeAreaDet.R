#############################################
### RGBIF data download and data cleaning ### 
#############################################


### Load in packages
library(rgbif)
library(tidyverse)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize)

## need to setup GBIF account and add info here
options(gbif_user="", gbif_email="", gbif_pwd="")

# put together list of needed species
tax_name <- c("Anthoxanthum odoratum","Agrostis capillaris","Campanula rotundifolia","Festuca rubra","Bistorta vivipara","Deschampsia cespitosa","Achillea millefolium","Potentilla erecta","Veronica alpina","Veronica officinalis","Alchemilla alpina","Poa pratensis","Nardus stricta","Trifolium repens","Selaginella selaginoides","Thalictrum alpinum","Poa alpina","Viola palustris","Sibbaldia procumbens","Viola biflora","Leontodon autumnalis","Rumex acetosa","Taraxacum officinale","Festuca ovina","Salix herbacea","Luzula multiflora","Ranunculus acris","Carex capillaris","Carex vaginata","Viola riviniana","Carex vaginata","Omalotheca supina","Rumex acetosella","Phleum alpinum","Avenella flexuosa","Hieracium pilosella","Stellaria graminea","Prunella vulgaris","Silene acaulis","Trifolium pratense","Knautia arvensis","Carex bigelowii","Astragalus alpinus","Carex pallescens","Veronica chamaedrys","Epilobium anagallidifolium","Carex norvegica","Vaccinium myrtillus","Potentilla crantzii","Carex pilulifera","Lotus corniculatus","Cerastium fontanum","Veronica serpyllifolia","Agrostis mertensii","Empetrum hermaphroditum","Pinguicula vulgaris","Antennaria dioica","Saussurea alpina","Dianthus deltoides","Pimpinella saxifraga","Galium verum","Geranium sylvaticum","Betula nana","Gentiana nivalis","Luzula spicata","Galium saxatile","Parnassia palustris","Plantago media","Rhinanthus minor","Calluna vulgaris","Carex leporina","Oxalis acetosella","Plantago lanceolata","Veronica fruticans","Carex nigra","Leucanthemum vulgare","Solidago virgaurea","Carex panicea","Pyrola minor","Saxifraga aizoides","Silene vulgaris","Viola tricolor","Hieracium vulgatum","Carum carvi","Festuca vivipara","Hypericum maculatum","Stellaria media","Betula pubescens","Cerastium cerastoides","Galium uliginosum","Noccaea caerulescens","Tofieldia pusilla","Melampyrum pratense","Carex atrata","Vaccinium vitis-idaea","Carex pulicaris","Trifolium medium","Vaccinium uliginosum","Botrychium lunaria","Oxyria digyna","Vicia cracca","Carex flava","Kobresia simpliciuscula","Omalotheca norvegica","Pyrola rotundifolia","Rubus idaeus","Salix reticulata","Aconitum septentrionale","Cirsium palustre","Galium boreale","Hypochaeris maculata","Ranunculus auricomus","Ranunculus repens","Sedum acre","Trichophorum cespitosum","Carex echinata","Cerastium alpinum","Comastoma tenellum","Geum rivale","Juncus trifidus","Luzula pilosa","Myosotis decumbens","Phyllodoce caerulea","Dactylis glomerata","Gentianella amarella","Hieracium alpinum","Gymnocarpium dryopteris","Juniperus communis","Potentilla argentea","Sorbus aucuparia","Succisa pratensis","Viscaria vulgaris","Arctous alpina","Bartsia alpina","Carex saxatilis","Gentiana purpurea","Juncus alpinoarticulatus","Omalotheca sylvatica","Phleum pratense","Plantago major","Rhodiola rosea","Anemone nemorosa","Coeloglossum viride","Danthonia decumbens","Erigeron uniflorus","Fragaria vesca","Huperzia selago","Melampyrum sylvaticum","Orthilia secunda","Tofieldia calyculata","Lysimachia europaea","Valeriana sambucifolia","Veronica arvensis","Minuartia biflora","Dryas octopetala","Oxytropis lapponica","Viola canina","Hieracium umbellatum","Avenula pubescens","Poa annua","Equisetum arvense","Viscaria alpina","Carex dioica","Carex rupestris","Linaria vulgaris","Pinus sylvestris","Saxifraga cespitosa","Hypochaeris radicata","Geum urbanum","Gentianella campestris","Ajuga pyramidalis","Saxifraga oppositifolia","Loiseleuria procumbens","Deschampsia alpina","Picea abies","Capsella bursa-pastoris","Filipendula ulmaria","Arctostaphylos uva-ursi","Carex oederi","Drosera rotundifolia","Vicia sepium")


# code to retrieve taxon key for use with GBIF

gbif_taxon_keys <- tax_name %>%  
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
  pull(usagekey)

# assemble the keys
gdat <- as.data.frame(gbif_taxon_keys)
gdar <- unique(gdat$gbif_taxon_keys)
gdar <- gdar[!is.na(gdar)]

tax <- paste(gbif_taxon_keys, collapse = ",")
sprintf("taxonKey = tax", paste0(gdar, collapse = ","))

# first download from lookup and parent key usage
occ_download(
  "taxonKey =  2705975,2706490,5410907,2704922,2889299,8059811,3120060,7874143,3172050,3172049,3027648,2704178,2702717,5358748,2688197,3033134,2704234,5331355,3025029,5331325,8058115,2888951,5394163,2704951,5373083,3033363,2729936,2725816,7296321,2725816,3104537,2888995,8952395,4149465,5384616,5341297,5384754,8324121,2888808,2722383,5345014,2726592,7874883,3189068,2724869,2882833,8285546,2726614,5357013,3085458,3172057,2706518,5415065,5385604,5404542,3085424,7622278,9063248,2891147,5332004,3170097,2700795,2914547,3032585,3189747,3172043,2882482,8285827,8211958,8208358,3172054,2724925,8848598,6446462,2724609,2888272,8165236,9172281,5331347,7777868,3034714,2704930,6437017,5384604,9118014,6364299,2913556,3045463,2864429,7693914,8657424,2882835,2726956,5358812,8073364,2650165,2889358,2975287,2726880,3118057,2888269,2993094,5372693,6374443,3113239,2914338,3093693,8212918,3033339,5362007,2710592,2726894,3085450,3840575,5369701,9011521,2700736,7295092,5333379,2705308,8196434,3135561,2650832,2684709,8011540,3012167,2888811,8153621,3170807,2727697,8207244,2701930,3103192,9014945,3189767,2985688,3033263,2703250,3147144,3029817,8190643,7331633,2888310,2864397,8272844,8063871,3172048,4889932,5361156,5331258,3135329,2703793,2704179,7924597,2728870,2723767,5415020,5285637,3032796,3093702,5369652,7270427,8429926,8353146,5284884,5375388,2987999,2882580,7973697,3191030,2974786",
  "hasCoordinate = TRUE",
  "hasGeospatialIssue = FALSE",
  "year >= 1950",
  "continent = europe",
  format = "SIMPLE_CSV")

### add in what is missing from the original list

taxize::get_gbifid_("Arctous alpina",method="backbone") 
taxize::get_gbifid_("Coeloglossum viride",method="backbone") 
taxize::get_gbifid_("Deschampsia alpina",method="backbone") 
taxize::get_gbifid_("Empetrum hermaphroditum",method="backbone") 
taxize::get_gbifid_("Hieracium pilosella",method="backbone") 
taxize::get_gbifid_("Kobresia simpliciuscula",method="backbone") 
taxize::get_gbifid_("Loiseleuria procumbens",method="backbone") 
taxize::get_gbifid_("Luzula multiflora",method="backbone") 
taxize::get_gbifid_("Minuartia biflora",method="backbone") 
taxize::get_gbifid_("Viscaria alpina",method="backbone") 

key2 <- c("2882527,5313015,4144915,6387994,3136365,2710388,2883015,2700934,3085555,6364441")

#second one
occ_download(
  "taxonKey =  2882527,5313015,4144915,6387994,3136365,2710388,2883015,2700934,3085555,6364441",
  "hasCoordinate = TRUE",
  "hasGeospatialIssue = FALSE",
  "year >= 1950",
  "continent = europe",
  format = "SIMPLE_CSV")

###################################################################################################
# second part: clean data and assemble means

#first, download gbif data at the following:
# https://www.gbif.org/occurrence/download/0024756-191105090559680 
# https://www.gbif.org/occurrence/download/0024723-191105090559680


############ packages for the following work
library(tidyverse);library(readr);library(countrycode);library(rgbif);library(sp)
library(CoordinateCleaner);library(maps);library(rgeos);library(maptools);library(rgdal)
library(raster);library(rasterVis);library(rspatial);library(sf);library(spData)
library(mapdata);library(dismo)


# load in the two datasets from GBIF- note the names may need changing
gbif_raw <- read_delim("GBIF_161219.csv", col_names=TRUE, delim="\t", col_types=cols_only(
  gbifID = col_skip(),
  datasetKey = col_skip(),
  occurrenceID = col_skip(),
  kingdom = col_skip(),
  phylum = col_skip(),
  class = col_skip(),
  order = col_skip(),
  family = col_character(),
  genus = col_character(),
  species = col_character(),
  infraspecificEpithet = col_logical(),
  taxonRank = col_character(),
  scientificName = col_character(),
  verbatimScientificName = col_logical(),
  verbatimScientificNameAuthorship = col_logical(),
  countryCode = col_character(),
  locality = col_skip(),
  stateProvince = col_skip(),
  occurrenceStatus = col_logical(),
  individualCount = col_double(),
  publishingOrgKey = col_skip(),
  decimalLatitude = col_double(),
  decimalLongitude = col_double(),
  coordinateUncertaintyInMeters = col_double(),
  coordinatePrecision = col_logical(),
  elevation = col_logical(),
  elevationAccuracy = col_logical(),
  depth = col_skip(),
  depthAccuracy = col_skip(),
  eventDate = col_skip(),
  day = col_skip(),
  month = col_skip(),
  year = col_double(),
  taxonKey = col_double(),
  speciesKey = col_double(),
  basisOfRecord = col_character(),
  institutionCode = col_skip(),
  collectionCode = col_skip(),
  catalogNumber = col_skip(),
  recordNumber = col_skip(),
  identifiedBy = col_skip(),
  dateIdentified = col_skip(),
  license = col_skip(),
  rightsHolder = col_skip(),
  recordedBy = col_skip(),
  typeStatus = col_skip(),
  establishmentMeans = col_skip(),
  lastInterpreted = col_skip(),
  mediaType = col_skip(),
  issue = col_skip()
))

gbif_raw2 <- read_delim("GBIF_161219_2.csv", col_names=TRUE, delim="\t", col_types=cols_only(
  gbifID = col_skip(),
  datasetKey = col_skip(),
  occurrenceID = col_skip(),
  kingdom = col_skip(),
  phylum = col_skip(),
  class = col_skip(),
  order = col_skip(),
  family = col_character(),
  genus = col_character(),
  species = col_character(),
  infraspecificEpithet = col_logical(),
  taxonRank = col_character(),
  scientificName = col_character(),
  verbatimScientificName = col_logical(),
  verbatimScientificNameAuthorship = col_logical(),
  countryCode = col_character(),
  locality = col_skip(),
  stateProvince = col_skip(),
  occurrenceStatus = col_logical(),
  individualCount = col_double(),
  publishingOrgKey = col_skip(),
  decimalLatitude = col_double(),
  decimalLongitude = col_double(),
  coordinateUncertaintyInMeters = col_double(),
  coordinatePrecision = col_logical(),
  elevation = col_logical(),
  elevationAccuracy = col_logical(),
  depth = col_skip(),
  depthAccuracy = col_skip(),
  eventDate = col_skip(),
  day = col_skip(),
  month = col_skip(),
  year = col_double(),
  taxonKey = col_double(),
  speciesKey = col_double(),
  basisOfRecord = col_character(),
  institutionCode = col_skip(),
  collectionCode = col_skip(),
  catalogNumber = col_skip(),
  recordNumber = col_skip(),
  identifiedBy = col_skip(),
  dateIdentified = col_skip(),
  license = col_skip(),
  rightsHolder = col_skip(),
  recordedBy = col_skip(),
  typeStatus = col_skip(),
  establishmentMeans = col_skip(),
  lastInterpreted = col_skip(),
  mediaType = col_skip(),
  issue = col_skip()
))

# Begin data prep

# bind together and remove fossils, greenland, and duplicates
gbif_clean <- gbif_raw %>% bind_rows(gbif_raw2) %>%
  filter(!(basisOfRecord =="FOSSIL_SPECIMEN")) %>%
  filter(!countryCode %in%  c("GL")) %>%
  distinct()

#take out far east russia
gbifclean<- gbif_clean[gbif_clean$decimalLongitude<50,]

# remove portugal islands
countrycode("Portugal", origin="country.name", destination="iso2c")
sppclimport <- gbifclean[gbifclean$countryCode=="PT",] 
sppclimport <- sppclimport[sppclimport$decimalLongitude>-10,]
gbifclean <- gbifclean[!(gbifclean$countryCode=="PT"),]
gbifclean <-  bind_rows(gbifclean, sppclimport)

# remove spain island
gbifclean <- gbifclean[gbifclean$decimalLatitude>29,]
gbifclean <- rownames_to_column(gbifclean, var="ind")

# further data cleaning with coordinate cleaner (CC)
## new ountry codes for CC
gbifclean$countryCode2 <- countrycode(gbifclean$countryCode,  origin = 'iso2c',
                                      destination = 'iso3c')

# remove missing latitude
gbifclean <- filter(gbifclean, !is.na(gbifclean$decimalLatitude))

# find issues to inspect
flags <- clean_coordinates(x = gbifclean, lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode2", 
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif",
                                     "institutions","zeros", "seas")) 
summary(flags)

# remove clear values in the sea
flagsea <- flags[flags$.sea=="FALSE",]
plot(flagsea, lon = "decimalLongitude", lat = "decimalLatitude", col="black", cex=.25)

flagsea <- flagsea[flagsea$decimalLongitude>-1,]
flagsea <- flagsea[flagsea$decimalLatitude<70,]
flagsea <- flagsea[flagsea$decimalLatitude>67.7,]

gbifclean <- gbifclean %>% 
  filter(!ind %in% c(794876, 916928,923888,930809,958734,917366,917367, 917368, 924324,
                     924325, 924326, 931226, 931227, 938234, 938235, 945142, 945143, 952206,
                     959176,  959177,  966059, 1298432, 1305294, 1305295, 1305296,4247952))

#coordinat uncertainty cleaning
gbifclean <- gbifclean %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

# Individual counts- get rid of 0
table(gbifclean$individualCount)
gbifclean <- gbifclean %>%
  filter(individualCount > 0 | is.na(individualCount)) %>%
  filter(!is.na(decimalLatitude)) 

## end cleaning


#### pull out climate data for the two data sets- start with full
# access climate data here:
# https://www.worldclim.org/data/worldclim21.html

mat_wc30 <- raster("wc2_30seconds/wc2.0_bio_30s_01.tif") # mat
sum_wc30 <- raster("wc2_30seconds/wc2.0_bio_30s_10.tif") # temp warmest quarter
ap_wc30 <- raster("wc2_30seconds/wc2.0_bio_30s_12.tif") # AP 

full_pts <- as.data.frame(cbind(gbifclean$decimalLongitude,
                                gbifclean$decimalLatitude))
names(full_pts) <- c("lon", "lat")

matextfull30 <- raster::extract(mat_wc30, full_pts, method="simple", df=TRUE)
sumextfull30 <- raster::extract(sum_wc30, full_pts, method="simple", df=TRUE)
apextfull30 <- raster::extract(ap_wc30, full_pts, method="simple", df=TRUE)

climdatfull <- left_join(matextfull30, sumextfull30, by="ID")
climdatfull <- left_join(climdatfull, apextfull30, by="ID")

# combine and rename
sppdat_nodup <- bind_cols(gbifclean, climdatfull)
sppdat_nodup <- sppdat_nodup %>% dplyr::select(-c(ID))
sppdat_nodup <- sppdat_nodup %>%
  rename(mat_wc30=wc2.0_bio_30s_01) %>%
  rename(summer_wc30=wc2.0_bio_30s_10) %>%
  rename(ap_wc30=wc2.0_bio_30s_12)


# methods for range area and subsampling
#Spatial data science -> methods for rasterizing and getting species
## range areas

# load map data and check CRS
data(wrld_simpl)
proj4string(wrld_simpl) <- CRS("+proj=longlat +datum=WGS84")
cn <- spTransform(wrld_simpl, CRS("+proj=longlat +datum=WGS84"))

# set coordinates and species range
coordinates(gbifclean) <- ~decimalLongitude + decimalLatitude
proj4string(gbifclean) <- CRS("+proj=longlat +datum=WGS84")

## sample one record per grid cell to compare climate predictions
#raster resotion is 1km
# make a raster grid resolution 0.0083 ~ resolution of worldclim
ras <- raster(gbifclean)
res(ras) <- 0.0083
ras <- extend(ras, extent(ras)+0.04)

# sample from the grid for each spp
xy <- coordinates(gbifclean)
nodup_samp <- xy %>% as_tibble() %>% 
  mutate(sp = gbifclean$species) %>% 
  group_by(sp) %>%
  nest() %>% 
  mutate(sampled = purrr::map(data, ~gridSample(as.data.frame(.x), ras, n=1))) %>%
  dplyr::select(-data) %>% 
  unnest(col=sampled)

nodup_samp
rast_samp <- nodup_samp
head(rast_samp)

# now extract the climate data
# points for the sub sampled raster
sub_pts <- as.data.frame(cbind(rast_samp$decimalLongitude, rast_samp$decimalLatitude))
names(sub_pts) <- c("lon", "lat")

# extract the values
matext <- raster::extract(mat_wc30, sub_pts, method="simple", df=TRUE)
sumext <- raster::extract(sum_wc30, sub_pts, method="simple", df=TRUE)
apext <- raster::extract(ap_wc30, sub_pts, method="simple", df=TRUE)
head(apext)

climdat <- left_join(matext, sumext, by="ID")
climdat <- left_join(climdat, apext, by="ID")

###### Assemble the climate extraction data
rast_samp_wc2 <- bind_cols(rast_samp, climdat)
head(rast_samp_wc2)
rast_samp_wc2 <- rast_samp_wc2 %>%
  rename(species=sp) %>%
  rename(lon=decimalLongitude) %>%
  rename(lat=decimalLatitude) %>%
  rename(mat=wc2.0_bio_30s_01) %>%
  rename(summer=wc2.0_bio_30s_10) %>%
  rename(ap=wc2.0_bio_30s_12) %>%
  dplyr::select(species,lat,lon,mat,summer,ap)

#### pull out climate variables 
# using dplyr to apply summarizing functions of interest
medmatsub<- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(med_mat_sub=median(mat, na.rm=TRUE))

meanmatsub <- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(mean_mat_sub=mean(mat, na.rm=TRUE))

medmatfull<- sppdat_nodup %>%
  group_by(species) %>%
  summarize(med_mat_full=median(mat_wc30, na.rm=TRUE))

meanmatfull <- sppdat_nodup %>%
  group_by(species) %>%
  summarize(mean_mat_full=mean(mat_wc30, na.rm=TRUE))

## summer temp
medsumsub <- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(med_sum_sub=median(summer, na.rm=TRUE))

meansumsub <- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(mean_sum_sub=mean(summer, na.rm=TRUE))

medsumfull <- sppdat_nodup %>%
  group_by(species) %>%
  summarize(med_sum_full=median(summer_wc30, na.rm=TRUE))

meansumfull <- sppdat_nodup %>%
  group_by(species) %>%
  summarize(mean_sum_full=mean(summer_wc30, na.rm=TRUE))

## annual precipitation
medapsub <- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(med_ap_sub=median(ap, na.rm=TRUE))
medapsub

meanapsub <- rast_samp_wc2 %>%
  group_by(species) %>%
  summarize(mean_ap_sub=mean(ap, na.rm=TRUE))
meanapsub

medapfull <- sppdat_nodup %>%
  group_by(species) %>%
  summarize(med_ap_full=median(ap_wc30, na.rm=TRUE))
medapfull

meanapfull <- sppdat_nodup %>%
  group_by(species) %>%
  summarize(mean_ap_full=mean(ap_wc30, na.rm=TRUE))
meanapfull

plot(meanmatfull$mean_mat_full, meanmatsub$mean_mat_sub)
abline(0,1)

climsppsum <- left_join(medmatsub, meanmatsub, by="species") %>%
  left_join(., medmatfull, by="species") %>%
  left_join(., meanmatfull, by="species") %>%
  left_join(., medsumsub, by="species") %>%
  left_join(., meansumsub, by="species") %>%
  left_join(., medsumfull, by="species") %>%
  left_join(., meansumfull, by="species") %>%
  left_join(., medapsub, by="species") %>%
  left_join(., meanapsub, by="species") %>%
  left_join(., medapfull, by="species") %>%
  left_join(., meanapfull, by="species") 

climsppsum

###### range area esimation

projection(cn) <- "+proj=longlat +datum=WGS84"
laea <- CRS("+proj=laea  +lat_0=50 +lon_0=10")
coordinates(gbifclean) <- ~decimalLongitude + decimalLatitude
proj4string(gbifclean) <- CRS("+proj=longlat +datum=WGS84")
clb <- spTransform(cn, laea)
pts <- spTransform(gbifclean, laea)

plot(clb, axes=FALSE, xlim=c(-2e06, 3e06), ylim=c(-2e06, 4e06))

ras <- raster(pts)
#raster resotion is 50km
res(ras) <-  50000
ras <- extend(ras, extent(ras)+ 100000)

# sum up the circles of presences/ CA50
xy <- coordinates(pts)
CA_samp <- xy %>% as_tibble() %>%
  mutate(sp = pts$species) %>%
  group_by(sp) %>%
  nest() %>%
  mutate(m = purrr::map(data, ~circles(.x, d=50000, lonlat=FALSE))) %>%
  mutate(CA = purrr::map(m, ~area(polygons(.x)))) %>%
  dplyr::select(-data) %>% unnest(col=CA)

CA_samp

# apply correction by dividing by the area of the circle
CA_samp$CA_cor <- CA_samp$CA / (pi * 50000^2)
plot(rev(sort(CA_samp$CA_cor)), ylab='CA50')


##### Methods for conex hull 

hull_samp <- xy %>% as_tibble() %>%
  mutate(sp = pts$species) %>%
  group_by(sp) %>% 
  distinct()%>%
  nest() %>%
  mutate(h = purrr::map(data, ~{if(nrow(.x) > 3)
  {convHull(.x, lonlat=FALSE)} else{NULL}})) %>%
  mutate(hull = purrr::map(h, ~polygons(.x))) %>%
  dplyr::select(-data) 


#plot the hulls
i <- which(!sapply(hull_samp$hull, is.null))
h <- hull_samp$hull[i]
# combine them
hh <- do.call(bind, h)
plot(hh, col="gray")
plot(clb, add=TRUE)

##  combine the data sets
hull_samp$ahull <- sapply(hull_samp$hull, function(i) ifelse(is.null(i), 0, area(i)))

range_met <- hull_samp %>% left_join(CA_samp, by="sp") %>%
  dplyr::select(-c(h,hull,m))

climsppsum <- left_join(climsppsum, range_met, by= c("species"="sp"))


write.csv(climsppsum, "sppclimsum_postclean_wc2_30s.csv")


