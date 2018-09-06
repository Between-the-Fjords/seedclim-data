#### build seedclim comm database

## Load packages ####
library("DBI")
library("RSQLite")
library("readxl")
library("tidyverse")
library("assertr")
library("readr")

#function to add data to database - padding for missing columns ####
dbPadWriteTable <- function(conn, table, value, row.names = FALSE, append = TRUE, ...){
  #get extra columns from DB
  allCols <- dbGetQuery(con, paste("select * from", table, "limit 0;"))
  value <- bind_rows(allCols, value)
  
  #add to database
  dbWriteTable(con, table, value = value, row.names = row.names, append = append, ...)
}

## make database ####
if(file.exists("../database/seedclim.sqlite")){
  file.remove("../database/seedclim.sqlite")
}
con <- dbConnect(SQLite(), dbname = "../database/seedclim.sqlite")

##set up structure ####

setup <- readChar("../databaseUtils/seedclimstructure.txt", nchar = 100000)
sapply(paste("CREATE", strsplit(setup, "CREATE")[[1]][-(1:2)]), dbExecute, conn = con)

dbListTables(con)

## optionally extract meta data from old mysql
extract_from_mysql <- FALSE
if(extract_from_mysql){
  oldDB <- dbConnect(RMySQL::MySQL(), group = "seedclim")
}

### load taxa ####
if(extract_from_mysql){
  oldDB_taxon <- dbGetQuery(oldDB, "select * from taxon")
  write_csv(oldDB_taxon, path = "../databaseUtils/taxon_table.csv")
}

taxa <- read_csv("../databaseUtils/taxon_table.csv", quote = "'") %>% 
  mutate(species = recode(
    species, 
  #old code = new code
    "Vis alp" = "Vis.alp",
    "Gent.sp" = "Gen.sp",
    "Ort sec" = "Ort.sec"
  )) %>% 
  bind_rows(read_csv(
"species,speciesName,family,functionalGroup,lifeSpan
Gale.sp,Galeopsis sp,Lamiaceae,forb,perennial
Cer,Cerastium sp??,Caryophyllaceae,forb,perennial
Ran...,Ranunculus sp,Ranunculaceae,forb,perennial
X...,???,???,NA,NA 
Dia.lan,Dianthus lanceolata!?!,Caryophyllaceae?,forb,perennial
Hol.lan,Not Holcus??,Poaceae,graminoid,perennial
Dia.med,Dianthus not media??,??,forb,perennial
X....1,???,???,NA,NA
Car.sp1,Carex sp1??,Cyperaceae,graminoid,perennial
Åkerplante,Crop plant???,Cropfamily,forb,perennial

Agr.can,Agrostis canina?,Poaceae,graminoid,perennial
Cre.pal,Crepis paludosa?,Asteraceae,forb,perennial
Frag.vir,Fragaria viridis,Rosaceae,forb,perennial
Hie.ore,Hieracium oreadea?,Asteraceae,forb,perennial
Sch.gig,Schedonorus giganteus,Poaceae,graminoid,perennial
Ste.bor,Stellaria borealis,Caryophyllaceae,forb,perennial
"))
  


dbPadWriteTable(conn = con, table = "taxon", value = taxa %>% select(species, speciesName, family, comment))

## load traits
if(extract_from_mysql){
  oldDB_traits <- dbGetQuery(oldDB, "select * from moreTraits")
  write_delim(oldDB_traits, path = "../databaseUtils/moreTraits_table.tab", delim = "\t")
}

traits <- read_delim("../databaseUtils/moreTraits_table.tab", quote = "'", delim = "\t")

all_traits <- traits %>% 
  rename(Norwegian_name = `Norwegian name`) %>% 
  full_join(taxa %>% select(-speciesName, -family, -comment)) %>% 
  group_by(species) 

all_traits %>% 
  select_if(is.numeric) %>% 
  gather(key = trait, value = value, -species) %>% 
  filter(!is.na(value)) %>% 
  dbPadWriteTable(conn = con, table = "numeric_traits", value = .)

all_traits %>% 
  select_if(is.character) %>% 
  gather(key = trait, value = value, -species) %>% 
  filter(!is.na(value)) %>% 
  dbPadWriteTable(conn = con, table = "character_traits", value = .)


## load sites ####
if(extract_from_mysql){
  oldDB_sites <- dbGetQuery(oldDB, "select * from sites")
  write_csv(oldDB_sites, path = "../databaseUtils/site_table.csv")
}

sites <- read_csv("../databaseUtils/site_table.csv") %>% 
  rename(annualPrecipitation_gridded = Annualprecipitation_gridded, temperature_level = Temperature_level, summerTemperature_gridded =  SummerTemperature_gridded, precipitation_level = Precipitation_level)

site_fields <- dbListFields(conn = con, "sites")


dbPadWriteTable(conn = con, table = "sites", value = sites %>% select(one_of(site_fields)))

## site attributes
sites %>% 
  group_by(siteID) %>% #will force addition of siteID column
  select(-one_of(site_fields)) %>% 
  gather(key = variable, value = value, -siteID) %>% 
  dbPadWriteTable(conn = con, table = "site_attributes", value = .)

  


## load blocks ####
if(extract_from_mysql){
  oldDB_blocks <- dbGetQuery(oldDB, "select * from blocks")
  write_csv(oldDB_blocks, path = "../databaseUtils/blocks_table.csv")
}

blocks <- read_csv("../databaseUtils/blocks_table.csv")
dbPadWriteTable(conn = con, table = "blocks", value = blocks)


## load plots ####
if(extract_from_mysql){
  oldDB_plots <- dbGetQuery(oldDB, "select * from plots")
  write_csv(oldDB_plots, path = "../databaseUtils/plots_table.csv")
}

plots <- read_csv("../databaseUtils/plots_table.csv")

dbPadWriteTable(conn = con, table = "plots", value = plots)

## load turfs ####
if(extract_from_mysql){
  oldDB_turfs <- dbGetQuery(oldDB, "select * from turfs")
  write_csv(oldDB_turfs, path = "../databaseUtils/turfs_table.csv")
}

turfs <- read_csv("../databaseUtils/turfs_table.csv")

dbPadWriteTable(conn = con, table = "turfs", value = turfs)



## load mergeDictionary ####
if(extract_from_mysql){
  oldDB_mergedictionary <- dbGetQuery(oldDB, "select * from mergedictionary")
  write_csv(oldDB_mergedictionary, path = "../databaseUtils/mergedictionary.csv")
  dbDisconnect(oldDB)
}

merge_dictionary <- read_csv("../databaseUtils/mergedictionary.csv") %>% 
  mutate(newID = recode(newID, "Vis alp" = "Vis.alp")) %>% 
  bind_rows(
    read_csv(
"oldID,newID
Salix.graa,Sal.sp
Vis.alp,Vis.alp
Gen.sp.,Gen.sp
Car.Cap,Car.cap
Galeopsis.sp,Gale.sp
Seedlings,NID.seedling"))


## load main data ####
source("inst/uploadDataSource/importcommunityNY_test_16.des.2013.r")

## get list of data files
datafiles <- dir(path = "rawdata/", pattern = "csv", full.names = TRUE, recursive = TRUE)

#check taxonomy
extra <- c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "RTtreat", "GRtreat",   "subPlot", "year",  "date",  "Measure", "recorder", "pleuro", "acro",  "liver", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "comment",  "X",  "X.1", "X.2", "missing") 

datafiles %>% 
  set_names %>% 
  map(function(x){
  print(x)
  f <- read.table(x, header = TRUE, sep = ",", nrows = 2, comment = "")
  if(ncol(f) == 1){
    f <- read.table(x, header = TRUE, sep = ";", nrows = 2, comment = "")  
  }
  setdiff(names(f), c(extra, merge_dictionary$oldID, taxa$species))
  })

datafiles %>% 
  map(import_data, con = con, merge_dictionary = merge_dictionary)

