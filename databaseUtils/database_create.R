#### build seedclim comm database

## Load packages ####
library("DBI")
library("RSQLite")
library("readxl")
library("tidyverse")
library("assertr")
library("conflicted")
conflict_prefer("filter", "dplyr")

#function to add data to database - padding for missing columns ####
db_pad_write_table <- function(conn, table, value, row.names = FALSE,
                            append = TRUE, ...) {
  #get extra columns from DB
  all_cols <- dbGetQuery(con, paste("select * from", table, "limit 0;"))
  value <- bind_rows(all_cols, value)

  #add to database
  dbWriteTable(con, table, value = value,
               row.names = row.names, append = append, ...)
}

## make database ####
if (file.exists("database/seedclim.sqlite")) {
  file.remove("database/seedclim.sqlite")
}
con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

##set up structure ####

setup <- readChar("databaseUtils/setup-data/seedclimstructure.txt", nchar = 100000)
sapply(paste("CREATE", strsplit(setup, "CREATE")[[1]][-(1:2)]),
       dbExecute, conn = con)

dbListTables(con)

## optionally extract meta data from old mysql
extract_from_mysql <- FALSE
if (extract_from_mysql) {
  old_DB <- dbConnect(RMySQL::MySQL(), group = "seedclim")
}

### load taxa ####
if (extract_from_mysql) {
  oldDB_taxon <- dbGetQuery(old_DB, "select * from taxon")
  write_csv(oldDB_taxon, path = "databaseUtils/setup-data/taxon_table.csv")
}

taxa <- read_csv("databaseUtils/setup-data/taxon_table.csv", quote = "'") %>%
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
Pop.tre,Populus tremula,Salicaceae,woody,perennial
Ped.pal,Pedicularis palustris,Orobanchaceae,forb,perennial
Are.ser,Arenaria serpyllifolia,Carophyllaceae,forb,annual
Sch.pra,Schedonorus pratensis,Poaceae,graminoid,perennial
Luz.syl,Luzula sylvatica,Juncaceae,graminoid,perennial
Sal.lan,Salix lanata,Salicaceae,woody,perennial
Ver.ver,Veronica verna,Plantaginaceae,forb,annual 
"))
  

taxa %>% 
  select(species, species_name = speciesName, family, comment) %>% 
  db_pad_write_table(conn = con, table = "taxon", value = .)

## load traits
if (extract_from_mysql) {
  oldDB_traits <- dbGetQuery(old_DB, "select * from moreTraits")
  write_delim(oldDB_traits, path = "databaseUtils/setup-data/moreTraits_table.tab",
              delim = "\t")
}

species_attributes <- read_delim("databaseUtils/setup-data/moreTraits_table.tab",
                                 quote = "'", delim = "\t")

species_attributes <- species_attributes %>%
  rename(
   rarity = `Common-rear`,
   polyploid_2n = `Polyploid (2n)`, 
   
   flowers_spring = `F-V<e5>r`,
   flowers_early_summer = `F-Fso`, 
   flowers_mid_summer = `F-Mso`, 
   flowers_late_summer = `F-Sso`, 
   flowers_autumn = `F-H<f8>`,
   
   nemoral = Nem, 
   boreal_nemoral = BNem,
   south_boreal = SBor,
   mid_boreal = MBor,
   north_boreal = Nbor,
   low_alpine = LAlp,
   mid_alpine = MAlp,
   high_alpine = HAlp
  ) %>% 
  rename_with(.cols = matches(" "), ~str_replace(., " ", "_")) %>% 
  rename_with(.cols = c(-Lids_page, -`Mossberg_page`), tolower) %>% 
  rename_with(.cols = c(lower, upper), ~paste0(., "_vegetation_zone")) %>%  
  full_join(select(taxa, species, functional_group = functionalGroup, lifespan = lifeSpan)) %>%
  mutate(across(starts_with("flowering_"), ~ recode(.,
                                                    "MSo" = "mid-summer", 
                                                    "H<f8>st" = "autumn",
                                                    "FSo" = "early summer",
                                                    "SSo" = "late summer", 
                                                    "V<e5>r" = "spring"))) %>% 
  group_by(species)

bind_rows(
  character_attributes = species_attributes %>% 
    select(!where(is.numeric)) %>% 
    pivot_longer(-species, names_to = "attribute", values_to = "value_character"), 
  
  numeric_attributes = species_attributes %>%
    select(!where(is.character)) %>% 
    pivot_longer(-species, names_to = "attribute", values_to = "value_numeric")
) %>%
  filter(!(is.na(value_character) & is.na(value_numeric))) %>% 
  db_pad_write_table(conn = con, table = "species_attributes", value = .)



## load sites ####
cat("sites upload")
if (extract_from_mysql) {
  oldDB_sites <- dbGetQuery(old_DB, "select * from sites")
  write_csv(oldDB_sites, path = "databaseUtils/setup-data/site_table.csv")
}

sites <- read_csv("databaseUtils/setup-data/site_table.csv")

site_fields <- dbListFields(conn = con, "sites")

sites <- sites %>% 
  rename(norwegian_name = site_name) %>% 
  mutate(
    site_code = str_replace(site_code, "low", "bor"), 
    site_code = str_replace(site_code, "int", "sub"), 
    biogeographic_zone = case_when(
      str_detect(site_code, "alp") ~ "Low-alpine", 
      str_detect(site_code, "sub") ~ "Sub-alpine", 
      str_detect(site_code, "bor") ~ "North-boreal"
    ),
    biogeographic_section = case_when(
      str_detect(site_code, "1") ~ "O2", 
      str_detect(site_code, "2|3") ~ "O1", 
      str_detect(site_code, "4") ~ "OC"
    )
  )


db_pad_write_table(conn = con, table = "sites",
                value = sites %>% select(all_of(site_fields)))

## site attributes
sites_attributes <- sites %>% 
  group_by(siteID) %>% 
  select(-any_of(site_fields)) %>% 
  select(where(~!all(is.na(.))))

bind_rows(
  character_attributes = sites_attributes %>% 
    select(!where(is.numeric)) %>% 
    pivot_longer(-siteID, names_to = "attribute", values_to = "value_character"), 
  numeric_attributes = sites_attributes %>%
    select(!where(is.character)) %>% 
    pivot_longer(-siteID, names_to = "attribute", values_to = "value_numeric")
) %>% 
  db_pad_write_table(conn = con, table = "site_attributes", value = .)

  


## load blocks ####
if (extract_from_mysql) {
  oldDB_blocks <- dbGetQuery(old_DB, "select * from blocks")
  write_csv(oldDB_blocks, path = "databaseUtils/setup-data/blocks_table.csv")
}

blocks <- read_csv("databaseUtils/setup-data/blocks_table.csv") %>% 
  mutate(siteID = recode(siteID,  
                         "Ovstedal" = "Ovstedalen" , 
                         "Skjellingahaugen" = "Skjelingahaugen", 
                         "Ulvhaugen" = "Ulvehaugen"))

db_pad_write_table(conn = con, table = "blocks", value = blocks)


## load plots ####
if (extract_from_mysql) {
  oldDB_plots <- dbGetQuery(old_DB, "select * from plots")
  write_csv(oldDB_plots, path = "databaseUtils/setup-data/plots_table.csv")
}

plots <- read_csv("databaseUtils/setup-data/plots_table.csv")

db_pad_write_table(conn = con, table = "plots", value = plots)

## load turfs ####
if (extract_from_mysql) {
  oldDB_turfs <- dbGetQuery(old_DB, "select * from turfs")
  write_csv(oldDB_turfs, path = "databaseUtils/setup-data/turfs_table.csv")
}

turfs <- read_csv("databaseUtils/setup-data/turfs_table.csv")

db_pad_write_table(conn = con, table = "turfs", value = turfs)



## load mergeDictionary ####
if (extract_from_mysql) {
  oldDB_mergedictionary <- dbGetQuery(old_DB, "select * from mergedictionary")
  write_csv(oldDB_mergedictionary, path = "databaseUtils/setup-data/mergedictionary.csv")
  dbDisconnect(old_DB)
}

merge_dictionary <- read_csv("databaseUtils/setup-data/mergedictionary.csv") %>%
  mutate(newID = recode(newID, "Vis alp" = "Vis.alp")) %>%
  bind_rows(
    read_csv(comment = "#",
"oldID,newID
Salix.graa,Sal.sp
Vis.alp,Vis.alp
Gen.sp.,Gen.sp
Car.Cap,Car.cap
Galeopsis.sp,Gale.sp
Seedlings,NID.seedling
Sax.aiz.,Sax.aiz
Dry.sp,Gym.dry
Tof.cal,Tof.pus
#2019 additions
Cir.vul,Cir.pal
Cirsium.sp,Cir.pal
Solidago,Sol.vir
Pin.sax,Pim.sax
Pinguicula.sp.,Pin.vul
Car.var,Car.vag
Dac.alp,Dac.glo
Gal.sp,Gal.uli
"))

## attributes
attributes <- read_csv("databaseUtils/setup-data/atttibute_table.csv")

db_pad_write_table(conn = con, table = "attributes", value = attributes)


## load main data ####
source("databaseUtils/R/importcommunity.r")

## get list of data files
datafiles <- dir(path = "rawdata/", pattern = "csv$",
                 full.names = TRUE, recursive = TRUE)

#exclude 2019 raw files (keep processed)
datafiles <- str_subset(datafiles, pattern = "2019_data", negate = TRUE)

#check taxonomy
meta_cols <- c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat",
           "destinationPlotID", "turfID", "RTtreat", "GRtreat", "subPlot",
           "year",  "date",  "Measure", "recorder", "pleuro", "acro",  "liver",
           "lichen", "litter", "soil", "rock", "totalVascular",
           "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight",
           "comment",  "X",  "X.1", "X.2", "missing")

## process 2019 data
source("databaseUtils/R/2019_temp.R")


datafiles %>% #grep("2017", ., value = TRUE) %>%
  set_names %>%
  map(function(x) {
  print(x)
  f <- read.table(x, header = TRUE, sep = ",", nrows = 2, comment = "")
  if (ncol(f) == 1) {
    f <- read.table(x, header = TRUE, sep = ";", nrows = 2, comment = "")
  }
  setdiff(names(f), c(meta_cols, merge_dictionary$oldID, taxa$species))
  })


datafiles %>% #grep("2017", ., value = TRUE) %>%
  map(import_data, con = con, merge_dictionary = merge_dictionary)


## do corrections

source("databaseUtils/R/speciesCorrections.R")
