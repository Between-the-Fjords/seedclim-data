#load packages
library("tidyverse")
library("DBI")# also needs RSQLite installed




## ---- load_community

#make database connection
con <- dbConnect(RSQLite::SQLite(), "database/seedclim.sqlite")

#load cover data and metadata
#cover
source("R/importSource/loadCover.r")

#subturf frequencies
source("R/importSource/loadSubplotfreq.r")

#get taxonomy table
taxa <- tbl(con, "taxon") %>%
  collect()
