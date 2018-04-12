#### build seedclim comm database

## Load packages ####
library("DBI")
library("RSQLite")
library("readxl")
library("tidyverse")
library("assertthat")
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

### load taxa ####
taxa <- read_csv("../databaseUtils/taxon_table.csv", quote = "'")
taxa[taxa == "NULL"] <- NA
taxa <- taxa %>% mutate_at(vars(height:SLA), as.numeric)

dbPadWriteTable(conn = con, table = "taxon", value = taxa)

