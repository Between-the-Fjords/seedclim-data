library(DBI)
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")
filelistall<-dir(path = "rawdata/", full.names = TRUE, pattern = "csv$")

source("inst/uploadDataSource/importcommunityNY_test_16.des.2013.r")
import.data(c(filelistall), con)     #run me

source("inst/uploadDataSource/speciesCorrections.R")



#wipe()
close(con)