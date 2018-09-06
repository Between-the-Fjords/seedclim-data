#Script for making a set of 1k phylogenies for analyses
#Brian Maitner, bmaitner@gmail.com

#########################################################################

#Loading needed data and functions

library(ape)
library(phytools)
library(BIEN)
library("tidyverse")
library("DBI")# also needs RSQLite installed

## ---- load_community

#make database connection
con <- dbConnect(RSQLite::SQLite(), "database/seedclim.sqlite")

#get taxonomy table
taxa <- tbl(con, "taxon") %>%
  collect()
taxa<-as.data.frame(taxa)
dbDisconnect(con)
rm(con)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Windows code for creating sunplin.spn object needed by sunplin code
#cd "C:\Users\Brian\Desktop\current_projects\Park_DNH_scale"
#R CMD SHLIB sunplin-r.cpp --output=sunplin.spn

source("phylogeny/sunplin-functions.r")
source("phylogeny/r_functions/sunplin_fxs.R")
dyn.load("C:/Users/Brian/Desktop/current_projects/misc_R_code/sunplin/sunplin/sunplin.spn")

#load in Smith phylogeny
gbotb<-read.tree("phylogeny/Smith_2017_gbotb.tre")


#########################################################################

#clean up names (yes, I know I should use tidyr....)
taxa$speciesName<-gsub(pattern = ".",replacement = "",x = taxa$speciesName,fixed = T)
taxa$speciesName<-gsub(pattern = "?",replacement = "",x = taxa$speciesName,fixed = T)
taxa$speciesName<-gsub(pattern = "!",replacement = "",x = taxa$speciesName,fixed = T)
taxa<-taxa[!taxa$speciesName%in%c("","Crop plant", "Not Holcus"),]
taxa<-taxa[which(!is.na(taxa$speciesName)),]
taxa$speciesName<-gsub(pattern = "not media",replacement = "sp1",x = taxa$speciesName,fixed = T)
taxa<-taxa[grep(pattern = "sp\\b",x = taxa$speciesName,invert = T),]
taxa$family[grep(pattern = "Dianthus",taxa$speciesName)]<-"Caryophyllaceae"



#Make sunplin files
sp_fam<-taxa[c("speciesName","family")]
puts_info_seedclim<-get_put_info(sp_fam = sp_fam,phylogeny = gbotb)
make_puts_input(puts_info = puts_info_seedclim,phylogeny = gbotb,phylogeny_filename = "phylogeny/gbotb_annotated.tre",
                        puts_filename = "phylogeny/seedclim.puts")

sunplin_phylo_replicates(put_file = "seedclim.puts",phylogeny_file = "gbotb_annotated.tre",
                         output_directory = "C:/Users/Brian/Desktop/current_projects/seedclimComm/phylogeny/phylogenies/",
                         output_base_filename = "gbotb_base_rep_",nrep = 1000,method = 2,
                         directory = "C:/Users/Brian/Desktop/current_projects/seedclimComm/phylogeny/",
                         taxa_to_keep<-puts_info_seedclim$speciesName)



