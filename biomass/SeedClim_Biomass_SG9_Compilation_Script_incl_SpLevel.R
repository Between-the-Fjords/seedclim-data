### SG.9 Biomass Allocation  ###
# Data Cleaning: Sonya R. Geange

# This contains the data found within:
# 2010 and 2012: Serge Farina's Msc thesis and initial PhD work
# 2013: Data collected from Serge Farina + Sigrid Skriverik Bruvoll
# 2014: Data collected from Aksel Anker Henriksen and Benjamin Scwarz
# 2015: Data collected by Terezie Novakova and Tabea Gallhuser

# Data is saved across multiple files, and in some instances pre-aggregated.
# I.e. the 2013-2015 summary .xslx

# Load Packages

#packages
library(dplyr)
library(tidyr)
library(fs)
library(readxl)
library(tidyverse)
library("tidylog", warn.conflicts = FALSE)

## DOWNLOAD DATA
# Get data from OSF Repository
# Use this code to download the data directly from OSF and unzip the file.
# Alternatively you can find the raw data here: https://osf.io/npfa9/

### Download data from OSF
#install.packages("remotes")
#remotes::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
get_file(node = "npfa9",
         file = "SG.9_Biomass_Raw.zip",
         path = "biomass/data",
         remote_path = "6_Biomass_data/Raw_data")

zipFile <- "biomass/data/SG.9_Biomass_Raw.zip"
outDir <- "biomass/data/"
unzip(zipFile, exdir = outDir)



#############################################################
# Read Serge Farina's 2010 and 2012 work
# Conducted during summer field seasons
# Compiled doc has summary outputs from newly found 2012 only file with
# species level data 'SFarinas-Biomass 2012.xlsx'.
# Here for functional groups, used compiled, below for species we use the recent file
#############################################################
BM_2010_2012 <- read_excel("biomass/data/SFarinas_Compiled_data_for_SEEDCLIM_2010_and_2012.xlsx",sheet =1, col_names = TRUE)

# Drop first row with units
BM_2010_2012 = BM_2010_2012[-1,]


# Drop unnessary columns
BM_2010_2012 <- BM_2010_2012 %>% 
  select(-c(Precip, Temp, `Precip level`, `Temp Level`, Slope, Aspect,
            Site, Block,
            # Chemical attributes now found in SeedClim Chemistry datadoc
            pH, NO3N, NH4N, AvailN,
            # Soil depth and plant chemistry
            `Soil depth 1`, `Soil depth 2`, `Plant comm CN`,
            # And Vasc, as it's just a sum.
            Vasc,
            # Agbiomass is summ of forb, graminoid, bryo and is 'live' in other datasets
            # It's not clear what 'Total Bio' is a sum of, but we're no longer including aggregates
            Agbiomass, `Total bio`,
  )) %>% 
  rename(Root_bio_1 = `Root bio 1`, Root_bio_2 = `Root bio 2`)

# ?? What about woody? Has it already been added into the Forbs? Does it need to be?
# In  later files, i.e. 2013/14/15 on occasionals 'forbs' is forbs + odd woody species..

# Convert type
BM_2010_2012$Site <- as.character(BM_2010_2012$Site)
BM_2010_2012$Root_bio_1 <- as.numeric(BM_2010_2012$Root_bio_1)
BM_2010_2012$Root_bio_2 <- as.numeric(BM_2010_2012$Root_bio_2)
BM_2010_2012$Forb <- as.numeric(BM_2010_2012$Forb)
BM_2010_2012$Bryo <- as.numeric(BM_2010_2012$Bryo)
BM_2010_2012$Gram <- as.numeric(BM_2010_2012$Gram)
BM_2010_2012$Woody <- as.numeric(BM_2010_2012$Woody)
BM_2010_2012$Lichen <- as.numeric(BM_2010_2012$Lichen)
BM_2010_2012$Litter <- as.numeric(BM_2010_2012$Litter)

# Back-transform to the original units (g/0.625m2 instead of g/m2) (divide by 16)
BM_2010_2012 <- BM_2010_2012 %>% 
  mutate(Forb = (Forb/16),
         Root_bio_1 = (Root_bio_1/16),
         Root_bio_2 = (Root_bio_2/16),
         Bryo = (Bryo/16),
         Gram = (Gram/16),
         Woody = (Woody/16),
         Lichen = (Lichen/16),
         Litter = (Litter/16))

# Reformat
BM_2010_2012$Grass <- as.numeric(BM_2010_2012$Grass)
BM_2010_2012$Carex <- as.numeric(BM_2010_2012$Carex)

# To match 2013/14/15 data
BM_2010_2012 <- BM_2010_2012 %>% 
  mutate(Forb_Woody = as.numeric(rowSums(cbind(Forb, Woody), na.rm = T)))

#################################################################
# Read in 2013, 2014 and 2015 datasets
#################################################################
# NB. Two dataset options for 2013, one with species level data 'Biomasse 2013 Sigrid.xlsx'
# And the equivilant summed functional groups, formatted with 2014 and 2015 data
# which is in 'Biomass grassland 2013-2015.xls'.
#
# Here, we'll use the functional group summaries, and below the species level file.
#
# Note: In 2010/12/13/14/15, we have seperate data for each element than other years:
# Gram = Grass and Carex
# Forbs = Forbs and Woody (or 'Shrubs')
# Litter = Sum of Litter and 
# Where possible we retain the 'raw' data, and rename variables to reflect these ambiguities
#############################################################


#################################################################
# Read in 2013 
#################################################################

BM_2013 <- read_excel("biomass/data/Biomass grassland 2013-2015.xls",
                      sheet =1, col_names = TRUE)


# Remove columns that were blank
BM_2013 <- BM_2013 %>% 
  select(-c(Bryo...3:blank_3, Vasc,
            `Total Biomass`, `Total(S)`)) %>% 
  # Relabel the 'raw' functional group weights
  rename(c(Bryo = Bryo...19, Gram = Gram...21,
           Forb = Forbs...17,Litter = Litter...25))
# Add year
BM_2013$year <- 2013

BM_2013$Bryo <- as.numeric(BM_2013$Bryo)
BM_2013$Forb <- as.numeric(BM_2013$Forb)
BM_2013$Litter <- as.numeric(BM_2013$Litter)
BM_2013$Lichen <- as.numeric(BM_2013$Lichen)
BM_2013$Gram <- as.numeric(BM_2013$Gram)
BM_2013$Woody <- as.numeric(BM_2013$Woody)

# Note: In this doc for 2014/15, 'Forbs' is actually forbs + the odd woody species
# Here, to avoid confusion we add these two together
BM_2013 <- BM_2013 %>% 
  mutate(Forb_Woody = as.numeric(rowSums(cbind(Forb, Woody), na.rm = T)))


#################################################################
# 2014 work
# No species level data for this year
#############################################################
BM_2014 <- read_excel("biomass/data/Biomass grassland 2013-2015.xls",
                      sheet =2, col_names = TRUE)
# Select by column position
positions <- c(1:6,20:21)
BM_2014 <- BM_2014 %>% select(positions)
# Add year
BM_2014$year <- 2014

BM_2014$Bryo <- as.numeric(BM_2014$Bryo)
BM_2014$Forbs <- as.numeric(BM_2014$Forbs)
BM_2014$Litter <- as.numeric(BM_2014$Litter)
BM_2014$Gram <- as.numeric(BM_2014$Gram)

# Note, 'Forb' is actuall Forbs and the odd woody plant, so relabel as such
BM_2014$Forb_Woody <- BM_2014$Forbs
# Remove original 'Forbs' to avoid confusion
BM_2014 <- BM_2014 %>%  select(-c(Forbs))
# Litter here, is also litter + lichen (though minimal lichen)
# But are distinct categories elsewhere so adjust here
BM_2014<- BM_2014 %>% rename(Litter_Lichen = Litter)

#################################################################
# 2015 work
# No species level data for this year
################################################################
BM_2015 <- read_excel("biomass/data/Biomass grassland 2013-2015.xls",
                      sheet =3, col_names = TRUE)
# Select by column position
positions <- c(1:7)
BM_2015 <- BM_2015 %>% select(positions)

BM_2015$Bryo <- as.numeric(BM_2015$Bryo)
BM_2015$Forb <- as.numeric(BM_2015$Forbs)
BM_2015$Litter <- as.numeric(BM_2015$Litter)
BM_2015$Gram <- as.numeric(BM_2015$Gram)

# Add year
BM_2015$year <- 2015

# In 2013 and 2014 the odd woody specimen was added into 'forbs', and in 2010/12 is 'woody'
# Here we merge 'shrubs' into 'forbs to have these the same, but because forb is an actual
# category, we'll retain the original here.

BM_2015$Woody <- as.numeric(as.character(BM_2015$Shrubs))
BM_2015 <- BM_2015 %>% select(-c(Forbs, Shrubs))
BM_2015 <- BM_2015 %>% 
  mutate(Forb_Woody = as.numeric(rowSums(cbind(Forb, Woody), na.rm = T)))

# Litter here, is also litter + lichen (though minimal lichen)
# But are distinct categories elsewhere so adjust here
BM_2015<- BM_2015 %>% rename(Litter_Lichen = Litter)

#################################################################
# Merge Dataframes Together
################################################################

# Long Format
BM_2010_2012_long <- BM_2010_2012 %>% 
  pivot_longer(cols = Root_bio_1:Forb_Woody, names_to = "Biomass_Attributes",
               values_to = "Biomass_Values",
               values_drop_na = TRUE)

BM_2013_long <- BM_2013 %>% 
  pivot_longer(cols = c(Forb:Litter, Forb_Woody), names_to = "Biomass_Attributes",
               values_to = "Biomass_Values",
               values_drop_na = TRUE)

BM_2014_long <- BM_2014 %>% 
  pivot_longer(cols = c(Bryo:Litter_Lichen, Forb_Woody), names_to = "Biomass_Attributes",
               values_to = "Biomass_Values",
               values_drop_na = TRUE)

BM_2015_long <- BM_2015 %>% 
  pivot_longer(cols = c(Bryo:Forb, Woody:Forb_Woody), names_to = "Biomass_Attributes",
               values_to = "Biomass_Values",
               values_drop_na = TRUE)

###################################################################################
# Merge the dataframes
SG.9.Biomass <- bind_rows(BM_2010_2012_long, BM_2013_long, BM_2014_long, BM_2015_long)

#####################################################
# Check Date Format & Recode to match data dictionary
SG.9.Biomass <- SG.9.Biomass %>% 
  rename(samplingDate = Sampled, sortingDate = Sorted)
head(unique(SG.9.Biomass$samplingDate))
head(unique(SG.9.Biomass$sortingDate))

# Recode dates to match new format (yyyy-mm-dd) - Sampling Date
library(lubridate)
SG.9.Biomass$samplingDate <- as.Date(SG.9.Biomass$samplingDate, format = "%d/%m/%Y")
head(unique(SG.9.Biomass$samplingDate))

# Recode dates to match new format (yyyy-mm-dd) - Sorting Date
SG.9.Biomass$sortingDate <- as.Date(SG.9.Biomass$sortingDate, format = "%d/%m/%Y")
head(unique(SG.9.Biomass$sortingDate))

################################################################
# Check site, block and plot formatting
################################################################
unique(SG.9.Biomass$siteID)
unique(SG.9.Biomass$Site)
SG.9.Biomass$siteID <- recode(SG.9.Biomass$siteID, 
                              'Gudmedalen' = "Gudmedalen",
                              'Lavisdalen' = "Lavisdalen",
                              'Rambera' = "Rambera",
                              'Rambaera' = "Rambera",
                              'Ulvhaugen' = "Ulvehaugen",
                              'Skjellingahaugen' = "Skjelingahaugen",
                              'Alrust' = "Alrust",
                              'Arhelleren' = "Arhelleren",
                              'Fauske' = "Fauske",
                              'Hogsete' = "Hogsete",
                              'Ovstedal' = "Ovstedalen",
                              'Ovstedalen' = "Ovstedalen",
                              'Vikesland' = "Vikesland",
                              'Veskre' = "Veskre")

SG.9.Biomass$Site <- recode(SG.9.Biomass$Site, 
                            'Gud' = "Gudmedalen",
                            'Lav' = "Lavisdalen",
                            'Ram' = "Rambera",
                            'Ulv' = "Ulvehaugen",
                            'Skj' = "Skjelingahaugen",
                            'Alr' = "Alrust",
                            'Arh' = "Arhelleren",
                            'Fau' = "Fauske",
                            'Hog' = "Hogsete",
                            'Ovs' = "Ovstedalen",
                            'Vik' = "Vikesland",
                            'Ves' = "Veskre")

# Link to the Site and siteID as they're now the same, just different years
SG.9.Biomass <- SG.9.Biomass %>% 
  unite("siteID", c(siteID,Site), remove = TRUE, na.rm = TRUE)


#################################################
## STILL TO DO???
#################################################
## Figure out the Plot vs plotID and is Plot really just block?
# Check blockID names (appear ok for those with them, check if we can allocate missing)
unique(SG.9.Biomass$blockID)

# Check plotID codes (NB. One is missing (NA), cannot find data to interpret this yet)
unique(SG.9.Biomass$plotID)
unique(SG.9.Biomass$Plot) # Not unique among sites, so need to be resolved

# Check turfID codes
# ? If need be: Remove rows labelled with RTC, which is a different experiment. 
# Chat with Aud 18.5.2021, all of 2010 might dissapear, are TTC meant to have been cut? For example plot 286, TTC plot from Block One 
# And are the RTC meant to be in SG.9, or SG.7 Though that was 2011-2016.
# Format might also need to be changed
unique(SG.9.Biomass$turfID)
#SG.9.Biomass <- dplyr::filter(SG.9.Biomass, !grepl('RTC', turfID))


write_csv(SG.9.Biomass, "data/processed/seedclim_SG_9_biomass_functional_groups.csv")



#################################################################
# Species Only DataFrame
# 2012 and 2013 years
################################################################

# Import the species-level 2012 dataset
BM_2012_sp <- read_excel("biomass/data/SFarinas-Biomass 2012.xlsx",
                         sheet =1, col_names = TRUE)
# Drop first row with units
BM_2012_sp = BM_2012_sp[-1,]
# Add year
BM_2012_sp$year <- 2012

# Remove aggregate columns + precip/temp site characterstics
BM_2012_sp <- BM_2012_sp %>% 
  select(-c(Precip_level:Temp_level, Precip:Temp, Forbs_raw:`_...162`))

# Change units
BM_2012_sp[3:134] <- lapply(BM_2012_sp[3:134], as.numeric)

BM_2012_sp_long <- BM_2012_sp %>% 
  pivot_longer(cols = Ach_mil:Vio_tri,
               names_to = "species", values_to = "Biomass_g",
               values_drop_na = TRUE)

#########################################################################
# Import the 2013 species-level file
# Select only site and species level info, not the repeated summaries
BM_2013_sp <- read_excel("biomass/data/Biomasse 2013 Sigrid.xlsx",
                         sheet =1, col_names = TRUE)

# Add year
BM_2013_sp$year <- 2013

BM_2013_sp <- BM_2013_sp %>% 
  select(-c(Precip_level:Temp_level, Precip:Temp,Forbs:Mean_litter))

# Change units
BM_2013_sp[3:134] <- lapply(BM_2013_sp[3:134], as.numeric)

# Pivot longer, and remove na values
BM_2013_sp_long <- BM_2013_sp %>% 
  pivot_longer(cols = Ach_mil:Vio_tri,
               names_to = "species", values_to = "Biomass_g",
               values_drop_na = TRUE)

# Merge 2012 and 2013 species level biomass data
SG.9.Species_Combined <- bind_rows(BM_2012_sp_long, BM_2013_sp_long)




#################################################################
# Data cleaning'SG.9.Species_Combined' 
#################################################################

unique(SG.9.Species_Combined$Site)
SG.9.Species_Combined$siteID <- recode(SG.9.Species_Combined$Site, 
                                       'Gudmedalen' = "Gudmedalen",
                                       'Låvisdalen' = "Lavisdalen",
                                       'Rambaera' = "Rambera",
                                       'Ulvhaugen' = "Ulvehaugen",
                                       'Skjellingahaugen' = "Skjelingahaugen",
                                       'Alrust' = "Alrust",
                                       'Arhelleren' = "Arhelleren",
                                       'Fauske' = "Fauske",
                                       'Hogsete' = "Hogsete",
                                       'Øvestedal' = "Ovstedalen",
                                       'Ovstedalen' = "Ovstedalen",
                                       'Vikesland' = "Vikesland",
                                       'Veskre' = "Veskre")

SG.9.Species_Combined <- SG.9.Species_Combined %>% select(-c(Site))

# Reformat species, changing '_' to '.'
SG.9.Species_Combined$species <- gsub("_","\\.", SG.9.Species_Combined$species)

write_csv(SG.9.Species_Combined, "data/processed/seedclim_SG_9_biomass_species.csv")

