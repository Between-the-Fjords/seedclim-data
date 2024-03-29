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
outDir <- "biomass/data/raw"
unzip(zipFile, exdir = outDir)



#############################################################
# Read Serge Farina's 2010 and 2012 work
# Conducted during summer field seasons
# Compiled doc has summary outputs from newly found 2012 only file with
# species level data 'SFarinas-Biomass 2012.xlsx'.
# Here for functional groups, used compiled, below for species we use the recent file
#############################################################
BM_2010_2012 <- read_excel("cleaning_code/6_biomass_data/data/raw/SFarinas_Compiled_data_for_SEEDCLIM_2010_and_2012.xlsx",sheet =1, col_names = TRUE)

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
#BM_2010_2012$Site <- as.character(BM_2010_2012$Site)
BM_2010_2012$Root_bio_1 <- as.numeric(BM_2010_2012$Root_bio_1)
BM_2010_2012$Root_bio_2 <- as.numeric(BM_2010_2012$Root_bio_2)
BM_2010_2012$Forb <- as.numeric(BM_2010_2012$Forb)
BM_2010_2012$Bryo <- as.numeric(BM_2010_2012$Bryo)
BM_2010_2012$Gram <- as.numeric(BM_2010_2012$Gram)
BM_2010_2012$Woody <- as.numeric(BM_2010_2012$Woody)
BM_2010_2012$Lichen <- as.numeric(BM_2010_2012$Lichen)
BM_2010_2012$Litter <- as.numeric(BM_2010_2012$Litter)
BM_2010_2012$Grass <- as.numeric(BM_2010_2012$Grass)
BM_2010_2012$Carex <- as.numeric(BM_2010_2012$Carex)

# Back-transform to the original units (g/0.625m2 instead of g/m2) (divide by 16)
BM_2010_2012 <- BM_2010_2012 %>% 
  mutate(Forb = (Forb/16),
         Root_bio_1 = (Root_bio_1/16),
         Root_bio_2 = (Root_bio_2/16),
         Bryo = (Bryo/16),
         Gram = (Gram/16),
         Grass = (Grass/16),
         Carex = (Carex/16),
         Woody = (Woody/16),
         Lichen = (Lichen/16),
         Litter = (Litter/16))


# To match 2013/14/15 data
BM_2010_2012 <- BM_2010_2012 %>% 
  mutate(Forb_Woody = as.numeric(rowSums(cbind(Forb, Woody), na.rm = T)))

# add missing turfID using ugly trick
BM_2010_2012 <- BM_2010_2012 |> 
  mutate(newturf = rep(c("RTC", "TTC"), 90)) |>
  mutate(turfID = if_else(year == 2010 & is.na(turfID), newturf, turfID),
         turfID = if_else(turfID == "RTC", paste0(blockID, turfID, "x"), turfID),
         turfID = case_when(year == 2010 & blockID == "Ram1" & turfID == "TTC" ~ "203 TTC",
                            year == 2010 & blockID == "Ram2" & turfID == "TTC" ~ "528 TTC",
                            year == 2010 & blockID == "Ram3" & turfID == "TTC" ~ "525 TTC",
                            TRUE ~ turfID),
         plotID = case_when(year == 2010 & blockID == "Ram1" & turfID == "203 TTC" ~ 203,
                            year == 2010 & blockID == "Ram2" & turfID == "528 TTC" ~ 528,
                            year == 2010 & blockID == "Ram3" & turfID == "525 TTC" ~ 525,
                            year == 2010 & blockID == "Ram1" & turfID == "Ram1RTCx" ~ 20720,
                            year == 2010 & blockID == "Ram2" & turfID == "Ram2RTCx" ~ 20700,
                            year == 2010 & blockID == "Ram3" & turfID == "Ram3RTCx" ~ 20710,
                            TRUE ~ plotID),
         blockID = case_when(year == 2010 & blockID == "Ram1" & turfID == "203 TTC" ~ "Ram6",
                             year == 2010 & blockID == "Ram2" & turfID == "528 TTC" ~ "Ram8",
                             year == 2010 & blockID == "Ram3" & turfID == "525 TTC" ~ "Ram9",
                            TRUE ~ blockID)) |> 
  select(-newturf)

# remove 2012, because it is a copy of 2013
BM_2010 <- BM_2010_2012 |> 
  tidylog::filter(year != 2012) |> 
  select(-Root_bio_1, -Root_bio_2)


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

BM_2013 <- read_excel("cleaning_code/6_biomass_data/data/raw/Biomass grassland 2013-2015.xls",
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
BM_2014 <- read_excel("cleaning_code/6_biomass_data/data/raw/Biomass grassland 2013-2015.xls",
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
BM_2015 <- read_excel("cleaning_code/6_biomass_data/data/raw/Biomass grassland 2013-2015.xls",
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
BM_2010_long <- BM_2010 %>% 
  pivot_longer(cols = Forb:Forb_Woody, names_to = "Biomass_Attributes",
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
SG.9.Biomass <- bind_rows(BM_2010_long, BM_2013_long, BM_2014_long, BM_2015_long)

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



SG.9.Biomass <- SG.9.Biomass |> 
  # PlotID is blockID where blockID is missing
  mutate(blockID = if_else(is.na(blockID), paste0(substr(siteID, 1, 3), Plot), blockID)) |> 
  pivot_wider(names_from = Biomass_Attributes, values_from = Biomass_Values) |> 
  # fix Ram blockIDs
  mutate(blockID = case_when(blockID == "Ram1" ~ "Ram6",
                             blockID == "Ram2" ~ "Ram8",
                             blockID == "Ram3" ~ "Ram9",
                             TRUE ~ blockID)) |> 
  # sort, rename and drop newturf and Plot
  select(year, siteID, blockID, plotID, turfID,
         forb = Forb, woody = Woody, forb_woody = Forb_Woody,
         carex = Carex, poaceae_juncaceae = Grass, graminoid = Gram, 
         bryophyte = Bryo, lichen = Lichen, litter = Litter, litter_lichen = Litter_Lichen, 
         sampling_date = samplingDate, sorting_date = sortingDate)


write_csv(SG.9.Biomass, "cleaning_code/6_biomass_data/data/VCG_clean_functional_group_biomass_2010_2013-2015.csv")



ggplot(SG.9.Biomass, aes(x = as.character(year), y = forb)) + 
  geom_point() + 
  facet_wrap(~siteID)

SG.9.Biomass |> filter(year %in% c(2012, 2013)) |> 
  ggplot(aes(x = as.character(year), y = forb)) +
  geom_violin() +
  geom_point() +
  facet_wrap(~ siteID)


#################################################################
# Belowground biomass
# 2012? and 2014 years
################################################################

# Root biomass 2013
# run code above for 2010-2012 data to get BM_2010_2012

root_biomass_2012 <- BM_2010_2012 |> 
  filter(year == 2012) |> 
  select(year, siteID, blockID, Root_bio_1, Root_bio_2) |> 
  pivot_longer(cols = c(Root_bio_1, Root_bio_2), names_to = "replicate", values_to = "value") |> 
  mutate(replicate = str_sub(replicate, -1, -1),
         variable = "belowground_biomass",
         unit = "gm-2",
         siteID = recode(siteID, 
                         'Ulvhaugen' = "Ulvehaugen",
                         'Skjellingahaugen' = "Skjelingahaugen",
                         'Ovstedal' = "Ovstedalen"))


# Import raw data
soil_depth_raw <- read_excel(path = "cleaning_code/6_biomass_data/data/raw/soil-grass-heath-2014.xlsx", skip = 1, sheet = "grassland")
ric_raw <- read_excel(path = "cleaning_code/6_biomass_data/data/raw/soil-grass-heath-2014.xlsx", sheet = "root biomass raw (grass)")

# prettify and merge
soil_depth <- soil_depth_raw |> 
  select(siteID = Site, blockID = Block, soil_depth_1 = depth1, soil_depth_2 = depth2, soil_depth_3 = depth3) |> 
  mutate(siteID = recode(siteID, "Lav" = "Lad"))

ric_2014 <- ric_raw |> 
  select(siteID = Side, blockID = Block, replicate = Replicate, wet_mass_g = FW, dry_mass_g = DW) |> 
  left_join(soil_depth, by = c("siteID", "blockID")) |> 
  mutate(year = 2014,
         siteID = recode(siteID, 
         'Gud' = "Gudmedalen",
         'Lad' = "Lavisdalen",
         'Ram' = "Rambera",
         'Ulv' = "Ulvehaugen",
         'Skj' = "Skjelingahaugen",
         'Alr' = "Alrust",
         'Arh' = "Arhelleren",
         'Fau' = "Fauske",
         'Hog' = "Hogsete",
         'Ovs' = "Ovstedalen",
         'Vik' = "Vikesland",
         'Ves' = "Veskre"),
         blockID = paste0(str_sub(siteID, 1, 3), blockID),
         replicate = recode(replicate, "A" = 1, "B" = 2)) |> 
  pivot_longer(cols = c(wet_mass_g, dry_mass_g), names_to = "variable", values_to = "value") |> 
  mutate(variable = recode(variable, "wet_mass_g" = "belowground_productivity_wet", "dry_mass_g" = "belowground_productivity_dry"),
         unit = "gy-1") |> 
  select(year, siteID, blockID, variable, value, unit, soil_depth_1, soil_depth_2, soil_depth_3)

# merge root and ric
root_data <- bind_rows(root_biomass_2012, ric_2014) |> 
  select(year, siteID, blockID, replicate, variable, value, unit, soil_depth_1:soil_depth_3)

write_csv(root_data, file = "cleaning_code/6_biomass_data/data/VCG_clean_belowground_biomass_2013-2014.csv")



#################################################################
# Species Only DataFrame
# 2013 years
################################################################

#########################################################################
# Import the 2013 species-level file
# Select only site and species level info, not the repeated summaries
BM_2013_sp <- read_excel("cleaning_code/6_biomass_data/data/raw/Biomasse 2013 Sigrid.xlsx",
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

# rename object
SG.9.Species_Combined <- BM_2013_sp_long


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

SG.9.Species_Combined <- SG.9.Species_Combined |> 
  select(year, siteID, plotID = Plot, species, value = Biomass_g)

write_csv(SG.9.Species_Combined, "cleaning_code/6_biomass_data/data/VCG_clean_species_level_biomass_2013.csv")

# vizualisation
ggplot(SG.9.Species_Combined, aes(x = as.character(year), y = value)) + 
  geom_violin() + 
  geom_jitter(alpha = 0.5) +
  facet_wrap(~siteID)
