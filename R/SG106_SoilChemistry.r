# SeedClim Soil Chemistry Data Documentation (SG.106)
# Author: Sonya Geange


#packages
library(dplyr)
library(tidyr)
library(fs)
library(readxl)
library(readr)

#############################################################
# Read in Kari's SeedClim data from 2009 (pre transplants)
#############################################################
SC_2009 <- read_excel("./data/raw/Species and env pre-experiment.xls",
                      sheet = 7, col_names = TRUE, na = "NA")
# Relabel variables
SC_2009$LOI_pc <- SC_2009$`LOI_(%)`
SC_2009$water_cont_pc <- SC_2009$`water_cont._(%)`

# Add site column
SC_2009$siteID <- SC_2009 %>% 
  if_else(Low_4 == 1, "Ovstedalen", if_else(Low3 == 1, "Arhelleren",
                                            if_else(Low2 == 1, "Vikesland", if_else(Low1 == 1, "Fauske",
                                                                                    if_else(Int4 == 1, "Veskre", if_else(Int3 == 1, "Rambera",
                                                                                                                         if_else(Int2 == 1, "Hogsete", if_else(Int1 == 1, "Alrust",
                                                                                                                                                               if_else(Alp4 == 1, "Skjelingahaugen",
                                                                                                                                                                       if_else(Alp3 == 1, "Gudmedalen",
                                                                                                                                                                               if_else(Alp2 == 1, "Lavisdalen", if_else(Alp1 == 1, "Ulvehaugen",
                                                                                                                                                                                                                        NA))))))))))))

SC_2009 %>% add_column(siteID = NA, .before = 1)
SC_2009$siteID[SC_2009$Low_4 == 1] <- "Ovstedalen"
SC_2009$siteID[SC_2009$Low3 == 1] <- "Arhelleren"
SC_2009$siteID[SC_2009$Low2 == 1] <- "Vikesland"
SC_2009$siteID[SC_2009$Low1 == 1] <- "Fauske"
SC_2009$siteID[SC_2009$Int4 == 1] <- "Veskre"
SC_2009$siteID[SC_2009$Int3 == 1] <- "Rambera"
SC_2009$siteID[SC_2009$Int2 == 1] <- "Hogsete"
SC_2009$siteID[SC_2009$Int1 == 1] <- "Alrust"
SC_2009$siteID[SC_2009$Alp4 == 1] <- "Skjelingahaugen"
SC_2009$siteID[SC_2009$Alp3 == 1] <- "Gudmedalen"
SC_2009$siteID[SC_2009$Alp2 == 1] <- "Lavisdalen"
SC_2009$siteID[SC_2009$Alp1 == 1] <- "Ulvehaugen"

# Subset to siteID, plot and chemistry related columns
SC_2009_chem <-
  SC_2009 %>% select(siteID, plot, Plot_id,  pH, LOI_pc)

# Add year column, 2009
SC_2009_chem$year <- 2009

# Drop extra rows
SC_2009_chem = SC_2009_chem[-c(236:309), ] 


# Read in the SeedClim data dictionary for the turfs
turfs_table <- read.csv("./data/raw/turfs_table.csv", header = T, sep = ";")

# Split Plot_ID into origin and destination to match turf table
SC_2009_chem <- SC_2009_chem %>%
  separate(Plot_id, c("originPlotID", "destinationPlotID"),
           sep = "([/])")
SC_2009_chem$originPlotID <- as.integer(as.character(SC_2009_chem$originPlotID))
SC_2009_chem$destinationPlotID <-
  as.integer(SC_2009_chem$destinationPlotID)

# Match originPlotID between SC_2009_chem and turfs_table
SC_2009_chem <-
  left_join(SC_2009_chem, turfs_table, by = "originPlotID")
SC_2009_chem$destinationPlotID <-
  as.integer(SC_2009_chem$destinationPlotID.x)

# Remove rows not needed
SC_2009_chem <-
  SC_2009_chem %>% select(-c(
    TTtreat,
    RTtreat,
    GRtreat,
    destinationPlotID.x,
    destinationPlotID.y,
    destinationPlotID,
    plot
  )) %>% 
  rename(plotID = originPlotID)

#############################################################
# Read in 2010 and 2012 soil chemistry datasets 2010 is from Farinas Msc Thesis,
# and 2012 is from Farinas PhD Thesis Proposal
#############################################################

SC_2010_2012 <-
  read_excel(
    "./data/raw/SFarinas_Compiled_data_for_SEEDCLIM_2010_and_2012.xlsx",
    sheet = 1,
    col_names = TRUE,
    na = "NA"
  )

# Remove white space in column names
colnames(SC_2010_2012) <- gsub(" ", "_", colnames(SC_2010_2012))

# Remove first row with units
SC_2010_2012 = SC_2010_2012[-1,]

# Subset to plot, location and chemistry related columns
SC_2010_2012_chem <-
  SC_2010_2012 %>% select(c(
    siteID, blockID, plotID, turfID, year,
    NO3N, NH4N, AvailN
  ))



# 2010_2012 match SiteID to data dictionary
SC_2010_2012_chem <-
  SC_2010_2012_chem %>% mutate(
    siteID = recode(
      siteID,
      "Ulvhaugen" = "Ulvehaugen",
      "Lavisdalen" = "Lavisdalen",
      "Gudmedalen" = "Gudmedalen",
      "Skjellingahaugen" = "Skjelingahaugen",
      "Alrust" = "Alrust",
      "Hogsete" = "Hogsete",
      "Rambera" = "Rambera",
      "Veskre" = "Veskre",
      "Fauske" = "Fauske",
      "Vikesland" = "Vikesland",
      "Arhelleren" = "Arhelleren",
      "Ovstedal" = "Ovstedalen"
    )
  )


# Merge Both DataFrames Together (year)
SeedClim_soil_chem <- merge(SC_2010_2012_chem, SC_2009_chem,
                            by = "year",
                            all = TRUE)

# Merge the two turfID, siteID and plotID columns together, as they shouldn't overlap
# Remove the original columns and ignore the NA's.
SeedClim_soil_chem <- SeedClim_soil_chem %>% 
  unite("turfID", c(turfID.x,turfID.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("siteID", c(siteID.x, siteID.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("plotID", c(plotID.x, plotID.y), remove = TRUE, na.rm = TRUE)

# Format type
SeedClim_soil_chem$NO3N <- as.numeric(as.character(SeedClim_soil_chem$NO3N))
SeedClim_soil_chem$NH4N <- as.numeric(as.character(SeedClim_soil_chem$NH4N))
SeedClim_soil_chem$AvailN <- as.numeric(as.character(SeedClim_soil_chem$AvailN))




############################################################
# Test if this 2013 data is new LOI  measures
# Based off Farinas PhD Thesis Proposal, dated 2013

SC_2013 <-
  read_excel(
    "./data/raw/Seedclim-rootgrowth-tested-variables.xlsx",
    sheet = 1,
    col_names = TRUE,
    na = "NA"
  )


# Remove white space in column names
colnames(SC_2013) <- gsub(" ", "_", colnames(SC_2013))

# Subset to plot, location and chemistry related columns
SC_2013_chem <-
  SC_2013 %>% select(c(
    Site, NO3, NH4, N, pH, LOI
    #NB: Labelling here is wrong for traits - NO3N, NH4N, AvailN
  ))



# 2013 match SiteID to data dictionary
# NB: Not all 12 sites are present in this dataset it seems
SC_2013_chem <-
  SC_2013_chem %>% mutate(
    siteID = recode(
      Site,
      "Ulvhaugen" = "Ulvehaugen",
      "Lavisdalen" = "Lavisdalen",
      "Gudmedalen" = "Gudmedalen",
      "Skjellingahaugen" = "Skjelingahaugen",
      "Alrust" = "Alrust",
      "Hogsete" = "Hogsete",
      "Rambera" = "Rambera",
      "Veskre" = "Veskre"
    )
  )

# Rename chemical variables
SC_2013_chem <- rename(
  SC_2013_chem, NO3N = NO3,
  NH4N = NH4,
  AvailN = N, 
  LOI_pc = LOI)

# Add year column for 2013
SC_2013_chem$year <- 2013

# remove Site
SC_2013_chem <- SC_2013_chem %>% select(-c(Site))

# Merge with 2009/10/12 DataFrames  (year)
SeedClim_soil_chem_all <- merge(SeedClim_soil_chem, SC_2013_chem,
                                by = "year",
                                all = TRUE)

# Merge the two turfID, siteID and plotID columns together, as they shouldn't overlap
# Remove the original columns and ignore the NA's.
SeedClim_soil_chem_all <- SeedClim_soil_chem_all %>% 
  unite("siteID", c(siteID.x, siteID.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("LOI_pc", c(LOI_pc.x, LOI_pc.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("pH", c(pH.x, pH.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("NO3N", c(NO3N.x, NO3N.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("NH4N", c(NH4N.x, NH4N.y), remove = TRUE, na.rm = TRUE) %>% 
  unite("AvailN", c(AvailN.x, AvailN.y), remove = TRUE, na.rm = TRUE)

# Format type
SeedClim_soil_chem_all$LOI_pc <- as.numeric(as.character(SeedClim_soil_chem_all$LOI_pc))
SeedClim_soil_chem_all$pH <- as.numeric(as.character(SeedClim_soil_chem_all$pH))
SeedClim_soil_chem_all$NO3N <- as.numeric(as.character(SeedClim_soil_chem_all$NO3N))
SeedClim_soil_chem_all$NH4N <- as.numeric(as.character(SeedClim_soil_chem_all$NH4N))
SeedClim_soil_chem_all$AvailN <- as.numeric(as.character(SeedClim_soil_chem_all$AvailN))


# Write to long format
SeedClim_soil_chem_long <- SeedClim_soil_chem_all %>% 
  pivot_longer(cols = NO3N:LOI_pc, names_to = "Attributes", values_to = "Values",
               values_drop_na = TRUE)

# Summarize values within each dataset and compare the values
# Ignore the sample size, as often it's been copied across rows
summary_all <- SeedClim_soil_chem_all %>% group_by(siteID, year) %>% 
  summarise(mean_LOI = mean(LOI_pc, na.rm = TRUE), n = n(),
            mean_pH = mean(pH, na.rm = TRUE), n = n(),
            mean_NO3N = mean(NO3N, na.rm = TRUE), n = n(),
            mean_NH4N = mean(NH4N, na.rm = TRUE), n = n(),
            mean_AvailN = mean(AvailN, na.rm = TRUE), n = n())


# LOI: Values seem to differ between 2009 and 2013, so keep both
# pH: Values seem to differ between 2009 and 2013, so keep both
# For both NO3N and NH4N, it seems odd that they appear identical here, given the difference in sample sizes, and raw data points as well?? 
# NO3N: 2010/2012 appear identical (bar sample size), but 2013 is different
# NH4N: 2012/2012 appear identical (bar sample size), but 2013 is different
# How convinced are we about the 2012 dataset? Do we not include 2012 therefore in the final output file?


#############################
# Output File

# Using write_csv keeps from converting row numbers to a column
write_csv(SeedClim_soil_chem_long, "./data/processed/seedclim_soil_chemistry_long.csv")
