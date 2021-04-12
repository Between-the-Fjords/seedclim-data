# making corrections

# enable tidylog
# library(tidylog)
# map(getNamespaceExports("tidylog"), ~ conflict_prefer(.x, "tidylog"))

# load files
corrections <- read_csv("databaseUtils/setup-data/speciesCorrections.csv", 
                        comment = "#") %>% 
  rename(year = Year)

corrections_2020 <- read_excel("databaseUtils/setup-data/Seeclim Corrections 2020.xlsx", sheet = "Sheet1") %>% 
  rename(turfID = plot,
         old = was, 
         new = `should be`) %>% 
  mutate(`type error` = replace_na(`type error`, "")) %>% 
  filter(`type error` != "corrected by silje") %>% #corrected in proofreading
  mutate(
    turfID = toupper(turfID), #turfID must be upper case
    across(c(old, new), str_replace, pattern = "[\\s-]", replacement = "."), #. not space or - as separator
    across(c(old, new), str_replace, pattern = "\\.\\.", replacement = "."), #. not .. as separator
    across(c(old, new), str_to_title)
  ) %>% #species codes have capital first letter
  mutate(cover = case_when(
    str_detect(comm, "\\d*%") ~ str_extract(comm, "\\d*(?=%)"),
    !is.na(...6) ~ as.character(...6 * 100), #excel has converted % to proportion
    TRUE ~ ""
  )) %>% 
  mutate( # split siteID into own column
    siteID = if_else(str_detect(turfID, " "), NA_character_, turfID),
    turfID = if_else(str_detect(turfID, " "), turfID, NA_character_),
    # put delete into correct column
    new = if_else(str_detect(`type error`, "[Dd]elete"), "Delete", new)
    )

# combine corrections
corrections <- bind_rows(
  corrections,
  corrections_2020
)

#clean corrections
corrections <- corrections %>% 
  filter(!str_detect(turfID, "#") | is.na(turfID)) %>% #removes comments
  mutate(across(c(turfID, old, new), trimws)) %>% 
  #correct bad turfID
  mutate(turfID = case_when(
    turfID == "504 TT4 227" ~ "504 TT4 277",
    turfID == "20 TT4 118" ~ "20 TT4 119", # found with agrepl
    turfID == "537 TT2 279" ~ "237 TT2 279", # found with agrepl
    TRUE ~ turfID
    )) %>% 
  #correct bad spp id
  mutate(old = case_when(
    old == "Sel.sel." ~ "Sel.sel",
    old == "Car.pall" ~ "Car.pal",
    old == "X…." ~ "X...",
    old == "X…" ~ "X...",
    old == "Anto.odo" ~ "Ant.odo",
    old == "Ave.flex" ~ "Ave.fle",
    old == "Rhin.min" ~ "Rhi.min",
    old == "Pote.ere" ~ "Pot.ere", 
    old == "Bist.viv" ~ "Bis.viv", 
    old == "Cer.c34" ~ "Cer.cer",
    old == "Åkreplante" ~ "Åkerplante",
    TRUE ~ old
  )) %>% 
  #force year to numeric
  mutate(year = as.numeric(year)) %>% 
  #fix siteID
  mutate(
    siteID = str_to_title(siteID),
    siteID = case_when(
   siteID %in% c("Ålrust") ~ "Alrust",         
   siteID %in% c("Arh") ~ "Arhelleren",     
   siteID %in% c("Gud")  ~ "Gudmedalen",     
   siteID %in% c("Høgsete")  ~ "Hogsete",       
   siteID %in% c("Ovstedal") ~ "Ovstedalen",     
   siteID %in% c("Rambæra") ~ "Rambera",
   siteID %in% c("Skjell", "Skjellingahaugen") ~ "Skjelingahaugen",
   siteID %in% c("Ulv", "Ulvhaugen") ~ "Ulvehaugen",     
   siteID %in% c("Vikelsand") ~ "Vikesland",   
   siteID %in% c("General", "All") ~ NA_character_,
   TRUE ~ siteID
  )) %>% 
  #convert NA to avoid filter problems
  mutate(
    comm = replace_na(comm, ""),
    `type error` = replace_na(`type error`, "")
    )
  
## pull off special cases
#rotation - fixed below
rotated <- corrections %>% 
  filter(str_detect(comm, "plot is turned"))

corrections <- corrections %>% 
  filter(!str_detect(comm, "plot is turned"), 
         !str_detect(`type error`, "all covers are missing"))# not missing now


#check for taxon name anomalies
taxon <- tbl(con, "taxon") %>% 
  collect() 

missing_new <- corrections %>% 
  filter(old != new, #only check name changes
         !new %in% taxon$species, # check not in species list
         new != "Delete") 

if(nrow(missing_new) > 0) {
  warning("unrecognised species in new")
}

# load database data
turfCom <- tbl(con, "turf_community") %>% collect()
subturfCom <- tbl(con, "subturf_community") %>% collect()

# check turfID
turfID_glitch <- corrections %>% 
  filter(turfID != "") %>% 
  anti_join(turfCom, by = "turfID") 
assertthat::assert_that(nrow(turfID_glitch) == 0)

# check species
species_glitch <- corrections %>% 
  filter(turfID != "") %>% 
  anti_join(turfCom, by = c("old" = "species"))
assertthat::assert_that(nrow(species_glitch) == 0)

#check species/turfs
speciesturfs_glitch <- corrections %>% 
  filter(turfID != "", old != new) %>% 
  anti_join(turfCom, by = c("old" = "species", "year" = "year", "turfID" = "turfID"))

species_sub_turfs_glitch <- corrections %>% 
  filter(turfID != "", old != new) %>% 
  anti_join(subturfCom, by = c("old" = "species", "year" = "year", "turfID" = "turfID"))
assertthat::assert_that(nrow(species_glitch) == 0)

#Delete 521 TT1 523 from 2012 - comment "ødelagt av ku! Kopi fra 2011!" - subturfs idential to previous year
turfCom <- turfCom %>% 
  filter(!(turfID == "521 TT1 523" & year == 2012))
subturfCom <- subturfCom %>% 
  filter(!(turfID == "521 TT1 523" & year == 2012))


## Types of corrections
#global - YES
#site - ??? (maybe partial, but something else has wiped them)
#local (turf) - YES
#cover change - YES
#subturf changes - NO
#rotate - YES
#delete - YES
#cover clone from previous year -NO

#### identify type of corrections ####

corrections <- corrections %>% 
  mutate(type = case_when(
    #global
    (siteID == "" | is.na(siteID)) & #site is blank or NA
    (turfID == "" | is.na(turfID)) & #turf is blank or NA
    old != new ~ "global",
    #site
    (turfID == "" | is.na(turfID)) & old != new ~ "site",
    #delete
    new == "Delete" ~ "delete",
    #turf
    (turfID != "" | is.na(turfID)) & old != new ~ "turf",
    #abundance
    str_detect(cover, "\\d+") ~ "abundance",
    #NA old ? blank rows -
    is.na(old) ~ "blank?",
    #others
    TRUE ~ "other"
    )
  )

corrections %>% count(type)
corrections %>% select(-functionalGroup) %>% filter(type == "other") %>% View()
corrections %>% select(-functionalGroup) %>% filter(type == "abundance") %>% View()

#### global name changes (maybe merges) ####
# -should be in merge table
global <- corrections %>% 
  filter(type == "global") %>% 
  select(old, new)
  
#turf
turfCom2 <- turfCom %>% 
  left_join(global, by = c("species" = "old")) %>% 
  mutate(species = coalesce(new, species)) %>% 
  select(-new)

#subturf
subturfCom2 <- subturfCom %>% 
  left_join(global, by = c("species" = "old")) %>% 
  mutate(species = coalesce(new, species)) %>% 
  select(-new)



#new taxa?
corrections %>% 
  filter(siteID == "", old == new) %>% 
  select(old, new)

#### site level name changes ####
turfs <- tbl(con, "turfs") %>% collect()
plots <- tbl(con, "plots") %>% collect()
blocks <- tbl(con, "blocks") %>% collect()

site_corr <- corrections %>% 
  filter(type == "site")

site_corr <- site_corr %>% 
  select(siteID, year, old, new) %>% 
  left_join(blocks %>% select(blockID, siteID), by = "siteID") %>% 
  left_join(plots %>% select(plotID, blockID), by = "blockID") %>% 
  left_join(turfs %>% select(turfID, destinationPlotID), by = c("plotID" = "destinationPlotID")) %>% 
  select(-blockID, -plotID)
  

#turf
turfCom2 <- turfCom2 %>% 
  left_join(site_corr, 
            by = c("species" = "old", "year" = "year", "turfID" = "turfID"), 
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -siteID)

#subturf
subturfCom2 <- subturfCom2 %>% 
  left_join(site_corr, 
            by = c("species" = "old", "year" = "year", "turfID" = "turfID"),
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -siteID)

# all years
site_corr <- site_corr %>% 
  filter(is.na(year)) %>% 
  select(-year) 
  
#turf
turfCom2 <- turfCom2 %>% 
  left_join(site_corr, 
            by = c("species" = "old", "turfID" = "turfID"), 
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -siteID)

#subturf
subturfCom2 <- subturfCom2 %>% 
  left_join(site_corr, 
            by = c("species" = "old",  "turfID" = "turfID"),
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -siteID)



#### local (turf) name changes ####
# - perhaps abundance change (maybe merges)
local <- corrections %>% 
  filter(type == "turf") %>% 
  mutate(cover = as.numeric(cover)) %>% 
  select(-siteID, -functionalGroup)


#turf
turfCom2 <- turfCom2 %>% 
  left_join(local, 
            by = c("species" = "old", "year" = "year", "turfID" = "turfID"), 
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species),
    cover = coalesce(cover_new, cover)
    ) %>% 
  select(-new, -cover_new)

#subturf
subturfCom2 <- subturfCom2 %>% 
  left_join(local, 
            by = c("species" = "old", "year" = "year", "turfID" = "turfID"),
            suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -cover)

#remove local corrections
corrections <- corrections %>% 
  anti_join(local, by = c("turfID", "year", "old", "new"))



         
#### abundance changes ####
local_abun <- corrections %>% 
  filter(type = "abundance") %>% 
  select(-functionalGroup, -new)

local_abun %>% filter(grepl("[a-zA-Z]", cover)) %>% print()
local_abun <- local_abun %>% 
  filter(!grepl("[a-zA-Z]", cover)) %>% 
  mutate(cover = as.numeric(cover))

turfCom2 <- turfCom2 %>%
  left_join(local_abun, 
            by = c("species" = "old", "year" = "year", "turfID" = "turfID"),
            suffix = c("", "_new")) %>% 
  mutate(cover = coalesce(cover_new, cover)) %>% 
  select(-cover_new)

# additions
turfCom2 <- bind_rows(
  turfCom2, 
  local_abun %>% 
    anti_join(turfCom2, by = c("old" = "species", "year" = "year", "turfID" = "turfID")) %>% 
    select(-siteID) %>% 
    rename(species = old)
)

#### rotate turfs ####
rotate_turf <- function(x){# could generalise to any rotation
  vec <- 1:25
  mat <- matrix(vec, nrow = 5)
  mat
  mat2 <- t(mat)[, 5:1]
  vec2 <- as.vector(mat2)
  
  vec2 <- set_names(vec2, vec)
  vec2[as.character(x)]
}

subturfCom2 <- subturfCom2 %>% 
  mutate(subturf = if_else(turfID == rotated$turfID & year == rotated$year, 
                           true = rotate_turf(subturf),
                           false = subturf)
  ) 

#### deletions ####
# Not sure why there are any of these
delete_taxa <- corrections %>% 
  filter(type == "delete") 

turfCom2 <- anti_join(turfCom2, delete_taxa, by = c("turfID", "species" = "old", "year"))
subturfCom2 <- anti_join(subturfCom2, delete_taxa, by = c("turfID", "species" = "old", "year"))


####missing cover fixes####
##rare taxa
# singlesubplot occurrences always get 1%

turfCom2 <- subturfCom2 %>% 
  group_by(year, turfID, species) %>% 
  filter(n() == 1) %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species")) %>% #taxa/year/turf not in turfCom2
  select(turfID, year, species, cf, flag) %>% 
  mutate(cover = 1, flag = "imputed from single subturf") %>% 
  bind_rows(turfCom2)

# NID seedling is always 1%
turfCom2 <- subturfCom2 %>% 
  filter(species == "NID.seedling") %>% 
  distinct(turfID, year, species, cf, flag) %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species")) %>% #taxa/year/turf not in turfCom2
  select(turfID, year, species, cf, flag) %>% 
  mutate(cover = 1, flag = "imputed from NID.seedling presence") %>% 
  bind_rows(turfCom2)

# New rule Sag.sp  and Eup.sp if 1-3subplots ==2%, if more 4%, more than half 6% 
turfCom2 <- subturfCom2 %>% 
  filter(species %in% c("Sag.sp", "Eup.sp")) %>% 
  group_by(turfID, year, species, cf, flag) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species")) %>% #taxa/year/turf not in turfCom2
  mutate(
    cover = case_when(
      between(n, 1, 3) ~ 2,
      between(n, 4, 12) ~4,
      n > 12 ~ 6
    ),
    flag = "imputed from Sag.sp/Eup.sp n presence") %>%
  select(turfID, year, species, cover, cf, flag) %>% 
  bind_rows(turfCom2)

# impute missing covers as mean of previous and next year
sampling_year <- turfCom %>% 
  group_by(turfID) %>% 
  distinct(turfID, year) %>% 
  arrange(turfID, year) %>% 
  mutate(sampling = 1:n())


turfCom2 <- subturfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species")) %>%  #taxa/year/turf not in turfCom2
  left_join(sampling_year, by = c("turfID", "year")) %>% 
  left_join(
    left_join(turfCom2, sampling_year, by = c("turfID", "year")),
    by = c("turfID", "species"), 
    suffix = c("", "_cover")) %>% #join to other years
  filter(abs(sampling - sampling_cover) == 1) %>% #next/previous year
  group_by(turfID, species, year, cf) %>% 
  filter(n() == 2) %>% #need before and after year
  summarise(cover = mean(cover), 
            flag = "Subturf w/o cover. Imputed as mean of adjacent years", 
            .groups = "drop_last") %>% 
  bind_rows(turfCom2)

#other subturf w/o cover
subturfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species"))

#### merge any duplicates up####
turfCom2 <- turfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>%
  summarise(cover = sum(cover), .groups = "drop_last")
  
subturfCom2 <- subturfCom2 %>% 
  group_by(turfID, subturf, year, species, flag) %>% 
  summarise(
    presence = paste(presence, collapse = " & "),
    seedlings = sum(seedlings),
    juvenile = sum(juvenile),
    adult = as.integer(any(adult == 1)),
    fertile = as.integer(any(fertile == 1)),
    vegetative = as.integer(any(vegetative == 1)),
    dominant = as.integer(any(dominant == 1)),
    cf = as.integer(any(cf == 1)), 
    .groups = "drop_last"
  )

# what got merged
subturfCom2 %>% ungroup() %>% filter(grepl("&", presence)) 




#### correct covers for botanist effects ####
turf_env <- tbl(con, "turf_environment") %>% 
  select(year, turfID, recorder,  total_vascular) %>% 
  collect()


turfCom2 <- turfCom2 %>%
  mutate(cover_raw = cover) %>% #keep original cover values
  left_join(turf_env, by = c("turfID", "year")) %>%
  
  # PM - generally low turf covers - multiple by 1.2 to correct
  mutate(cover = if_else(recorder == "PM", cover * 1.2, cover)) %>%
  
  # Siri - sometimes sum of covers is << total vascular cover
  group_by(turfID, year) %>% 
  mutate(sum_cover = sum(cover)) %>% 
  mutate(cover = if_else(recorder == "Siri" & (sum_cover / total_vascular < 1.35), 
                         true = cover * 1.3,
                         false = cover)) %>% 
  
  # remove columns from turf_env
  select(-recorder, -total_vascular, -sum_cover)

#### stomping correction ####

bad <- tbl(con, "subturf_environment") %>% 
  select(turfID, subturf, year, bad) %>% 
  group_by(turfID, year) %>% 
  summarise(not_bad = 25 - sum(bad == "x", na.rm = TRUE)) %>% 
  collect()


turfCom2 <- turfCom2 %>% 
  left_join(bad, by = c("turfID", "year")) %>% 
  #remove turfs with many stomped subturfs
  filter(not_bad > 10) %>% # currently removes empty set
  # correct for stomping unless cover is already above 80 %
  mutate(cover = if_else(cover < 80 & not_bad < 25, 
                          true = cover * 25 / not_bad, 
                          false = cover)) %>% 
  select(-not_bad)
  
#### John's corrections

## TODO need to check these make sense as they are rather large change
turfCom2 <- turfCom2 %>% 
  mutate(cover = if_else(turfID == '111 TT2 137' & year == 2011 & species == 'Agr.cap', 25, cover)) %>% 
  mutate(cover = if_else(turfID == '32 TT3 109' & year == 2009, cover/2, cover), 
         cover = if_else(turfID == '32 TT3 109' & year == 2012, cover * 2/3, cover), 
         cover = if_else(turfID == '33 TT2 58' & year == 2009, cover * 2/3, cover),
         cover = if_else(turfID == '34 TT1 32' & year == 2009, cover/2, cover), 
         cover = if_else(turfID == '40 TT2 62' & year == 2011, cover * 2/3, cover)) 


#### delete contents of tables ####
dbExecute(conn = con, "DELETE FROM turf_community")
dbExecute(conn = con, "DELETE FROM subturf_community")

## add column for cover_raw
dbExecute(conn = con, "ALTER TABLE turf_community
                       ADD COLUMN cover_raw float;")

#### add revised contents ####
db_pad_write_table(conn = con, table = "turf_community", value = turfCom2)

db_pad_write_table(conn = con, table = "subturf_community", value = subturfCom2)
