# making corrections
# load file
corrections <- read_csv("databaseUtils/setup-data/speciesCorrections.csv", 
                        comment = "#")

corrections <- corrections %>% 
  filter(!str_detect(turfID, "#")) %>% 
  mutate(
    turfID = trimws(turfID),
    old = trimws(old),
    new = trimws(new)
  ) %>% 
  #correct bad turfID
  mutate(turfID = if_else(turfID == "504 TT4 227", true = "504 TT4 277", false = turfID)) %>% 
  #correct bad spp id
  mutate(old = case_when(
    old == "Sel.sel." ~ "Sel.sel",
    old == "Car.pall" ~ "Car.pal",
    old == "X…." ~ "X...",
    TRUE ~ old
  )) %>% 
  #force year to numeric
  mutate(Year = as.numeric(Year))
  

#check for taxon name anomalies
taxon <- tbl(con, "taxon") %>% 
  collect() 

missing_new <- corrections %>% 
  filter(old != new) %>% 
  filter(!new %in% taxon$species)

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
try(assertthat::assert_that(nrow(turfID_glitch) == 0))
# check species
species_glitch <- corrections %>% 
  filter(turfID != "") %>% 
  anti_join(turfCom, by = c("old" = "species"))
try(assertthat::assert_that(nrow(species_glitch) == 0))
#check species/turfs
speciesturfs_glitch <- corrections %>% 
  filter(turfID != "", old != new) %>% 
  anti_join(turfCom, by = c("old" = "species", "Year" = "year", "turfID" = "turfID"))
try(assertthat::assert_that(nrow(species_glitch) == 0))

#Delete 521 TT1 523 from 2012 - comment "ødelagt av ku! Kopi fra 2011!" - subturfs idential to previous year
turfCom <- turfCom %>% 
  filter(!(turfID == "521 TT1 523" & year == 2012))
subturfCom <- subturfCom %>% 
  filter(!(turfID == "521 TT1 523" & year == 2012))


## global name changes (maybe merges) -should be in merge table
global <- corrections %>% 
  filter(siteID == "", old != new) %>% 
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

##local name changes - perhaps abundance change (maybe merges)
local <- corrections %>% 
  filter(siteID != "", old != new) %>% 
  mutate(cover = as.numeric(cover)) %>% 
  select(-siteID, -functionalGroup)


#turf
turfCom2 <- turfCom2 %>% 
  left_join(local, by = c("species" = "old", "year" = "Year", "turfID" = "turfID"), suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species),
    cover = coalesce(cover_new, cover)
    ) %>% 
  select(-new, -cover_new)

#subturf
subturfCom2 <- subturfCom2 %>% 
  left_join(local, by = c("species" = "old", "year" = "Year", "turfID" = "turfID"), suffix = c("", "_new")) %>% 
  mutate(
    species = coalesce(new, species)
  ) %>% 
  select(-new, -cover)

## abundance changes
local_abun <- corrections %>% 
  filter(siteID != "", new == old) %>% 
  select(-functionalGroup, -new)

local_abun %>% filter(grepl("[a-zA-Z]", cover)) %>% print()
local_abun <- local_abun %>% 
  filter(!grepl("[a-zA-Z]", cover)) %>% 
  mutate(cover = as.numeric(cover))

turfCom2 <- turfCom2 %>%
  left_join(local_abun, by = c("species" = "old", "year" = "Year", "turfID" = "turfID"), suffix = c("", "_new")) %>% 
  mutate(cover = coalesce(cover_new, cover)) %>% 
  select(-cover_new)

# additions
turfCom2 <- bind_rows(
  turfCom2, 
  local_abun %>% 
    anti_join(turfCom2, by = c("old" = "species", "Year" = "year", "turfID" = "turfID")) %>% 
    select(-siteID) %>% 
    rename(year = Year, species = old)
)

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
  summarise(n = n()) %>% 
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

#mean of previous and next year
sampling_year <- turfCom %>% 
  group_by(turfID) %>% 
  distinct(turfID, year) %>% 
  arrange(turfID, year) %>% 
  mutate(sampling = 1:n())


turfCom2 <- subturfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>% 
  summarise(n = n()) %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species")) %>%  #taxa/year/turf not in turfCom2
  left_join(sampling_year) %>% 
  left_join(
    left_join(turfCom2, sampling_year),
    by = c("turfID", "species"), 
    suffix = c("", "_cover")) %>% #join to other years
  filter(abs(sampling - sampling_cover) == 1) %>% #next/previous year
  group_by(turfID, species, year, cf) %>% 
  filter(n() == 2) %>% #need before and after year
  summarise(cover = mean(cover), flag = "Subturf w/o cover. Imputed as mean of adjacent years") %>% 
  bind_rows(turfCom2)

#other subturf w/o cover
subturfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>% 
  summarise(n = n()) %>% 
  anti_join(turfCom2, by = c("turfID", "year", "species"))

#### merge any duplicates up####
turfCom2 <- turfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>%
  summarise(cover = sum(cover))
  
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
    cf = as.integer(any(cf == 1))
  )

# what got merged
subturfCom2 %>% ungroup() %>% filter(grepl("&", presence)) 

# delete contents of tables
dbExecute(conn = con, "DELETE FROM turf_community")
dbExecute(conn = con, "DELETE FROM subturf_community")


# add revised contents
db_pad_write_table(conn = con, table = "turf_community", value = turfCom2)

db_pad_write_table(conn = con, table = "subturf_community", value = subturfCom2)
