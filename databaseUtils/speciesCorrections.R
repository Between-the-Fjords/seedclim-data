# making corrections
# load file
corrections <- read.csv("databaseUtils/speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE)
corrections <- corrections %>% 
  mutate(
    old = trimws(old),
    new = trimws(new)
  )

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
turfCom <- tbl(con, "turfCommunity") %>% collect()
subturfCom <- tbl(con, "subturfCommunity") %>% collect()

# check turfID
turfID_glitch <- corrections %>% filter(turfID != "") %>% anti_join(turfCom, by = "turfID")
assertthat::assert_that(nrow(turfID_glitch) == 0)


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
  select(-new, -cover_new)

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

# merge any duplicates up
turfCom2 <- turfCom2 %>% 
  group_by(turfID, year, species, cf, flag) %>%
  summarise(cover = sum(cover))
  
subturfCom2 <- subturfCom2 %>% 
  group_by(turfID, subTurf, year, species, cf, flag) %>% 
  summarise(
    presence = paste(presence, collapse = " & "),
    seedlings = sum(seedlings),
    juvenile = sum(juvenile),
    adult = as.integer(any(adult == 1)),
    fertile = as.integer(any(fertile == 1)),
    vegetative = as.integer(any(vegetative == 1)),
    dominant = as.integer(any(dominant == 1))
  )

# what got merged
subturfCom2 %>% ungroup() %>% filter(grepl("&", presence)) 

# delete contents of tables
dbExecute(conn = con, "DELETE * FROM turfCommunity")
dbExecute(conn = con, "DELETE * FROM subturfCommunity")


# add revised contents
dbPadWriteTable(conn = con, table = "turfCommunity", value = turfCom2)

dbPadWriteTable(conn = con, table = "subturfCommunity", value = subturfCom2)