library("tidyverse")
#Clean 2019 data - species codes and turfID codes

#### load 2019 data ####
data2019_0 <- list.files("rawdata/2019_data/", full.names = TRUE) %>%   
  set_names(str_remove(basename(.), coll("2019.csv"))) %>% 
  map(read_csv2, col_types = cols(.default = col_character())) %>% 
  #delete blank columns (hopefully simplifies code later)
  map(~select(.x, where(~!all(is.na(.)))))


####taxa#### 
taxa <- tbl(con, "taxon") %>% collect()

#species
sp19 <- data2019_0 %>% 
  map(names) %>% 
  map_df(enframe, .id = "site") %>%
  filter(name > 12) %>% 
  select(-name) 

corrections_2019 <- tribble(~current, ~correct,
                            "Alch sp", "Alc.sp", 
                            "Bart alp", "Bar.alp",
                            "Cham alp", "Cha.alp", 
                            "Dant dec", "Dan.dec", 
                            "Arc uva-ursi", "Arc.uva", 
                            "Saus alp", "Sau.alp", 
                            "Sal phyl", "Sal.phy", 
                            "Rub idae", "Rub.ida", 
                            "Pyrola sp", "Pyr.sp", 
                            "Rhin min", "Rhi.min", 
                            "Phle alp", "Phl.alp",
                            "Phle prat", "Phl.pra", 
                            "Phyll caer", "Phy.cae",
                            "Nar stri", "Nar.str", 
                            "Jun fili", "Jun.fil", 
                            "Hie vulg", "Hie.vul",
                            "Hier sp", "Hie.sp", 
                            "Frag ves", "Fra.ves", 
                            "Filip ulm", "Fil.ulm", 
                            "Equi arv", "Equ.arv", 
                            "Euph sp", "Eup.sp", 
                            "Sorbus", "Sor.auc",
                            "Epilob sp", "Epi.sp", 
                            "Caps b-p", "Cap.bur",
                            "Car atra", "Car.atr", 
                            "Car pulic", "Car.pul", 
                            "Carex sp", "Car.sp", 
                            "Trien eur", "Tri.eur", 
                            "Thlaspi arv", "Thl.arv", 
                            "Tarax", "Tar.sp", 
                            "Salix sp" , "Sal.sp",
                            "Aco lyc", "Aco.sep",
                            "Hyp per", "Hype.mac",
                            "Jun alp art", "Jun.alp",
                            "Myosotis cf", "Myo.dec",
                            "Rum ac-la", "Rum.acl",
                            "Valeriana", "Val.sam", 
                            "Fes pra", "Sch.pra",
                            "NID graminoid", "NID.gram",
                            "#Seedling", "NID.seedling"
)


#corrected taxon list
corrected_sp19 <- sp19 %>%   
  mutate(value = str_replace(value, "  ", " "),
         value = str_replace(value, "^Nid ", "NID ")) %>% 
  left_join(corrections_2019, by = c("value" = "current")) %>% 
  mutate(value2 = coalesce(correct, value)) %>%
  mutate(value2 = str_replace(value2, " ", ".")) %>% 
  filter(!value %in% meta_cols) 

#check for extra/uncorrected taxa
corrected_sp19 %>%
  select(-site, -value, -correct) %>% 
  distinct() %>% 
  anti_join(taxa, by = c("value2" = "species")) %>% 
  arrange(value2) %>% 
 View()

# check for duplicate taxa after correction
dup <- corrected_sp19 %>% 
  group_by(site, value2) %>% 
  filter(n() > 1)
dup

#check for actual duplicates
bind_rows(data2019_0) %>% 
  filter(DestinationSite %in% dup$site) %>% 
  select(DestinationSite, DestinationBlock, turfID, subPlot, !!unique(dup$value)) %>% 
  pivot_longer(cols = unique(dup$value)) %>% 
  filter(!is.na(value)) %>% 
  group_by(DestinationSite, DestinationBlock, turfID, subPlot) %>% 
  filter(n() > 1)
  

# correct taxa
correct_names <- with(corrections_2019, set_names(current, correct))

data2019_1 <- data2019_0 %>% 
  map(~ rename_with(.x, ~ str_replace(.x, "  ", " "))) %>% 
  map(~ rename_with(.x, ~ str_replace(.x, "^Nid ", "NID "))) %>% 
  map_df(~rename(.x, !!!correct_names[correct_names %in% names(.x)])) %>% 
  #fix Car.sp Car sp problem (Veskre vs everywhere else)
  mutate(Car.sp = coalesce(Car.sp, `Car sp`)) %>% 
  select(-`Car sp`) %>%
  #fix Alc.sp Alc sp problem
  mutate(Alc.sp = coalesce(Alc.sp, `Alc sp`)) %>% 
  select(-`Alc sp`) %>%
  #replace space with "."
  rename_with( ~ str_replace(.x, " ", "."))


#### turfs ####
#tibble of corrections for turfIDs
correct_turfID <- read_csv("turfID, corrected
RAM6RTC,  Ram6RTCnew
RAM7RTC,  Ram7RTCnew
RAM9RTC,  Ram9RTCnew
GUD5RTC,  Gud5RTCnew
TT4103, 2 TT4 103
324TT4302, 524 TT4 302
121TT3193, 121 TT3 192
74TT4206, 77 TT4 206 
117TT3C, 117 TT3 530
91TT4C229, 91 TT4 529 
515TT4C247, 515 TT4 274
257TT2C253,  257 TT2 283
65TT3131, 56 TT3 131
11TT2137, 111 TT2 137
RTC/C505, 505 TTC
RTC/C506, 506 TTC
20TT4118, 20 TT4 119
201TT2310, 285 TT2 310
RTCNEW, Gud4RTCnew
")


#  2 ""       RtcNEW     
# 6  <NA>          <NA>       
# 7  <NA>          <NA>       
# 9 "L\xc5V3RTC-1" Lav3RTC-1  
# 10 "L\xc5V3RTC-2" Lav3RTC-2  
# 20 "newRTC"       newRTC     
# 21 "newRTC"       newRTC     
# 24  <NA>          <NA>       

#correct turfIDs
data2019 <- data2019_1 %>% 
  filter(!is.na(DestinationSite) & !is.na(DestinationBlock)) %>%
  left_join(correct_turfID, by = "turfID") %>% 
  mutate(turfID = coalesce(corrected, turfID)) %>%
  select(-corrected) %>% 
  mutate(turfID = case_when(
    DestinationSite == "Hogsete" & DestinationBlock == 1 & is.na(turfID) & TTtreat == "TTC" ~ "101 TTC",
    DestinationSite == "Hogsete" & DestinationBlock == 3 & is.na(turfID) & TTtreat == "TT2" ~ "88 TT2 114",
    DestinationSite == "Vikesland" & DestinationBlock == 1 & is.na(turfID) & TTtreat == "TTC" ~ "126 TTC",
    DestinationSite == "Skjellingahaugen" & DestinationBlock == 1 & turfID == "newRTC" ~ "Skj1RTCnew",
    DestinationSite == "Skjellingahaugen" & DestinationBlock == 2 & turfID == "newRTC" ~ "Skj2RTCnew",
    TRUE ~ turfID)) %>% 
  mutate(
    fixed = iconv(turfID, "UTF-8", "UTF-8", sub = "A"), #fix bad Å
    fixed = str_remove(fixed, "[cdD]$"), #remove trailing c/d/D
    fixed = str_remove(fixed, "XC$"), #remove trailing XC
    fixed = str_remove(fixed, "(?<=\\d)C"), #remove C following a digit eg TT4C
    fixed = case_when(
      str_detect(fixed, "^(TTC)(\\d{1,3})$") ~ str_replace(fixed, "^(TTC)(\\d{1,3})$", "\\2 \\1"), #switch numbers to before TTC
      str_detect(fixed, "^[A-Z]{3}") ~ gsub(x = fixed, "([A-Z])([A-Z]{2})(.*)", "\\1\\L\\2\\U\\3", perl = TRUE), #fix case (RAM... -> Ram...)
      str_detect(fixed, "^[A-Z][a-z]{2}") ~ fixed, #Ram - OK
      str_detect(fixed, "^\\d+TT.\\d*") ~ str_replace(fixed, "(\\d+)(TT[1-4C])(\\d*)", "\\1 \\2 \\3" ), #add spaces
      TRUE ~ fixed # anything else
    ),
    fixed = str_trim(fixed)) %>% 
  #attempted fix Lav3RTC duplicate problem
    mutate(DestinationBlock = if_else(fixed == "Lav3RTC-1", "1", DestinationBlock),
           fixed = if_else(fixed == "Lav3RTC-1", "Lav1RTC", fixed),
           fixed = str_remove(fixed, "-\\d$"))


#load turfs
turfs <- read_csv("databaseUtils/turfs_table.csv") 

#check for uncorrected turfID
ID2 <- data2019 %>% 
  select(DestinationSite, DestinationBlock, originPlotID, TTtreat, destinationPlotID, turfID, RTtreat, GRtreat, fixed)

anti_join(ID2, turfs, by = c("fixed" = "turfID")) %>% 
  distinct() %>% 
  select(-originPlotID, -destinationPlotID, -GRtreat) %>% 
  view

anti_join(turfs, ID2,  by = c("turfID" = "fixed")) %>% 
  filter(!str_detect(turfID, "TT1")) %>% 
  view()

#### fixes ####
data2019 <- data2019 %>% 
  # fix column shunt in 4 TT3 91
  mutate(
    comment = if_else(turfID == "4TT391" & subPlot == "%", mossHeight, comment),
    mossHeight = if_else(turfID == "4TT391" & subPlot == "%", vegetationHeight, mossHeight),
    vegetationHeight = if_else(turfID == "4TT391" & subPlot == "%", totalLichen, vegetationHeight),
    totalLichen = if_else(turfID == "4TT391" & subPlot == "%", "0", totalLichen),
  ) %>% 
  # convert numeric cols to numeric (totalBryophytes, mossHeight, etc)
  mutate(
    across(c(starts_with("total"), ends_with("Height")), ~str_replace(., ",", ".")),
    across(c(starts_with("total"), ends_with("Height")), as.numeric)
  )


#replace turfID with fixed
data2019 <- data2019 %>% 
  mutate(turfID = fixed) %>% 
  select(-fixed)

## multiple comment columns
 data2019 <- data2019 %>% 
  #combine comment fields
  mutate(
    comment = str_replace(comment, "\x85", "?"),
    comment_1 = replace_na(comment_1, replace = ""),
    X259 = replace_na(X259, replace = ""),
    comment = paste(comment, comment_1, X259, sep = " "), 
    comment = str_trim(comment)
  ) %>% 
  select(-comment_1, -X259) %>% 
   #enter missing years
   mutate(year = if_else(is.na(year), "2019", year))

#### save data for ingestion ####
data2019 %>% 
  semi_join(turfs, by = "turfID") %>% # only good turfID
  select(any_of(c(meta_cols, taxa$species))) %>% 
  #move pleuro etc to end 
  relocate(any_of(meta_cols[which(meta_cols == "pleuro"):length(meta_cols)]), .after = last_col()) %>% 
  write_csv("rawdata/2019_harmonised.csv")
