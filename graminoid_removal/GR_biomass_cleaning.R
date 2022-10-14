# Graminoid removal biomass cleaning code


biomass_11_16 <- read_excel("graminoid_removal/GR7_raw_graminoid_biomass_2011-2016.xlsx") |> 
  # merge site and block
  mutate(blockID = paste0(site, block),
         # recode site
         siteID = recode(site,
                         "Ulv"="Ulvehaugen", 
                         "Arh" = "Arhelleren",
                         "Alr"="Alrust",
                         "Fau"="Fauske",
                         "Gud"="Gudmedalen",
                         "Hog"= "Hogsete",
                         "Lav"="Lavisdalen",
                         "Ovs"= "Ovstedalen",
                         "Ram"="Rambera",
                         "Skj"= "Skjelingahaugen",
                         "Ves"="Veskre",
                         "Vik"="Vikesland")) |> 
  # make numeric
  mutate(biomass = as.numeric(biomass)) |> 
  select(year, siteID, blockID, turfID, cutting, value = biomass, comment = comments)




biomass_17_18 <- bind_rows("2017" = read_excel("graminoid_removal/GR7_raw_graminoid_biomass_2017-2018.xlsx", sheet = "2018"),
          "2018" = read_excel("graminoid_removal/GR7_raw_graminoid_biomass_2017-2018.xlsx", sheet = "2017"),
          .id = "year") |> 
  mutate(date = dmy(Dato),
         year = as.numeric(year),
         cutting = 1) |> 
  # fill missing sites
  fill(Lokalitet) |> 
  mutate(Blokk = recode(Blokk,
                        "I" = "1",    
                        "II" = "2",   
                        "III" = "3",  
                        "IV" = "4",  
                        "V" = "5",  
                        "VI" = "6",  
                        "VII" = "7",  
                        "IX" = "9"),
         blockID = paste0(substr(Lokalitet, 1, 3), Blokk),
         turfID = paste0(blockID, "RTC"),
         turfID = recode(turfID,
                         "Gud4RTC" = "Gud4RTCnew",
                         "Gud5RTC" = "Gud5RTCnew",
                         "Skj1RTC" = "Skj1RTCnew",
                         "Skj2RTC" = "Skj2RTCnew",
                         "Ram6RTC" = "Ram6RTCnew",
                         "Ram7RTC" = "Ram7RTCnew",
                         "Ram9RTC" = "Ram9RTCnew",
                         "Vik1RTC" = "Vik1RTCnew"
                         )) |> 
  # remove unknown data
  filter(turfID != "Skjukjent SKJRTC") |> 
  select(year, date, siteID = Lokalitet, blockID, turfID, cutting, value = Vekt, comment = Kommentarer)


bind_rows(biomass_11_16, biomass_17_18) |> 
  select(year, date, everything()) %>% 
  write_csv(., "graminoid_removal//Graminoid_removal_clean_biomass_2011-2018.csv")
