#########################################################
 # READ IN AND CLEAN RAW SEEDCLIM CLIMATE DATA FROM OSF #
#########################################################

### LIBRARIES
library("lubridate")
library("tidyverse")
library("data.table")

# Stuff
pn <- . %>% print(n = Inf)

#### IMPORT CLIMATE DATA FOR ALL SITES ####
# load in premade functions
source('Functions_ReadInSeedClimClimateData.R')

# Download zip files from OSF, place them in folder of your choice, insert that path name into "path"
### on line 20

climateRepo <- list.files(path = "/Users/joshisoriginal/Downloads/filesfromosf", 
                             pattern = "txt", recursive = TRUE, full.names = TRUE) %>% 
  grep(pattern = "Notes|Notater|UVB", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  grep(pattern = "ITAS\\d{0,4}\\.txt|ITAS-FALL-2014\\.txt", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  ### files that need fixing!!!
  grep(pattern = "1239_23062009.txt", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  #(function(.).[(1:50)]) %>% # only run subset
  map_df(ReadData) #%>% 
  #mutate(Repo = "old_Bio_Felles")


# Warnings that are ok
# format not recognised: Skjellingahaugen-met1-20120910.txt format not recognised (corrupted file)


# bind rows of old and new repo
climate <- climateRepo %>% 
  # remove wrong or double files
  filter(!file %in% c("Skjellingahaugen_ITAS_110624_111015.txt", "#001038_20080924_5 cm jord.txt", "#001056_20090611_2001.txt")) %>% 
  group_by(file) %>% 
  # remove first two and last two rows for each file.
  slice(-c(1:2, n()-1, n())) %>% 
  setDT()

# remove duplicate files
climate <- climate %>% 
  group_by(date, logger, site, type, value) %>% 
  slice(1) %>% 
  group_by(date, logger, site, type) %>% 
  slice(1)

save(climate, file = "climate.Rdata")
#load(file = "climate.Rdata")  


#### CLEAN DATA ####
# get rid of 1900 date, all empty lines
# remove everything before 1.10.2008, values too high (for temp but then other climate data probably also crap)
climate <- climate %>% ungroup() %>%
  filter(!date < "2008-10-01 00:00:00")


# Check logger names
unique(climate$logger)
table(climate$logger, climate$site)


# Subset soilmoisture, precipitation and temperatur loggers into seperate object
temperature <- subset(climate, logger %in% c("temp1", "temp2", "temp200cm", "temp30cm", "", "PØN", "-5cm", "thermistor 1", "thermistor 2", "jord-5cm", "2m", "temperature", "temperature2", "soil temp", "veg. temp", "veg temp"))
precipitation <- subset(climate, logger %in% c("nedbor", "rain", "arg100", "counter", "counter1", "counter2"))
soilmoisture <- subset(climate, logger %in% c("jordf1", "jordf2", "soil.moisture", "soil moisture 2", "soil moisture 1", "soil moisture", "sm300 2", "sm300 1", "jordfukt2"))

write.csv(precipitation, "Precipitation.csv")
save(soilmoisture, file = "Soilmoisture.RData")

# if(basename(textfile) %in% c("Fauske_temp_Fall2016.txt")){
#   message("removing soil moisture and precipitation data from fauske fall 2016 file")
#   dat <- dat %>%
#     filter(!logger %in% c("jordf1",	"jordf2", "nedbor"))
# }


# Explore temparure data and plot
table(temperature$logger, temperature$site)
table(temperature$logger, year(temperature$date))
table(temperature$site, year(temperature$date))

# plot logger by site 
#plot_climate(start_date = "2008.1.1", end_date = "2017.11.1", log = c("temp1"), inc = TRUE, SITE = "Lav")

# plot single site
temperature %>% 
  filter(site == "skj") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

# Find file names
temperature %>%
  filter(site == "skj", logger == "") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))
  
temperature %>% 
  filter(type == "ITAS") %>% 
  ungroup() %>% 
  distinct(logger, site, file) %>% 
  arrange(site, logger) %>% pn


#### DATA CLEANING ####

# Change logger names
temperature <- temperature %>% 
  # add column for remarks, when data is doubtfull
  mutate(flag = NA) %>%  
  ungroup() %>% 
  # delete crap UTL data !!! IS THIS NEEDED???
  #filter(! site %in% c("lav", "gud") & logger == "-5cm")
#temperature <- temperature[!(temperature$site == "Lav" & temperature$logger == "-5cm"),]
#temperature <- temperature[!(temperature$site == "Gud" & temperature$logger == "-5cm"),]
  mutate(logger = ifelse(file %in% c("1229_30092009.txt", "#002474_20180607_1000.txt", "#001066_20081030_0000.txt", "1233_30092009.txt"), "temp30cm", logger)) %>% 
  mutate(logger = ifelse(file %in% c("2488_21092010.txt", "#002482_20180705_0800.txt", "#001047_20081030_0000_Veskre.txt", "1169_30092009.txt"), "temp200cm", logger)) %>% 
  mutate(logger = recode(logger, "2m" = "temp200cm", "PØN" = "temp200cm", "-5cm" = "temp30cm", "soil temp" = "tempsoil", "veg temp" = "tempabove", "veg. temp" = "tempabove"))

temperature$logger[temperature$logger == ""] <- "temp30cm"

#check new logger names
table(temperature$logger)


#### FIX ITAS DATA ####
## FAUSKE
temperature2 <- temperature %>% 
  filter(file != "fauske_climate_soil_moist_prec_Fall2016.txt") %>% 
  
  # delete temp1 and temp2 logger between June 2013 and October 2014 wrong values
  mutate(value = ifelse(file %in% c("Fauske_ITAS_13.6.6-13.9.22.txt", "Fauske_ITAS_13.9.22-14.5.14.txt", "Fauske_ITAS_14.5.14-14.10.14.txt") & logger == "temp1", NA, value)) %>% 
  
  # Flag temp1 logger between 2015 and May 2016 soil logger not in the soil
  # Summer 2016 data: both ITAS loggers show very similar variance
  mutate(flag = ifelse(file %in% c("fauske_climate 20150129 - 20150429.txt", "fauske_climate 20150429 - 20151009.txt", "Fauske_temp_Fall2016.txt", "fauske_climate_spring2016.txt") & logger == "temp1", "VarianceProblem_TooLow", flag),
         flag = ifelse(file %in% c("Fauske_temp_Fall2016.txt") & logger == "temp2", "VarianceProblem", flag)) %>% 
  
  # switch logger temp1 and temp2 always !!!
  mutate(logger = case_when(site == "fau" & logger %in% c("temp1", "thermistor 2") ~ "tempsoil",
                            site == "fau" & logger %in% c("temp2", "thermistor 1") ~ "tempabove",
                            TRUE ~ logger)) %>%
  # 2018 variance high from sun exposure-JSL
  mutate(flag = ifelse(site == "fau" &
                         year(date) == 2018 &
                         logger == "tempabove",
                       "VarianceProblem_TooHigh", flag))

# check again 30cm at start!!!
# check 2018 above ground! exposed to sun! fau, arh


## VIKESLAND
# flag tempsoil from April 2015 - now: bias
temperature2 <- temperature2 %>%
  mutate(flag = ifelse(file %in% c("Vikesland_climate 20150428 - 20151007.txt",
                                   "Vikesland_climate_spring2016.txt",
                                   "Vikesland_climate_Fall2016.txt", "Vikesland_climate.txt",
                                   "Vikesland_climate_autumn2017.txt") &
                         logger == "temp2",
                       "VarianceProblem_TooHigh", flag))


# ARHELLEREN
# switch temp1 and temp2 for July 2009 - Oct 2009
temperature2 <- temperature2 %>% 
  mutate(logger = ifelse(file == "Arhelleren_08072009.txt" & logger == "temp1", "tempsoil", logger),
         logger = ifelse(file == "Arhelleren_08072009.txt" & logger == "temp2", "tempabove", logger))%>%
 ## flag high variacne in the the post 2018 summer temps above-sun exposure as above-JSL
   mutate(flag = ifelse(site == "arh" &
                           year(date) >= 2018 &
                           logger == "tempabove",
                         "VarianceProblem_TooHigh", flag))


# OVSTEDAL
# flag temp2 between 2011 and 2015; largeBias from Dec 2012 - 2015
temperature2 <- temperature2 %>% 
  mutate(flag = ifelse(site == "ovs" & logger == "temp2" & file %in% c("Øvstedal_met1_20160511_20160523.txt",  "Øvstedal_met1_Fall2016.txt","Øvstedal_met1_autumn2017.txt", "Øvstedal_met1.txt"), "bias", flag)) %>% 
  # remove high values at the start
  mutate(value = ifelse(site == "ovs" &
                          logger %in% c("temp200cm", "temp30cm") &
                          date < ymd_hms("2009-06-28 00:00:00"),
                        NA, value)) %>%
  ## assigning correct logger lables to files-JSL
  mutate(logger = ifelse(file == "#002486_20190506_1100.txt" & logger == "temp30cm", "temp30cm", logger),
         logger = ifelse(file == "#002473_20190506_1200.txt" & logger == "temp30cm", "temp200cm", logger))



# VESKRE
temperature2 <- temperature2 %>% 
  # switch temp1 and temp2 from 2014, 2016, 2017
  mutate(logger = ifelse(file %in% c("Veskre_ITAS_140802_141003.txt", "Veskre_klima_autumn2017.txt", "Veskre_klima_Fall2016.txt", "Veskre_klima.txt") &
                           logger == "temp2", "tempabove", logger),
         logger = ifelse(file %in% c("Veskre_ITAS_140802_141003.txt", "Veskre_klima_autumn2017.txt", "Veskre_klima_Fall2016.txt", "Veskre_klima.txt") &
                           logger == "temp1", "tempsoil", logger)) %>% 
  # Flag temp1 in 2015, because measuring both soil temp
  mutate(flag = ifelse(site == "ves" &
                         logger == "temp1" &
                         file %in% c("Veskre_climate 20150909 - 20151011.txt", "Veskre_climate.txt"),
                       "VarianceProblem_TooLow", flag)) %>%
  # temperature data at veskre belongs in tempsoil-JSL
  mutate(logger = ifelse(file %in% c("Veskre_klima.txt") &
                           logger== "temperature", "tempsoil", logger))


# RAMBAERA
temperature2 <- temperature2 %>% 
  # switch temp1 and temp2 before 2014
  mutate(logger = ifelse(site == "ram" &
                           #year(date) <= 2013 &
                           logger == "temp1",
                         "tempsoil", logger),
         logger = ifelse(site == "ram" &
                           #year(date) <= 2013 &
                           logger == "temp2",
                         "tempabove", logger)) %>% 
  # flag temp2 from Oct 2015 - May 2016 too large variance
  mutate(flag = ifelse(site == "ram" &
                         logger == "temp2" &
                         file == "Rambera_met1.txt" &
                         date < dmy_hms("24.05.2016 18:00:00"),
                       "VarianceProblem_TooHigh", flag)) %>%
  ## flag high variacne in the the post 2020 summer tempabove and tempsoil-sun exposure as above-JSL
  mutate(flag = ifelse(site == "ram" &
                         date >= dmy_hms("03.06.2020 18:00:00") &
                         date <= dmy_hms("18.06.2020 18:00:00")&
                         logger %in% c("tempabove", "tempsoil"),
                       "VarianceProblem_TooHigh", flag))


# HOGSETE
# flag temp1 from from June 2015 - Oct 2016 too little variance
temperature2 <- temperature2 %>%
  mutate(flag = ifelse(file == "Høgsete_met1_2015_2016.txt",
                       "VarianceProblem_TooLow", flag)) %>% 
  mutate(flag = ifelse(file == "Høgsete_met1_2015_2016.txt" &
                         logger == "temp2" &
                         value < -5,
                       "WrongValues_TooLow", flag))


# ALRUST
temperature2 <- temperature2 %>% 
  # switch logger until 31.12.2015
  mutate(logger = ifelse(site == "alr" &
                           year(date) < 2016 &
                           logger == "temp1",
                         "tempsoil", logger),
         logger = ifelse(site == "alr" &
                           year(date) < 2016 &
                           logger == "temp2",
                         "tempabove", logger)) %>% 
  # flag temp1 from April 2012 - Dec 2012 too little variance
  mutate(flag = ifelse(site == "alr" &
                         logger %in% c("temp1", "tempabove") &
                         year(date) > 2011 &
                         year(date) < 2015,
                       "VarianceProblem_TooLow", flag)) %>% 
# flag temp2 in 2010 and April 2015 - May 2016 too large variance
  mutate(flag = ifelse(site == "alr" &
                         logger %in% c("temp2", "tempsoil") &
                         year(date) == 2010 &
                         year(date) == 2015,
                       "VarianceProblem_TooHigh", flag))


# ULVHAUGEN
# switch temp1 to soil and temp2 to abovegroun
temperature2 <- temperature2 %>% 
  mutate(logger = ifelse(logger == "temp1" & site == "ulv",
                         "tempsoil", logger),
         logger = ifelse(logger == "temp2"& site == "ulv",
                         "tempabove", logger))


# LAVISDALEN
temperature2 <- temperature2 %>% 
  # Remove wrong data in 2012/2013
  filter(!grepl("visdalen-met1-20120913.txt", file)) %>% 
  mutate(value = ifelse(logger == "temp1" &
                          file == "Lavisdalen_25_10_11.txt",
                        NA, value)) %>% 
  mutate(value = ifelse(year(date) < 2014 &
                          grepl("visdalen_met1.txt", file),
                        NA, value)) %>% 
  # switch logger before 2012
  mutate(logger = ifelse(site == "lav" &
                           logger == "temp1" &
                           year(date) < 2012,
                         "tempsoil", logger),
         logger = ifelse(site == "lav" &
                           logger == "temp2" &
                           year(date) < 2012,
                         "tempabove", logger)) %>% 
  # rename loggers
  mutate(logger = ifelse(site == "lav" &
                            logger == "temperature",
                          "tempabove", logger),
         logger = ifelse(site == "lav" &
                            logger == "temperature2",
                          "tempsoil", logger)) %>% 
  # Bias in all the aboveground data
  mutate(flag = ifelse(site == "lav" &
                         logger %in% c("temp1", "tempabove"),
                       "Bias_IncreasingVarOverTime", flag)) %>% 
  # remove last crazy file: "Låvisdalen_met1 (2).txt"
  mutate(value = ifelse(site == "lav" &
                          logger %in% c("temp1", "temp2", "tempabove", "tempsoil") &
                          year(date) > 2012 &
                          year(date) < 2014,
                        NA, value))


# GUDMEDALEN
temperature2 <- temperature2 %>% 
  # remove double
  filter(logger != "jord-5cm") %>% 
  # remove data from 2018, wrong values > just remove 2018
  mutate(value = ifelse(site == "gud" &
                          logger %in% c("temperature", "temperature2") &
                          year(date)== 2018,
                       NA, value)) %>% 
  # switch logger until end of 2014
  mutate(logger = ifelse(site == "gud" &
                           logger == "temp1" &
                           date < "2015-01-01 00:00:00",
                         "tempsoil", logger),
         logger = ifelse(site == "gud" &
                           logger == "temp2" &
                           date < "2015-01-01 00:00:00",
                         "tempabove", logger)) %>% 
  mutate(logger = case_when(file == "Gudmedalen_ITAS_140619_141013.txt" &
                              logger == "tempabove" ~ "tempsoil",
                            file == "Gudmedalen_ITAS_140619_141013.txt" &
                              logger == "tempsoil" ~ "tempabove",
                            TRUE ~ logger)) %>% 
  ## temperature is tempabove and temperature2 is tempsoil in fall 2019-JSL
  mutate(logger = case_when(file == "Gudmedalen-met1.txt" &
                              logger == "temperature" ~ "tempabove",
                            file == "Gudmedalen-met1.txt" &
                              logger == "temperature2" ~ "tempsoil",
                            TRUE ~ logger)) %>%
  mutate(logger = case_when(file == "Gudmedalen_met.txt" &
                              logger == "temperature" ~ "tempabove",
                            file == "Gudmedalen_met.txt" &
                              logger == "temperature2" ~ "tempsoil",
                            TRUE ~ logger)) %>%
  # Variance problems with above logger between end of June 2014 - Oct 2014
  mutate(flag = ifelse(file == "Gudmedalen_ITAS_140619_141013.txt" &
                         logger == "tempabove",
                       "VarianceProblem_TooLow", flag))


# SKJELLINGAHAUGEN
temperature2 <- temperature2 %>% 
  # Flag variance problems with temp1 logger in 2014
  mutate(flag = ifelse(site == "skj" &
                         year(date) == 2014 &
                         logger == "temp1",
                       "VarianceProblem_TooLow", flag)) %>% 
  # Flag few data points for both loggers in 2012
  mutate(flag = ifelse(site == "skj" &
                         year(date) == 2012 &
                         logger %in% c("temp1","temp2"),
                       "Bias_FewDataPoints", flag)) %>% 
  # switch logger from 2015 - 2016
  mutate(logger = ifelse(site == "skj" &
                           logger == "temp1" &
                           file %in% c("Skjellingahaugen_met1 20141016 - 20150121.txt", "Skjellingahaugen_met1_20150806 - 20150814.txt", "Skjellingahaugen_met1-20150907- 20150910.txt", "Skjellingahaugen_met1_Spring2017.txt", "Skjellingahaugen_met1_temp_prec_20170712-20170808_Spring2017.txt"),
                         "tempsoil", logger),
         logger = ifelse(site == "skj" &
                           logger == "temp2" &
                           file %in% c("Skjellingahaugen_met1 20141016 - 20150121.txt", "Skjellingahaugen_met1_20150806 - 20150814.txt", "Skjellingahaugen_met1-20150907- 20150910.txt", "Skjellingahaugen_met1_Spring2017.txt", "Skjellingahaugen_met1_temp_prec_20170712-20170808_Spring2017.txt"),
                         "tempabove", logger)) %>% 
  # remove high values at the start
  mutate(value = ifelse(site == "skj" &
                          logger %in% c("temp200cm", "temp30cm") &
                          date < ymd_hms("2009-06-25 00:00:00"),
                        NA, value)) %>%
  # one file is on a 2 second timeframe-just remove it-JSL
  mutate(value = ifelse(site == "skj" &
                          logger %in% c("temperature", "temperature2") &
                          year(date)==2019,
                        NA, value))
  #filter(!between(date, as.Date("2019-06-18"), as.Date("2019-06-20")))



# Change remaining logger names
temperature2 <- temperature2 %>% 
  # remove strange data from 2008
  mutate(value = ifelse(logger == "temp30cm" &
                          year(date) < 2009,
                        NA, value)) %>% 
  # remove NAs
  filter(!is.na(value)) %>% 
  # rename loggers
  mutate(logger = recode(logger, "temp1" = "tempabove", "temp2" = "tempsoil")) %>% 
  # Remove first observations for 2m
  group_by(site) %>% 
  slice(-c(1:30)) %>% 
  
  # Rename and order sites levels
  ungroup() %>% 
  mutate(site = recode(site, "skj" = "Skjellingahaugen", "gud" = "Gudmedalen", 
                       "lav" = "Lavisdalen", "ulv" = "Ulvhaugen", "ves" = "Veskre", 
                       "ram" = "Rambera", "hog" = "Hogsete", "alr" = "Alrust", "ovs" = "Ovstedalen",
                       "arh" = "Arhelleren", "vik" = "Vikesland", "fau" = "Fauske"),
         site = factor(site, levels = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvhaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske")))

# fill missing dates with NA and merging with complete dataset
full_grid <- expand.grid(logger = unique(temperature2$logger), site = unique(temperature2$site), date = seq(min(temperature2$date), max(temperature2$date), by = "hour"))

temperature2 <- left_join(full_grid, temperature2) %>% as_tibble()

temperature2 <- temperature2 %>% filter(date < "2021-01-01 00:00:00")# remove the 2038 data point 


write.csv(temperature2,"Temperature.csv")


# plot single site to check
temperature2 %>% 
  filter(site == "Ovstedalen") %>% # filter(date > "2020-01-01 00:00:00")%>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~logger)


# Find file names
temperature %>%
  filter(site == "skj", logger == "") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))

temperature %>% 
  filter(type == "ITAS") %>% 
  ungroup() %>% 
  distinct(logger, site, file, Repo) %>% 
  arrange(site, logger) %>% pn



####################################
### Clean the soil moisture data ###
####################################

print(soilmoisture, width=Inf)

# visualize the data
soilmoisture %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(site ~ logger)


# what kind of loggers are used 
levels(factor(soilmoisture$type)) # all ITAS
levels(factor(soilmoisture$logger))
levels(factor(soilmoisture$site))

#### Data Cleaning ####

# fix names of loggers
soilmoisture <- soilmoisture %>% 
  mutate(flag = NA) %>% mutate(sensor.disagree = NA) %>% # add columns for remarks on doubtful data
  mutate(logger = recode(logger, "jordf1" = "soil.moisture1", "jordf2" = "soil.moisture2", "jordfukt2" = "soil.moisture2", 
                         "sm300 1" = "soil.moisture1","sm300 2" = "soil.moisture2", "soil moisture" = "soil.moisture1", 
                         "soil moisture 1" = "soil.moisture1", "soil moisture 2" = "soil.moisture2"))
  

## clean by site ##

# alr

# add cleaning and note steps
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 2) from 2013-2015
  mutate(flag = ifelse(site == "alr" &
                  logger == "soil.moisture2" &
                  date > "2013-09-19 00:00:00" &
                  date < "2015-05-11 00:00:00", "VarianceProblem_TooHigh", flag))


# arh

# add cleaniing steps
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 2) from 2012-2015
  mutate(flag = ifelse(site == "alr" &
                  logger == "soil.moisture2" &
                  date > "2013-04-01 00:00:00" &
                  date < "2015-12-31 00:00:00", "VarianceProblem_TooHigh", flag))

# fau 
# looked good- no cleaning needed

# gud
# cleaning
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 1) from 2014-2019
  mutate(flag = ifelse(site == "gud" &
                  logger == "soil.moisture1" &
                  date > "2014-01-01 00:00:00" &
                  date < "2019-12-31 00:00:00", "VarianceProblem_TooHigh", flag))

# hog
# clean
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 1) in 2019
  mutate(flag = ifelse(site == "hog" &
                  logger == "soil.moisture2" &
                  date > "2019-05-05 00:00:00" &
                  date < "2019-12-31 00:00:00", "WrongValues_TooLow", flag)) %>%
  mutate(flag = ifelse(site == "hog" &
                  logger == "soil.moisture1" &
                  date > "2019-05-25 00:00:00" &
                  date < "2019-10-01 00:00:00", "VarianceProblem_TooHigh", flag)) 
  

# lav
# nothing obvious that needs to be cleaned

# ovs
# nothing obvious that needs to be cleaned

# ram
# clean
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 1) in 2019
  mutate(flag = ifelse(site == "ram" &
                  logger == "soil.moisture2" &
                  date > "2013-01-01 00:00:00" &
                  date < "2013-12-31 00:00:00", "VarianceProblem_TooHigh", flag)) 

# skj 
# nothing obvious that needs to be cleaned

# ulv
# clean
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 2) in 2016-2017
  mutate(flag = ifelse(site == "ulv" &
                  logger == "soil.moisture2" &
                  date > "2016-09-20 00:00:00" &
                  date < "2017-10-12 00:00:00", "VarianceProblem_TooHigh", flag)) 

# ves
# nothing obvious that needs to be cleaned

# vik
# clean
soilmoisture <-  soilmoisture %>% 
  # flag weird in SM (soilmoisture 2) in 2016-2017
  mutate(flag = ifelse(site == "vik" &
                  logger == "soil.moisture1" &
                  date > "2013-10-20 00:00:00" &
                  date < "2014-12-12 00:00:00", "VarianceProblem_TooHigh", flag)) %>%
  mutate(flag = ifelse(site == "vik" &
                  logger == "soil.moisture2" &
                  date > "2013-09-20 00:00:00" &
                  date < "2014-12-31 00:00:00", "VarianceProblem_TooHigh", flag))


### now flag sensor.disagree
# "sensor.disagree" is the difference in the replicate soil moisture sensors in the site
# users can use this column to decide on sensor disagreement thresholds they are compfortable using

# add in the difference
soilmoisture2 <- soilmoisture %>% spread(key = "logger", value = "value") %>%
  mutate(sensor.disagree = soil.moisture1-soil.moisture2)

soilmoisture2 %>%
  ggplot(aes(x = date, y = sensor.disagree)) +
  geom_line() +
  facet_wrap(~ site)

soilmoisture2 %>% filter(sensor.disagree > -0.25) %>% filter(sensor.disagree < 0.25) %>%
  ggplot(aes(x = date, y = sensor.disagree)) +
  geom_line() +
  facet_wrap(~ site)

soilmoisture2 %>% gather("soil.moisture1", "soil.moisture2", key= "logger", value="value") %>%
  filter(sensor.disagree > -0.25) %>% filter(sensor.disagree < 0.25) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(site ~ logger)

soilmoisture3 <- soilmoisture2 %>% gather("soil.moisture1", "soil.moisture2", key= "logger", value="value")


# finally reassign site names and save
soilmoisture3 <-  soilmoisture3 %>% mutate(site = recode(site, "skj" = "Skjellingahaugen", "gud" = "Gudmedalen", 
                     "lav" = "Lavisdalen", "ulv" = "Ulvhaugen", "ves" = "Veskre", 
                     "ram" = "Rambera", "hog" = "Hogsete", "alr" = "Alrust", "ovs" = "Ovstedalen",
                     "arh" = "Arhelleren", "vik" = "Vikesland", "fau" = "Fauske"),
       site = factor(site, levels = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvhaugen", "Veskre", "Rambera", 
                                      "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske")))


write.csv(soilmoisture3, "SoilMoisture.csv")

