################## SEEDLING DATA #######################

#need to adjust the SQL query
seedlingGR <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN subTurfCommunity ON turfs.turfID = subTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
HAVING subTurfCommunity.Year=2013 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"
))

head(seedlingGR)

levels(seedlingGR$TTtreat) <- c(levels(seedlingGR$TTtreat), levels(seedlingGR$GRtreat))
seedlingGR$TTtreat[seedlingGR$TTtreat == ""|is.na(seedlingGR$TTtreat)] <- seedlingGR$GRtreat[seedlingGR$TTtreat == ""|is.na(seedlingGR$TTtreat)]
seedlingGR$GRtreat <- NULL
seedlingGR$TTtreat <- factor(seedlingGR$TTtreat)
seedlingGR <- seedlingGR[!seedlingGR$blockID%in%remsites,]

recruitment <- seedlingGR %>%
  group_by(turfID) %>%
  mutate(sum_seedlings = sum(seedlings), sum_juveniles = sum(juvenile)) %>%
  select(-subTurf, - Year) %>%
  distinct(turfID, .keep_all = TRUE) %>%
  group_by(blockID) %>%
  mutate(seed_diff = sum_seedlings - lag(sum_seedlings), juv_diff = sum_juveniles - lag(sum_juveniles)) %>%
  filter(TTtreat == "RTC")


recruitment$prec <- as.factor(c(0.6,1.2,2.0,2.7)[recruitment$Precipitation_level])
recruitment$temp <- as.factor(c(6.5,8.5,10.5)[recruitment$Temperature_level])
