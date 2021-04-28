###############################################
# 
# ##cover data
# my.GR.data <- tbl(con, "turfCommunity") %>%
#   collect() %>% 
#   bind_rows(problems.cover) %>%
#   left_join(tbl(con, "taxon"), copy = TRUE) %>%
#   left_join(tbl(con, "turfs"), copy = TRUE) %>%
#   left_join(tbl(con, "plots"), by = c("destinationPlotID" = "plotID"), copy = TRUE) %>%
#   left_join(tbl(con, "blocks"), by = "blockID", copy = TRUE) %>%
#   left_join(tbl(con, "sites"), by = "siteID", copy = TRUE) %>%
#   left_join(tbl(con, "turfEnvironment"), copy = TRUE) %>%
#   select(siteID, blockID, destinationPlotID, originPlotID, turfID, TTtreat, Year = year, species, cover, Temperature_level, Precipitation_level, recorder, totalVascular, functionalGroup, vegetationHeight) %>%
#   filter(!is.na(TTtreat)) %>% 
#   collect()
# 
# #incorporate these into the code...
# 
# cover$NID.herb <- NULL
# cover <- cover[!is.na(rowSums(cover)),]
# 
# cover$Car.cap <- cover$Car.Cap + cover$Car.cap
# cover$Car.Cap <- NULL
# 
# cover$Gen.sp <- cover$Gen.sp.
# cover$Gen.sp. <- NULL
# 
# cover$Agr.cap <- cover$Agr.cap + cover$Agr.can
# cover$Agr.can <- NULL
# 
# cover$Pyr.sp <- cover$Pyr.min + cover$Pyr.rot
# cover$Pyr.min <- NULL
# cover$Pyr.rot <- NULL
# 
# cover$Epi.sp <- cover$Epi.sp + cover$Epi.ana
# cover$Epi.ana <- NULL

cover.thin <- tbl(con, sql("SELECT sites.siteID, blocks.blockID, turfs.TTtreat,turfs.turfID, dest_blocks.blockID AS destBlockID, (SELECT Count(subturf_environment.bad) AS CountOfbad
FROM subturf_environment where (subturf_environment.year = turf_community.year) AND (subturf_environment.turfID = turf_community.turfID)
 AND ( (subturf_environment.bad)='')) AS notbad, sites.temperature_level, sites.summer_temperature_gridded as summer_temperature, sites.annual_precipitation_gridded as annual_precipitation, sites.precipitation_level, turf_community.Year, turf_community.species, turf_community.cover, turf_environment.recorder , dest_blocks.siteID as destSiteID
FROM (((blocks AS dest_blocks INNER JOIN plots AS dest_plots ON dest_blocks.blockID = dest_plots.blockID) INNER JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) 
INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON dest_plots.plotID = turfs.destinationPlotID) INNER JOIN turf_community ON turfs.turfID = turf_community.turfID) INNER JOIN turf_environment ON (turf_environment.year = turf_community.year) AND (turfs.turfID = turf_environment.turfID) 
WHERE NOT turfs.TTtreat='' AND ((Not (turf_community.Year)=2010))")) %>% 
  collect()


if (FALSE){ #cleaner sql
sites_blocks_plots <- tbl(con, "sites") %>% 
  inner_join(tbl(con, "blocks"), by = "siteID") %>% 
  inner_join(tbl(con, "plots"), by = "blockID") %>% 
  select(siteID, norwegian_name, annual_precipitation_gridded, summer_temperature_gridded,
         temperature_level, precipitation_level, blockID, plotID)

cover.thin <- sites_blocks_plots %>% 
  inner_join(tbl(con, "turfs"), by = c("plotID" = "originPlotID")) %>% 
  inner_join(sites_blocks_plots, by = c("destinationPlotID" = "plotID"), suffix = c("_origin", "_dest")) %>% 
  inner_join(tbl(con, "turf_community"), by = "turfID") %>% 
  select(-plotID, -destinationPlotID, -cover_raw) %>% 
  filter(
    TTtreat != "", # only TTtreat
    !year == 2010  # no TTtreat data for 2010
  ) %>%
  collect() 


cover.thin

}        


                                       

# make fat table
cover <- cover.thin %>% spread(key = species, value = cover)

#make meta data
cover.meta <- cover %>% select(siteID:destSiteID) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("TTC","TT1", "TT2", "TT3", "TT4")))
  
  
turfs <- cover.meta[!duplicated(cover.meta$turfID),]

cover <- cover %>% select(-(siteID:destSiteID))


#set NID.seedling to  0/1
#cover$NID.seedling <- ifelse(cover$NID.seedling > 0,1,0) # leave this out for the moment
         
table(cover.meta$turfID, cover.meta$year)   
table(cover.meta$year, cover.meta$siteID, cover.meta$TTtreat)         
                                       
alltaxa <- TRUE
propertaxa <- !names(cover) %in% c("NID.seedling", "Car.sp", "Hie.sp", "Luz.sp",  "NID.gram", "NID.herb", "NID.rosett", "Pyr.sp")
noNIDseedlings <- !names(cover) %in% c("NID.seedling")

turfs$newTT <- turfs$TTtreat  #alternative TTtreat with combined controls
levels(turfs$newTT)[1:2] <- "control"

save(cover, cover.thin, cover.meta, turfs, file = "cover.Rdata")

