#load data

subturf.thin <- tbl(con, "sites") %>%  
    inner_join(tbl(con, "blocks"), "siteID") %>% 
    inner_join(tbl(con, "plots") %>% select(plotID, blockID), by = "blockID") %>% 
    inner_join(tbl(con, "turfs"), by = c("plotID" = "originPlotID")) %>%
    inner_join(
      tbl(con, "plots") %>% 
                     select(plotID, destBlockID = blockID),
      by = c("destinationPlotID" = "plotID")
    ) %>% 
    inner_join(tbl(con, "subTurfCommunity"), by = "turfID") %>% 
    inner_join(tbl(con, "subTurfEnvironment") %>% 
                 select(turfID, year, subTurf, bad),
               by = c("turfID", "year", "subTurf")) %>% 
   inner_join(tbl(con, "turfEnvironment") %>% 
                select(turfID, year, recorder), 
              by  = c("turfID", "year")) %>% 
   filter(TTtreat != "", year != 2010) %>% 
   select(siteID, blockID, TTtreat, turfID, subTurf, year, species, destBlockID, bad, temperature_level, precipitation_level, recorder) %>% 
  collect()


# subturf.thin <- tbl(con, sql("SELECT sites.siteID, blocks.blockID, turfs.TTtreat, turfs.turfID, SubTurfCommunity.subTurf, SubTurfCommunity.Year, SubTurfCommunity.species, SubTurfCommunity.species, dest_blocks.blockID AS destBlockID, subTurfEnvironment.bad, sites.Temperature_level, sites.Precipitation_level, turfEnvironment.recorder
# FROM (((blocks AS dest_blocks INNER JOIN plots AS dest_plots ON dest_blocks.blockID = dest_plots.blockID) INNER JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON dest_plots.plotID = turfs.destinationPlotID) 
# INNER JOIN (SubTurfCommunity INNER JOIN subTurfEnvironment ON (SubTurfCommunity.Year = subTurfEnvironment.year) AND (SubTurfCommunity.subTurf = subTurfEnvironment.subTurf)) ON (turfs.turfID = subTurfEnvironment.turfID) AND (turfs.turfID = SubTurfCommunity.turfID)) INNER JOIN turfEnvironment ON (turfEnvironment.year = SubTurfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID) 
# WHERE ((Not (turfs.TTtreat)='')) AND ((Not (SubTurfCommunity.Year)=2010))")) 
                                                                          
subturf.thin
       
#fat       
subturf <- subturf.thin %>% 
  mutate(one = 1) %>% 
  spread(key = species, value = one, fill = 0)

  

subturf.meta <- subturf %>% 
  select(siteID:recorder) %>% 
  mutate(TTtreat = factor(TTtreat, levels=c("TTC","TT1", "TT2", "TT3", "TT4")))
  
subturf <- subturf %>% 
  select(-(siteID:recorder)) 

#stomping correction
boxplot(rowSums(subturf) ~ bad, data = subturf.meta, notch = TRUE, names = c("good", "stomped"))
boxplot(rowSums(subturf) ~ recorder + temperature_level, data=subturf.meta, notch = TRUE, las = 2)

subturf.meta[subturf.meta$bad == "x",]
#impute data from previous year for damaged subplots


#aggregate to turf
fsubturf <- subturf.thin %>% 
  count(turfID, year, species) %>% 
  spread(key = species, value = n, fill = 0) %>% 
  semi_join(cover.meta)#remove overstomped turf


boxplot(rowSums(select(fsubturf, -year, -turfID)) ~ recorder + temperature_level, data = cover.meta, notch=TRUE, las=2)
boxplot(rowSums(select(fsubturf, -year, -turfID) > 0) ~ recorder + temperature_level, data = cover.meta, notch=TRUE, las=2)


#add Val.sam
fsubturf$Val.sam <- 0

#cleanup
rm(subturf)
