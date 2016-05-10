ellenQ <-"SELECT blocks.siteID, plots.blockID, Avg(ellenbergBrit.moisture) AS AvgOfmoisture, Avg(ellenbergBrit.light) AS AvgOflight, Avg(ellenbergBrit.pH) AS AvgOfpH, Avg(ellenbergBrit.nitrogen) AS AvgOfnitrogen, sites.Annualprecipitation_gridded, sites.Precipitation_level, sites.Temperature_level
FROM sites INNER JOIN (((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN ((taxon INNER JOIN ellenbergBrit ON taxon.speciesName = ellenbergBrit.speciesName) INNER JOIN new_TurfCommunity ON taxon.species = new_TurfCommunity.species) ON turfs.turfID = new_TurfCommunity.turfID) ON sites.siteID = blocks.siteID
WHERE (((turfs.TTtreat)='TT1')) OR (((turfs.TTtreat)='TTC'))
GROUP BY blocks.siteID, plots.blockID, sites.Annualprecipitation_gridded, sites.Precipitation_level, sites.Temperature_level;"

ellen<-sqlQuery(con, ellenQ) 
head(ellen)


moistQ<-"SELECT sites.siteID, blocks.blockID, Avg(soil_moist.moisture) AS AvgOfmoisture, sites.Temperature_level, sites.Precipitation_level
FROM soil_moist RIGHT JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON soil_moist.originPlotID = turfs.originPlotID
WHERE (((turfs.TTtreat)='TT1')) OR (((turfs.TTtreat)='TTC'))
GROUP BY sites.siteID, blocks.blockID, sites.Temperature_level, sites.Precipitation_level;"

moist<-sqlQuery(con, moistQ) 
head(moist)


eQ <-"SELECT taxon.species, ellenbergBrit.moisture , ellenbergBrit.light , ellenbergBrit.pH , ellenbergBrit.nitrogen 
FROM taxon INNER JOIN ellenbergBrit ON taxon.speciesName = ellenbergBrit.speciesName;"
e<-sqlQuery(con, eQ) 
head(e)
pairs(e[,-1])




boxplot(AvgOfmoisture~Precipitation_level+Temperature_level, data=moist )
boxplot(AvgOfmoisture~Precipitation_level+Temperature_level, data=ellen )
boxplot(AvgOfpH~Precipitation_level+Temperature_level, data=ellen )
boxplot(AvgOflight~Precipitation_level+Temperature_level, data=ellen )
boxplot(AvgOfnitrogen~Precipitation_level+Temperature_level, data=ellen )
