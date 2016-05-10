#load other data

#traits
traitsQ<-"SELECT taxon.*, [more traits].*, extraTraits.*
FROM (taxon LEFT JOIN [more traits] ON taxon.species = [more traits].species) LEFT JOIN extraTraits ON taxon.species = extraTraits.speciescode
ORDER BY taxon.species;"
traits<-dbGetQuery(con, traitsQ) 
traits$species.1 <- NULL
head(traits)
names(traits) <- make.names(names(traits))
traits[!is.na(traits$Flowering.start),c("F.Vår", "F.Fso", "F.Mso", "F.Sso", "F.Hø" )][is.na(traits[!is.na(traits$Flowering.start),c("F.Vår", "F.Fso", "F.Mso", "F.Sso", "F.Hø" )])]<-0
traits[!is.na(traits$Lower),c("Nem", "BNem", "SBor", "MBor", "Nbor", "LAlp", "MAlp", "HAlp" )][is.na(traits[!is.na(traits$Lower),c("Nem", "BNem", "SBor", "MBor", "Nbor", "LAlp", "MAlp", "HAlp" )])] <- 0


traits$alpine <- traits$Nem==0 & traits$BNem==0 & traits$SBor==0 & traits$LAlp==1
traits$lowland <- traits$HAlp==0 & traits$MAlp==0 & traits$LAlp==0 & traits$Nem==1

#fiter traits
traits <- traits[traits$species %in% names(cover),]
identical(as.character(traits$species), names(cover))

#############################################################
#load turf level environment

turfenvQ<-"SELECT blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, blocks_1.blockID AS originalblockID, plots_1.plotID AS originalplotID, turfEnvironment.year, turfEnvironment.pleuro, turfEnvironment.acro, turfEnvironment.liver, turfEnvironment.lichen, turfEnvironment.litter, turfEnvironment.soil, turfEnvironment.rock, turfEnvironment.totalVascular, turfEnvironment.totalBryophytes, turfEnvironment.totalLichen, turfEnvironment.vegetationHeight, turfEnvironment.mossHeight
FROM (plots AS plots_1 INNER JOIN blocks AS blocks_1 ON plots_1.blockID = blocks_1.blockID) INNER JOIN ((blocks INNER JOIN (plots INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON blocks.blockID = plots.blockID) INNER JOIN turfEnvironment ON turfs.turfID = turfEnvironment.turfID) ON plots_1.plotID = turfs.originPlotID 
where not turfs.TTtreat ='' AND ((Not (turfEnvironment.Year)=2010));"

turfenv<-dbGetQuery(con,turfenvQ)
summary(turfenv)
sort(rowSums(is.na(turfenv)))

names(turfenv) 


x11()
plot(turfenv$totalBryophytes, turfenv$pleuro+turfenv$acro+turfenv$liver)
abline(0,1)
abline(h=0, v=0)

x11()
plot(turfenv$totalLichen, turfenv$lichen)
abline(0,1)
abline(h=0, v=0)


