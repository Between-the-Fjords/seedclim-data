#plot of realised against expected extinctions

ex.all<-extinctClasses(turfs$turfID[turfs$newTT!="control"])
dim(ex.all)
rownames(ex.all)

plot(
  sapply(ex.all["potentialExtinctions",],length),
  sapply(ex.all["expectedExtinctions",],length),
  col=TT.colours[turfs$TTtreat[turfs$newTT!="control"]],
  pch=20
)
abline(1,1)

boxplot(
  sapply(ex.all["expectedExtinctions",],length)/sapply(ex.all["potentialExtinctions",],length)~turfs$TTtreat[turfs$newTT!="control"]+turfs$Temperature_level[turfs$newTT!="control"],
  las=2, col=TT.colours)
boxplot(
  sapply(ex.all["expectedExtinctions",],length)/sapply(ex.all["potentialExtinctions",],length)~turfs$TTtreat[turfs$newTT!="control"]+turfs$Precipitation_level[turfs$newTT!="control"],
  las=2, col=TT.colours)

boxplot(
  sapply(ex.all["realisedImmigration",],length)/sapply(ex.all["potentialImmigration",],length)~turfs$TTtreat[turfs$newTT!="control"]+turfs$Temperature_level[turfs$newTT!="control"],
  las=2, col=TT.colours)
boxplot(
  sapply(ex.all["realisedImmigration",],length)/sapply(ex.all["potentialImmigration",],length)~turfs$TTtreat[turfs$newTT!="control"]+turfs$Precipitation_level[turfs$newTT!="control"],
  las=2, col=TT.colours)

