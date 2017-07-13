#Hogesete plots

x11();reverse.hogsete.plot("Hogsete", dat=cover>0)
x11(5,5);
par(mar=c(2.6,2.6,.3,.3), mgp=c(1.1,.1,0), tcl=0.2)
hogsete.plot("Hogsete", dat=fsubturf)

sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  reverse.hogsete.plot(siteID, dat=fsubturf)
  title(main=siteID)
})
                                  
sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  hogsete.plot(siteID, dat=fsubturf)
  title(main=siteID)
})
