####################
#todo############
#check species in taxon table before inserting
#
####################


library (RODBC)
#library(gdata)
 #Trim   (getting rid of gdata..)
    trim <-function (s, recode.factor = TRUE, ...) 
{
    s <- sub(pattern = "^ +", replacement = "", x = s)
    s <- sub(pattern = " +$", replacement = "", x = s)
    s
}



wd<-"O:\\data\\SEEDCLIM2014\\"

db<-odbcConnectAccess(paste(wd,"seedclim_2014-5-20.mdb", sep=""))
sqlTables(db)



#file.choose()
mossfilelist<-dir(path=paste(wd,"bryophytes\\",sep=""), full.names=TRUE, pattern="csv")    #uncomment to loop    #needs to be directory of just csv files



#for loop
import.moss<-function(filelist){
  sapply(filelist,function(n){     
  
                                                                              #uncomment to loop
    #n<-"O:\\data\\SEEDCLIM2014\\Seedclim 2012 sp.fix CSV\\Lavisdalen2012 sp.fix.csv"                  #comment to loop
    print(n)
    dat<-read.table(n, sep=";", header=TRUE)  
         
    dat<-dat[!is.na(dat$originPlotID),]
    head(dat)
    names(dat)
    dat$turfID<-trim(dat$turfID)
 
   
  print(max(nchar(as.character(dat$comment)))) #how long is longest comment)
                                          
     #subTurfCommunity  
     print("mosssubturfcommunity")  
    subspp<-cbind(dat[,c("turfID", "year", "subPlot")],dat[, (which(names(dat)=="recorder")+1):(which(names(dat)=="BryoMeasured")) ])[dat$Measure=="Presence",]
    
    #Find oddities in datasett:
    spp0<-data.frame(turfID=NA, year=NA, subTurf=NA, bryoSpecies=NA)
    spp0<-spp0[-1,]
    lapply(4:ncol(subspp),function(nc){
      sp<-subspp[,nc ]
      spp2<-data.frame(turfID=subspp$turfID, year=subspp$year, subTurf=subspp$subPlot, bryoSpecies=names(subspp)[nc], adult=sp)
      spp2<-spp2[!is.na(spp2$adult)&spp2$adult==1,] #keep only rows with presences
      spp2
      if(nrow(spp2)>0)spp0<<-rbind(spp0,spp2[,1:4])
      invisible()
    })  
    sqlSave(db,spp0,"bryoSubTurfCommunity", rownames=F, append=T)       
  })                                                                                                      #uncomment to loop
}

import.moss(mossfilelist)

# Codes for deleting moss:
wipemoss<-function(){
    sqlQuery(db, "Delete * FROM bryoSubTurfCommunity")
    print("Moss wiped. Hope you really wanted to do that!")
}

wipemoss()
 
close(db)
