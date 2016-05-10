con<-odbcConnectAccess(paste(wd,"seedclim_2014-12-19.mdb", sep=""))

wd<-"o:\\data\\seedclim2014\\"

#file.choose()
filelist2009<-dir(path=paste(wd,"Seedclim 2009 sp.fix csv\\",sep=""), full.names=T)    #uncomment to loop    #needs to be directory of just csv files
filelist2011<-dir(path=paste(wd,"Seedclim 2011 sp.fix CSV\\",sep=""), full.names=T) 
filelist2012<-dir(path=paste(wd,"Seedclim 2012 sp.fix CSV\\",sep=""), full.names=T) 
filelist2013<-dir(path=paste(wd,"SeedClim 2013 fix CSV\\",sep=""), full.names=T)
filelist2010<-dir(path=paste(wd,"rtg2010\\v2\\",sep=""), pattern=".csv", full.names=T)


filelistall<-dir(path=paste(wd,"csv files\\",sep=""), full.names=T)


#import.data(c(filelist2009,filelist2010,filelist2011,filelist2012,filelist2013))
import.data(c(filelistall))     #run me
#import.data(c(filelist2011))
#import.data(c(filelist2012))
#import.data(c(filelist2013))
import.data(c(filelist2010))           #run me ####warning NA by coercion


wipe()

close(con)
