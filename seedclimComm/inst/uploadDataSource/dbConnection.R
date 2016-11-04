con <- dbConnect(RMySQL::MySQL(), group = "seedclim")

filelistall<-dir(path = "rawdata/", full.names = TRUE, pattern = "csv$")


#import.data(c(filelist2009,filelist2010,filelist2011,filelist2012,filelist2013))
import.data(c(filelistall), con)     #run me
#import.data(c(filelist2011))
#import.data(c(filelist2012))
#import.data(c(filelist2013))
import.data(c(filelist2010))           #run me ####warning NA by coercion


wipe()

close(con)
