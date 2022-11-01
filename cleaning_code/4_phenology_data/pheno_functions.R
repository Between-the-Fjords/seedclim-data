#### READ IN HEAD OF PHENOLOGY DATA 2015 ####
ReadInHeadPhenology15 <- function(datasheet, site){
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  dat.h2 <- do.call(rbind, 
                    lapply(seq(3,ncol(dat.h),20),function(i){
                      x <- dat.h[ ,c(i)]
                      names(x) <- c("date", "weather", "name")
                      x <- c(x,week=dat.h[ 1,i+18])
                      x <- c(x,Site=site)
                      x <- c(x,doy=yday(dmy(dat.h[ 1,i])))
                      x
                    })
  )
  return(dat.h2)
}

#### READ IN PHENOLOGY DATA 2015 ####
ReadInBodyPhenology15 <- function(datasheet, site){
  # import body of data
  dat <- read.csv(datasheet, header=FALSE, sep=";", skip=4, stringsAsFactors=FALSE)
  head(dat)
  dat <- dat[dat$V2!="",] # get rid of empty lines, where no species
  dat$V2<-gsub("*", "", dat$V2,fixed = TRUE) # get rid of * and space
  dat$V2<-gsub(" ", "", dat$V2,fixed = TRUE)
  
  # loop to get turfID in all cells
  for (i in 2:nrow(dat)){
    if(nchar(dat$V1[i])==0){
      dat$V1[i] <- dat$V1[i-1]
    }
  }
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  
  # merge data into long data table
  long.table <- lapply(seq(3,ncol(dat)-19,20),function(i){
    x <- dat[ ,c(1:2,i:(i+19))]
    names(x) <- c("turfID", "species", paste(rep(c("v", "b", "f", "s", "r"), 4  ), rep(1:4, each=5), sep="."))
    x$week<-dat.h[1,i+18]
    x$doy <- yday(dmy(dat.h[1,i]))
    x  
  })
  dat.long <- do.call(rbind,c(long.table,stingsAsFactors=FALSE))
  
  # Extract turfID
  dat.long$turfID <- sapply(strsplit(dat.long$turfID, split=" - ", fixed=TRUE), function(x) (x[2]                                                                                       
  ))
  dat.long$Site <- site
  # convert to factor and numeric
  #sapply(dat.long[,c(4:7,9:12,14:17,19:22)],function(x)print(grep("\\D", x = x, value = TRUE))) # Check error messages
  dat.long <- cbind(dat.long[,c(1:3,8,13,18,23:25)],sapply(dat.long[,c(4:7,9:12,14:17,19:22)],as.numeric))
  #dat.long$turfID <- as.factor(dat.long$turfID)
  #dat.long$species <- as.factor(dat.long$species)
  #dat.long$week <- as.factor(dat.long$week)
  #dat.long$Site <- as.factor(dat.long$Site)
  dat.long
  return(dat.long)
}


#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEEDS PER TURFID AND SPECIES ####
CalcSums <- function(dat){
  dat$nr.b <- apply(dat[,c("b.1", "b.2", "b.3", "b.4")],1,sum, na.rm=TRUE)
  dat$nr.b[dat$nr.b == 0] <- NA
  dat$nr.f <- apply(dat[,c("f.1", "f.2", "f.3", "f.4")],1,sum, na.rm=TRUE)
  dat$nr.f[dat$nr.f == 0] <- NA
  dat$nr.s <- apply(dat[,c("s.1", "s.2", "s.3", "s.4")],1,sum, na.rm=TRUE)
  dat$nr.s[dat$nr.s == 0] <- NA
  dat$nr.r <- apply(dat[,c("r.1", "r.2", "r.3", "r.4")],1,sum, na.rm=TRUE)
  dat$nr.r[dat$nr.r == 0] <- NA
  return(dat)
}