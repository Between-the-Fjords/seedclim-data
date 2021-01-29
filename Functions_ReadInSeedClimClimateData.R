####################################
# READ IN SEEDCLIM CLIMATE DATA
#          FUNCTIONS
####################################

#### IMPORT DATA ####

#### NAME CHECK FUNCTION ####
NameCheck <- function(textfile){
  textfile <- iconv(textfile, "latin1", "ASCII", sub = "q")

  if(grepl("ulv|ule", textfile)) {site <- "ulv"}
  else if(grepl("gud", textfile)) {site <- "gud"}
  else if(grepl("skj", textfile)) {site <- "skj"}
  else if(grepl("ram", textfile)) {site <- "ram"}
  else if(grepl("ves", textfile)) {site <- "ves"}
  else if(grepl("fau", textfile)) {site <- "fau"}
  else if(grepl("vik", textfile)) {site <- "vik"}
  else if(grepl("arn|arh", textfile)) {site <- "arh"}
  else if(grepl(".vs|qqvs", textfile)) {site <- "ovs"}
  else if(grepl("h.g|hqqg", textfile)) {site <- "hog"}
  else if(grepl("qqlr|aqqlr|.lr|aal|ålr|ål", textfile)) {site <- "alr"}
  else if(grepl("l.v|laqqv|lqqv|lqv", textfile)) {site <- "lav"}
  else site <- "panic"

  #check
   if(!site %in% c("fau", "alr", "ulv", "vik", "hog", "lav", "arh", "ram", "gud", "ovs", "ves", "skj"))stop(paste("wrong site from", textfile))
  return(site)
}


#### Read in ITAS or UTL ####
ReadData <- function(textfile){
  cat(textfile)
  cat("\n")
  first <- readLines(textfile, n = 1) # read first line to check which logger it is
  if(grepl("Label", first)){ #check format based on first line
    dat <- ReadInBodyITAS(textfile)
  } else if(grepl("Versi", first)){
    dat <- ReadInBodyUTL(textfile)
  } else {
    warning(paste(textfile, "format not recognised")) # warning if logger not recognized
    dat <- NULL
  }
  dat$file <- basename(textfile) # puts file in extra column
  return(dat)
}


textfile <- "/Volumes/felles/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/rawdata by Site//Skj/ITAS/Skjeldingahaugen_ITAS_111015_120705.txt"

#  READ IN ITAS LOGGERS
ReadInBodyITAS <- function(textfile){
  # import body of data
  dat <- readLines(textfile) %>% 
    gsub(pattern = "\xf8", replacement = "o", x = .) %>% # replace stupid multibyte character with o
    paste(collapse = "\n") %>% 
    read_delim(delim = "\t")
  
  names(dat) <- gsub("-", "", names(dat)) # remove "-" on some column names 
  
  dat <- dat %>% 
    mutate_all(na_if, "#-INF") %>% # remove #-Inf
    mutate_all(na_if, "#+INF") %>% 
    mutate_all(gsub, pattern = ",", replacement = ".") %>% # replace comma with dot
    slice(-1)  # delete first row with units

  
  # some files have an extra time column (should be in X2 column)
  if(any(names(dat) == "X2")){ 
    dat <- dat %>% 
      mutate(date = paste(gsub("(.*) .*", "\\1", Label), X2)) %>% 
      select(-X2, -Label)
  } 
  
  else {
    dat <- dat %>% rename(date = Label)
  }

  
  dat <- dat %>% 
     mutate_at(vars(-date), as.numeric) %>% 
     mutate(date = dmy_hms(date, truncated = 1, tz = "Africa/Algiers")) %>% 
     gather(key = logger, value = value, -date) %>% 
     mutate(logger = tolower(logger))
   
   
  # extract site name from file name
  dat$site <- NameCheck(basename(tolower(textfile))) # check the site name
  dat$type <- "ITAS"

  return(dat)
}

textfile <- "/Volumes/felles/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/rawdata by Site//Lav/UTL/1239_23062009.txt"
textfile <- "/Volumes/felles/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/rawdata by Site//Ves/UTL/#001047_20081030_0000_Veskre.txt"

##### READ IN UTL LOGGERS ####
ReadInBodyUTL <- function(textfile){
  # There are not always the same number of header lines, find last line with Sample
  f <- readLines(textfile, n = 30)
  skip <- which(f == "Sample")
  if(length(skip) != 1)stop(paste("no Sample", textfile)) # warming is there is no Sample
  
  # import data, without header
  dat <- read_delim(textfile, delim = "\t", skip = skip, col_names = FALSE, locale = locale(decimal_mark = ","))
  
  if(ncol(dat) == 1){
    dat <- read.table(textfile, header=FALSE, sep=" ", skip=skip, dec=",", stringsAsFactors = FALSE)
    dat <- data.frame(paste(dat[, 1], dat[, 2]), dat[, 3], stringsAsFactors = FALSE)
    names(dat) <- c("X1", "X2")
    dat <- dat %>% 
      as.tibble()
    #dat <- data_frame(X1 = paste(dat[[1]], dat[[2]]), X2 = dat[[3]])
  }
  if(ncol(dat) == 3){
    dat <- data_frame(X1 = paste(dat[[1]], dat[[2]], sep=" "), X2 = dat[[3]])
  }
  
  dat <- dat %>% 
    rename(date = X1, value = X2) %>% 
    mutate(date = ymd_hms(date, tz = "Africa/Algiers"),
           # replace all commas with dot and make numeric
           value = as.numeric(gsub(pattern = ",", replacement = ".", x = value)))
  
  # import head of data to extract logger and site name
  dat.h <- read.delim(textfile, header = FALSE, nrows = skip)
  
  temp.logger <- gsub(" ", "",dat.h$V2[4], fixed=TRUE) #extract logger: 30cm or 200cm, delete space between nr and unit
  
  # extract site
  site.logger <- dat.h$V2[3]
  
  # give a warning if logger name is wrong
  if(!temp.logger %in% c("200cm", "30cm")){
    warning(paste(textfile, temp.logger, "logger name wrong"))
    # label these files
    t.name <- temp.logger  
  }
  else {t.name <- paste("temp", temp.logger, sep="")}
  
  # rename temp column with temp logger name
  dat <- dat %>% 
    mutate(logger = t.name,
           site = site.logger)
  
  # Missing site information
  if(grepl("| |Nr. 1 - luft", site.logger)){
    #textfile)dat$site[1] == "" | dat$site[1] == " " 
    # extract site name from path
    SITE <- last(unlist(strsplit(x = dirname(textfile), split = "/")))
    # if site is UTL (from other place where data is stored)
    if(SITE == "UTL"){
      SITE <- unlist(strsplit(x = dirname(textfile), split = "/"))[length(unlist(strsplit(x = dirname(textfile), split = "/")))-1]
    }
    dat <- dat %>% 
      mutate(site = SITE)
    
    warning(paste("site imputed from path for", SITE, basename(textfile)))
  }
  
  
  # check site name
  dat$site <- NameCheck(tolower(dat$site[1]))
  dat$type <- "UTL"
  
  return(dat)
}





############################################################################################

#### PLOTS ####

# Function to the climate data and zoom in on different time steps. Data is temperature by default. log will include a list of loggers if inc = TRUE. If inc = FALSE it will take the opposite of the logger list.
plot_climate <- function(data = temperature, SITE, start_date = "2000.1.1", end_date = "2100.1.1", log, inc = TRUE, breaks = "month"){
  data %>% 
    filter(date > as.POSIXct(ymd(start_date)), date < as.POSIXct(ymd(end_date))) %>%
    filter(site == SITE) %>% 
    filter ((logger %in% log) == inc) %>% 
    ggplot(aes(x = date, y = value, colour = logger)) + 
    geom_line() +
    scale_x_datetime(date_breaks = breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    ggtitle(label = SITE)
}



# Function to plot climate and gridded data. log will include a list of loggers if inc = TRUE. If inc = FALSE it will take the opposite of the logger list.
plot_gridded_temp <- function(data, start_date = "2000.1.1", end_date = "2100.1.1", SITE, log, inc = TRUE, breaks = "month"){
  data %>% 
    filter(date > as.POSIXct(ymd(start_date)), date < as.POSIXct(ymd(end_date))) %>%
    filter ((logger %in% log) == inc) %>%
    filter (site %in% SITE) %>% 
    filter(value < 40) %>%
    ggplot(aes(x = date, y = value, colour = logger, size = logger)) + 
    geom_line() +
    scale_color_manual(values = c("darkgray", "lightblue")) +
    scale_size_manual(values = c(3,1)) +
    scale_x_datetime(date_breaks = breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    facet_wrap(~site) +
    xlab("") + ylab("Temperature in °C")
}
