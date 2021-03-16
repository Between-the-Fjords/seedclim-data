####################
#todo############
#check species in taxon table before inserting
#merge subturf taxa
####################

#for loop
import_data <- function(file, con, merge_dictionary){
  #browser()
  print(file)
  chkft <- c("pleuro","acro", "liver", "lichen", "litter" ,"soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight")
  
  f <- readLines(file, warn = FALSE)  %>% 
    if_else(condition = substring(., 1, 1) == "\"", true = {
    gsub(pattern = "^\"", replacement = "", x = .) %>% #replace starting quote
      gsub(pattern = "\"\"", replacement = "\"", x = .) %>% #replace double quotes
      gsub(pattern = "\"\"$", replacement = "\"", x = .) %>% #replace double quotes that was part of a triple at end
      #gsub(pattern = "(?<!\\)),\"$", replacement = ",", x = ., perl = TRUE) #-ve look behind fixes Veskre comment problem
      gsub(pattern = ",\"$", replacement = ",", x = .) #-ve remove end quote
  }, false = .) %>% 
    paste(collapse = "\n")
    

  dat <- read.delim(text = f, sep = ";", dec = ",", stringsAsFactors = FALSE)#sep = ";", dec = ","
  if(ncol(dat) > 10){
    if(any(sapply(dat[, chkft], class) == "character")) 
      dat <- read.delim(text = f, sep = ";", dec = ".", stringsAsFactors = FALSE)#sep = ";", dec = "."  
  }else{
    dat <- read.delim(text = f, sep = ",", dec = ".", stringsAsFactors = FALSE)
   }
       
 
  names(dat) <- make.names(names(dat))
  dat <- dat %>%
    filter(!is.na(turfID), turfID != "", turfID != "turfID") %>% 
    mutate(
      turfID = trimws(turfID),
      comment = as.character(comment),
      year = as.integer(year)
      )
  head(dat)
  names(dat)
  
  Encoding(dat$comment) <- "latin1"
  
  #remove numeric suffix on duplicates
  names(dat) <- gsub("_\\d$", "", names(dat))

  #extract turf data####
  
  #fix typos in turfID
  dat <- dat %>% 
    mutate(turfID = recode(turfID, 
                           "515 TT4 247" = "515 TT4 274", 
                           "277 TTC" = "286 TTC",
                           "192 TT4 29" = "192 TT4 299"))
  
  turf <- dat %>% 
    select(turfID, matches("treat$"), one_of(c("originPlotID", "destinationPlotID"))) %>% 
    distinct() %>% 
    mutate_all(trimws)

  turf
  names(turf)
  
  alreadyIn <- dbGetQuery(con,"select turfID from turfs")
  newTurfs <- turf %>% 
    anti_join(alreadyIn) %>%  #find which turfs IDs are not already in database
    select(where(~!(all(is.na(.x))))) #remove empty columns to avoid type conflicts
  
  if(nrow(newTurfs) > 0) {
    message("adding ", paste(newTurfs$turfID, collapse = " "), " new turfs" )
    
    db_pad_write_table(con, "turfs", newTurfs, row.names = FALSE, append = TRUE)
    }

  message("done turfs")                                  
    
  #subTurf env ####
  subturfEnv <- dat %>% 
    filter(Measure != "Cover") %>% 
    select(turfID, subturf = subPlot, year, pleuro, acro, liver, lichen, litter, soil, rock, comment) %>% 
    mutate(subturf = as.integer(subturf))
  

  if(!is.null(dat$missing)){
     bad = dat$missing[dat$Measure != "Cover"]
     bad[is.na(bad)] <- ""
    subturfEnv <- subturfEnv %>% mutate(bad = bad)
  } else{
    subturfEnv <-  subturfEnv %>% mutate(bad = "")  
  }
  subturfEnv 
  db_pad_write_table(con, "subturf_environment", subturfEnv, row.names = FALSE, append = TRUE)
  nrow(subturfEnv)
    
    #TurfEnv ####
    turfEnv <- dat %>%
      filter(Measure == "Cover") %>% 
      select(turfID, year, pleuro, acro, liver, lichen, litter, soil, rock,
             total_vascular = totalVascular, 
             total_bryophytes = totalBryophytes, 
             total_lichen = totalLichen, 
             vegetation_height = vegetationHeight, 
             moss_height = mossHeight, 
             comment, recorder, date)
  
#  harmonise recorder names (supposed to be botanist but sometimes scribe)
  turfEnv <-  turfEnv %>% 
    mutate(
      recorder = case_when(
        recorder %in% c("", "not noted") | is.na(recorder) ~ "Not noted",
        recorder %in% c("VV", "vv", "W") ~ "Vigdis",
        recorder %in% c("KK") ~ "Kari",
        recorder %in% c("SO") ~ "Siri",
        TRUE ~ recorder
      ),
      recorder = str_replace(recorder,  "W(?![:alpha:])", "Vigdis"), 
      recorder = str_replace(recorder,  "KK(?![:alpha:])", "Kari")       
    )
  
  if(mode(turfEnv$moss_height) == "character"){
    turfEnv <- turfEnv %>% 
      mutate(moss_height = gsub(",", ".", moss_height),
             moss_height = as.numeric(moss_height))
    
  }
    
    if(any(nchar(as.character(turfEnv$comment)) > 255, na.rm = TRUE)) {
      stop ("more than 255 characters in a comment field in turfEnv")
    }
    db_pad_write_table(con, "turf_environment", turfEnv, row.names = FALSE, append = TRUE)
  nrow(turfEnv)   
  
  #TurfCommunity ####  
  spp <- dat %>% 
    filter(Measure == "Cover") %>% 
    select(turfID, year, (which(names(dat) == "recorder") + 1):(which(names(dat) == "pleuro") - 1)) %>% 
    gather(key = species, value = cover, -turfID, -year) %>% 
    filter(!is.na(cover), cover != 0) %>% #remove absent taxa
    mutate(
      cf = grepl("cf", cover, ignore.case = TRUE),
      cover = gsub("cf", "", cover, ignore.case = TRUE) #move any CF to new column
    ) 
  
  #oddity search
  spp %>% filter(is.na(as.numeric(cover)))  %>% count(cover)
  
  #merge synonyms
  spp <- spp %>% 
    left_join(merge_dictionary, by = c("species" = "oldID")) %>% 
    mutate(newID = coalesce(newID, species)) %>% 
    select(turfID, year, species = newID, cover, cf) %>%
    mutate(cover = as.numeric(cover)) %>% 
    filter(!is.na(cover)) %>% 
    group_by(year, turfID, species) %>% 
    summarise(cover = sum(cover), .groups = "drop") %>% #aggregate taxa
    filter(cover > 0)
  
  #check_new_taxa
  spp_list <- dbGetQuery(conn = con, statement = "select species from taxon")
  
  spp %>% anti_join(spp_list) %>% verify(nrow(.) == 0)
  
  #inject
  initNrowTurfCommunity <- dbGetQuery(con, "select count(*) as n from turf_community")
  db_pad_write_table(con, "turf_community", spp)
  finalNrowTurfCommunity <- dbGetQuery(con, "select count(*) as n from turf_community")

  stopifnot(nrow(spp) == finalNrowTurfCommunity - initNrowTurfCommunity)

                                              
  #subTurfCommunity  ####

  message("subturf_community")  
  
    subspp <- dat %>% 
      filter(Measure != "Cover") %>% 
      select(turfID, year, subturf = subPlot, (which(names(dat) == "recorder") + 1):(which(names(dat) == "pleuro") -1)) %>% 
      mutate(subturf = as.integer(subturf)) %>% 
      gather(key = species, value = presence, -turfID, -subturf, -year) %>%
      filter(!is.na(presence), presence != 0, presence != "")  #remove absent taxa

    # #oddity search
    subspp %>% count(presence)
    
  # #merge synonyms
   subspp <- subspp %>% 
       left_join(merge_dictionary, by = c("species" = "oldID")) %>% 
       mutate(newID = coalesce(newID, species)) %>% 
       select(turfID, subturf, year, species = newID, presence) %>% 
       group_by(year, turfID, subturf, species) %>% 
       summarise(presence = paste0(presence, collapse = ""), .groups = "drop") #aggregate taxa
     
     #check_new_taxa
     subspp %>% anti_join(spp_list) %>% verify(nrow(.) == 0)
    
    
    # subspp[subspp == 0] <- NA
    # subsppX <- lapply(unique(mergedNames), function(sppname){
    #   species <- subspp[, names(subspp) == sppname, drop = FALSE]
    #   if (ncol(species) == 1) {
    #     return(species)
    #   } else {
    #     apply (species, 1, function(r) {
    #       occurence <- which(!is.na(r))
    #       if(length(occurence) == 0) return(NA)
    #       if(length(occurence) == 1) return(r[occurence])
    #       else {
    #         warning(paste("more than one species observation in same subplot!"))
    #         write.csv(data.frame(filename = n, species = sppname, occurence = r[occurence]), file = "cooccurence_log.csv", append = TRUE)
    #         return(r[occurence][1])
    #       }
    #     })
    #   }
    # })
    # 
    # 
    # subsppX <- setNames(as.data.frame(subsppX), unique(mergedNames))
    # subspp <- cbind(subspp[, 1:3], subsppX)

     #euphrasia rule adults=adults+juvenile+seedling, j=j+s, s=s
     seedlingSp <- c("Euph.fri", "Eup.fri","Eup.sp","Eup.str","Euph.fri","Euph.sp", "Euph.str","Euph.str.1", "Euph.wet", "Poa.ann","Thlaspi..arv","Com.ten","Gen.ten", "Rhi.min", "Cap.bur", "Mel.pra","Mel.sp","Mel.syl","Noc.cae","Ste.med","Thl.arv","Ver.arv")
     
    subspp <- subspp %>% 
      mutate(
        cf = grepl("cf", presence, ignore.case = TRUE),
        fertile = grepl("F",presence, ignore.case = FALSE),
        dominant = grepl("D",presence, ignore.case = TRUE),
        vegetative = grepl("V",presence, ignore.case = TRUE),
        seedling_1 = grepl("S",presence, ignore.case = TRUE),
        seedling_n = stringr::str_extract(presence, pattern = "(?<=Sx)\\d+|\\d+(?=xS)"),
        seedling_n = as.integer(seedling_n),
        seedlings = case_when(
          !is.na(seedling_n) ~ seedling_n,
          seedling_1 ~ 1L,
          TRUE ~ 0L
        ),
        juvenile_1 = grepl("J",presence, ignore.case = TRUE),
        juvenile_n = stringr::str_extract(presence, pattern = "(?<=Jx)\\d+|\\d+(?=xJ)"),
        juvenile_n = as.integer(juvenile_n),
        juvenile = case_when(
          !is.na(juvenile_n) ~ juvenile_n,
          juvenile_1 ~ 1L,
          TRUE ~ 0L
        ), 
        adult = fertile|dominant|vegetative|grepl("1", presence)     ) %>% 
      select(-c(seedling_1, seedling_n, juvenile_1, juvenile_n)) %>% 
    #########more annuals?
    mutate(
      juvenile = if_else(species %in% seedlingSp & juvenile == 0L & adult, 1L, juvenile),
      seedlings = if_else(species %in% seedlingSp & seedlings == 0L & juvenile > 0L, juvenile, seedlings)
    ) %>% 
      mutate_at(vars(cf, fertile, dominant, vegetative, adult), as.integer)
  

    #inject
    initNrowSubTurfCommunity <- dbGetQuery(con, "select count(*) as n from subturf_community")
    db_pad_write_table(con, "subturf_community", subspp)
    finalNrowSubTurfCommunity <- dbGetQuery(con, "select count(*) as n from subturf_community")
    
    stopifnot(nrow(subspp) == finalNrowSubTurfCommunity - initNrowSubTurfCommunity)
    

    
  ############### Vigdis seedling problem #only for 2011 data     #############################

#     
# id_seedling <- subspp %>% 
#   filter(year == 2011, species != "seedling") %>% 
#   group_by(turfID) %>% 
#   summarise(n_seedlings = sum(seedling))
# 
# uid_seedlings <- subspp %>% 
#       filter(year == 2011, species == "seedling") %>% 
#   full_join(id_seedlings, by = turfID) %>%
#   left_join(dat %>% 
#               filter(year == 2011, measure == "cover") %>% 
#               select(turfID, recorder))
#     
#     
#   if(dat$year[1] == 2011 & FALSE){ #disable seedling error <---- FIX THIS!!!
#     seed <- dat[dat$TTtreat != "" & dat$Measure != "Cover", c("turfID","subPlot", "year", "seedlings", "recorder")]  #get data.frame of seedlings      N1
#     seed$subPlot <- as.integer(as.character(seed$subPlot))
#     seed$turfID <- factor(seed$turfID)
#     seedsum <- dbGetQuery(con, paste("select * from [number identified seedlings by subplot] where siteID='",dat$DestinationSite[1], "' and Year=2011", sep=""))     #sqlQuery database for number of seedlings per subplot N2
#     seed <- seed[order(seed$turfID, seed$subPlot),]
#   
#     head(seed)
#     head(seedsum)
#     
#     seed <- seed[!paste(seed$turf, seed$subPlot) %in% setdiff(paste(seed$turf, seed$subPlot), paste(seedsum$turf, seedsum$subTurf)),]#   then remove any missing rows as they have no species
#     
#     seed$seedlings[is.na(seed$seedlings)] <- 0
#   
#     seed$seedlings2 <- seed$seedlings
#     seed$seedlings2[seed$recorder == "W"]<-seed$seedlings[seed$recorder == "W"]-seedsum$SumOfseedlings[seed$recorder == "W"]#for VV /W subplots n seedlings N1 =  N1 - N2
#   
#     data.frame(seed$recorder, seed$seedlings, seedsum$SumOfseedlings, seed$seedlings2)
#   
#     #insert N1 into subturf_community as unident seedling
#   
#     seed <- seed[seed$seedlings2 > 0,]
#     seed <- data.frame(turfID = seed$turfID, year = seed$year, subTurf = seed$subPlot, species = "seed.unid", seedlings = seed$seedlings2, juvenile = 0,adult = 0,fertile = 0,vegetative = 0,dominant = 0, cf = 1)
#     dbWriteTable(con, "subturf_community", seed, row.names=FALSE, append = TRUE)
#   }
#   ######################### vigdis seedling problem fixed                                                                   
}
