##read 2017 data

#load packages####
library("readxl")
library("tidyverse")
library("DBI")

#get turfs and species from database####
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")
taxonomy <- tbl(con, "taxon") %>% 
  select(species, speciesName) %>% 
  collect() 
turfs <- tbl(con, "turfs") %>% 
  select(turfID) %>% 
  collect()
dbDisconnect(con)

#taxonomic corrections####
tax_cor <- readr::read_csv(comment = "#",
"old_code, new_name
Phle.alp  , Phleum alpinum
Cer   , need turfmap to know. Maybe Cerastium cerasioides
Euph.sp.  , Euphrasia sp.
R..ac.la  ,Rumex acetosella
Tarax    ,Taraxacum sp.
Rhy.squ ,Rhytidiadelphus squarrosus a moss.... Disregard. 
Mosses  , Mosscover
Ran...  , Ranunculus - but need turfmap to know which spices (probably acris but could be auricomus or repens these are found in a few sites only)
Hyp.mac , Hypericum maculatum
X...           ,unknown perchance?
Dia.lan   ,Dianthus deltoids [or Plantago lanceolata??] might need to chk turfmap
Ran.acr.1 ,Ranunculus acris
Alch.sp  ,Alchemilla sp #Alchemilla spp
C..pil  ,Carex pilulifera
Euph.str  ,Euphrasia sp.#stricta (which are merged into Eup spp in the code)
Leo.vul   ,Leontodon autumnalis
Poa.prat  ,Poa pratensis
Englodnegras  , Holcus lanatus
Sedum.sp.  ,Sedum acre#acris
Sed.sp ,Sedum acre#acris
Lichen.1  , Lichen cover
Hol.lan  ,Holcus lanatus
Car.cer  ,Cerastium cerastoides
Rhin.min  ,Rhinanthus minor
Kna.vul  , Knautia arvensis
Vio.bal , Viola palustris 
Dia.med  , Dianthus deltoides OR Plantago media?? might need to chk turfmap
Pyr.sp.  ,Pyrola sp.
Car.vag.1  , Carex vaginata
Luz.sp.  , Luzula sp.
Pyrola.sp., Pyrola sp.
Sal.sp.  , Salix sp. 
X....1  , yeah. 
Car.nor.1  ,Carex norvegica
Salix.sp., Salix sp.
Ver.riv  ,Viola riviniana
Gal.uni  , Galium uliginosum
Seedl..  ,some sorry seedling....
Seedl.  ,some sorry seedling too....
Sag.sag ,Sagina sp.#saginoides (gets merged into sagina spp in the code)
Emp.sp  ,Empetrum hermaphroditum
Sal.lan  ,Salix lanata
Pyr.vul ,Pyrola rotundifolia# (might need turfmap)
Ante.dio  ,Antennaria dioica
Par.pal.1 , Parnassia palustris
Eup.str  ,Euphrasia sp.#stricta
Seedl.sp ..  ,some sorry seedling again....
Ort.sec  ,Orthilia secunda
Car.sp.1  , Carex sp.
car.pan , Carex panicea
Jun.alp.arc , Juncus alpinoarticulatus
Jun.arc.alp , Juncus alpinoarticulatus
Car.sp.  , Carex sp.
Car.pan.  ,Carex panicea
sel.sel ,Selaginella selaginoides
Dac.glom ,Dactylis glomerata
Holc.lana ,Holcus lanatus
Phle.pra ,Phleum pratense
Mype.mac ,Hypericum maculatum
Vio.riv.  ,Viola riviniana
Åkerplante , yeah. need to chik map. maybe even think.
Car.vag.  ,Carex vaginata
Ave.fle.1 ,Avenella flexuosa
Pru.vul.1 ,Prunella vulgaris
Rot.lun , Botrychium lunaria"
)

dim(tax_cor)
tax_cor %>% inner_join(taxonomy, by = c("new_name" = "speciesName"))
tax_cor %>% anti_join(taxonomy, by = c("new_name" = "speciesName")) %>% print(n = Inf)

#read excel file####
f <- read_excel("rawdata/2017_data/SeedClim17.xlsx", col_names = FALSE)
f

#find chunks ####
start <- grep("Date", f$X__1)
stop <- c(start[-1], nrow(f) + 1)

#import each chunk ####
data2017 <- map_df(1:length(start), .f = function(i){
  #meta data
  meta <- f %>% 
    slice(start[i]) %>% 
    select_if(negate(is.na)) %>% 
    gather() %>% 
    select(-key) %>% 
    separate(value, into = c("key", "value"), sep = ":") %>% 
    mutate(value = trimws(value))
  print(meta)
  
  #contents
  guts <- f %>% slice((start[i] + 1):(stop[i] - 1)) %>% 
    mutate(X__1 = if_else(row_number() == 1L, "subPlot", X__1)) %>% 
    filter(!is.na(X__1)) %>% 
    t() %>% 
    as_data_frame() 

  guts$V1[grep("%", guts$V1) + 1] <- "comment"
  guts <- guts %>% filter(!is.na(V1)) %>% 
    setNames(make.names(trimws(guts[1, ]), unique = TRUE)) %>% 
    slice(-1) %>% #remove old header row
    rename_at(vars(one_of(c("Pleuro", "Acro", "Liver", "Lichen", "Lichen.1", "Litter", "Soil", "Rock"))), tolower) %>% 
    rename(totalVascular = Tot.veg...., totalBryophytes = Bryo, vegetationHeight = Height..cm.,	mossHeight = Mosse.depth..cm.)
         
 #     "Mosses"???          
  
  #append metadata to contents
  bind_cols(siteID = meta %>% filter(key == "Site") %>% pull(value),
         blockID   = meta %>% filter(key == "Block") %>% pull(value),
         turfID    = meta %>% filter(key == "Plot") %>% pull(value),
         date      = meta %>% filter(key == "Date") %>% pull(value),
         recorder  = meta %>% filter(key == "Recorder" ) %>% pull(value)
  ) %>% 
    crossing(guts)
  
})

#fix Rhy.squ to pleuro
data2017 %>% 
  filter(!is.na(Rhy.squ)) %>% 
  select(siteID, blockID, turfID, date, recorder, subPlot, Rhy.squ, pleuro)

data2017 <- data2017 %>% 
  mutate(pleuro = coalesce(Rhy.squ, pleuro),
         lichen = coalesce(lichen, lichen.1),
         Par.pal = coalesce(Par.pal, Par.pal.1),
         Pru.vul = coalesce(Pru.vul, Pru.vul.1),
         Car.nor = coalesce(Car.nor, Car.nor.1), 
         Ave.fle = coalesce(Ave.fle, Ave.fle.1),
         Car.vag = coalesce(Car.vag, Car.vag.1),
         Ran.acr = coalesce(Ran.acr, Ran.acr.1)#entered twice
         ) %>% 
  select(-Rhy.squ, -lichen.1, -Par.pal.1, -Pru.vul.1, -Car.nor.1, -Ave.fle.1, -Car.vag.1, -Ran.acr.1)


#taxa with 1
names(data2017)[grep("1", names(data2017))] %>% 
  set_names() %>% 
  map(function(n){
    data2017 %>% 
      select_("siteID", "turfID", "subPlot", n, gsub("\\.1", "", n)) %>% 
      filter(!is.na(!!as.name(n)))
  }) 


#checks ####
extras <- c("block", "turfID", "recorder", "lichen", "litter", "soil", "totalVascular", "totalBryophytes", "pleuro", "acro", "liver", "subPlot", "rock", "vegetationHeight", "mossHeight", "site", "date")
names(data2017)[!names(data2017) %in% c(taxonomy$species, extras)] %>% sort()

turfs %>% arrange(turfID) %>% pull()
data2017 %>% anti_join(turfs, by = "turfID") %>% distinct(turfID) %>% arrange(turfID)


#fix turfIDs ####
data2017 <- data2017 %>%
  mutate(
    turfID = recode(
      turfID,
      "219 TT3 28" = "219 TT3 287",
      "26TT253" = "26 TT2 53",
      "33TT258" = "33 TT2 58",
      "505 TTC/RTC" = "505 TTC",
      "82 TT4 97" = "82 TT4 197",
      "TTC 17" = "17 TTC",
      "TTC 222" = "222 TTC",
      "TTC37" = "37 TTC",
      "TTC 99" =  "99 TTC",
      
      "367 TTC" = "307 TTC",   # at Øvstedal? (0 was written sloppily)
      "53TTC52" = "53 TT1 52", #(was correct on sheet)
      "63 TT3 142" = "67 TT3 142"# (was correct on sheet)
    )
  )


## more checks ####
data2017 %>% anti_join(turfs, by = "turfID") %>% distinct(turfID) %>% arrange(turfID)
data2017 %>% count(turfID) %>% arrange(desc(n))

names(data2017) 
#- check for names in taxon table
data2017 %>% distinct(site)
#site > sitecode
data2017 %>% distinct(turfID)
#check turfid
data2017 %>% distinct(block)
data2017 %>% distinct(recorder)

##fix comments
comments <- data2017 %>% 
  filter(subPlot == "comment") %>% 
  gather(key = species, value = comment, -(siteID:subPlot)) %>% 
  select(-subPlot) %>% 
  filter(!is.na(comment))

comments_condensed <- comments %>% 
  group_by(turfID) %>% 
  mutate(comment = paste(species, comment)) %>%
  summarise(comments = paste(comment, collapse = " | "))

data2017 <- data2017 %>% left_join(comments_condensed)

##reorder dataset
start <- c("siteID", "blockID", "turfID", "date", "subPlot", "recorder") 
end <- c("pleuro", "acro", "liver", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "Mosses", "vegetationHeight", "mossHeight", "comments")

spp <- setdiff(names(data2017), c(start, end))

data2017 <- data2017 %>% 
  select(one_of(start), one_of(spp), one_of(end))

