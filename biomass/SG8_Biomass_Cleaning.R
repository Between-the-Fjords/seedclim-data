### SG.8 Biomass Allocation  ###
# Data cleaning: Sonya R. Geange

# This contains the biomass related data used in the Skarpaas et al. 2016 publication <https://www.sciencedirect.com/science/article/pii/S1433831916300063> , which draws upon the masters thesis work of Bargmann 2009, and Potch 2010.

# Load Packages
library(tidyverse)
library("tidylog", warn.conflicts = FALSE)

## DOWNLOAD DATA
# Get data from OSF Repository
# Use this code to download the data directly from OSF and unzip the file.
# Alternatively you can find the raw data here: https://osf.io/npfa9/

### Download data from OSF
#install.packages("remotes")
#remotes::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
get_file(node = "npfa9",
         file = "SG8_Biomass.zip",
         path = "biomass/data",
         remote_path = "6_Biomass_data/Raw_data")

zipFile <- "biomass/data/SG8_Biomass.zip"
outDir <- "biomass/data/raw"
unzip(zipFile, exdir = outDir)


# General Information Required
# Sites
sitename <- c("Ulvehaugen","Alrust","Fauske","Lavisdalen","Hogsete",
              "Vikesland","Gudmedalen","Rambera","Arhelleren",
              "Skjellingahaugen","Veskre","Ovstedal")
site <- c("U","Al","F","L","H","Vi","G","R","Ar","S","Ve","O")

# Climate
climate <- read.delim("biomass/data/raw/Clim_dat_daily_all.txt",dec=",")

###########################################################
# Data prep for Carex pallescens
# Based off original 'dataprep cpal.r' 
###########################################################

cpalseeds <- read.delim("biomass/data/raw/cpal seeds all sites.txt",
                        sep="\t",dec=",")
cpalseeds$IND <- gsub("\\[","",cpalseeds$IND)
cpalseeds$IND <- gsub("\\]","",cpalseeds$IND)
f <- function(x,split) unlist(strsplit(x,split))[1]
cpalseeds$IND <- sapply(cpalseeds$IND,f,split="\\(")
cpalseeds$IND <- paste(cpalseeds$Site,"-",cpalseeds$IND,sep="")
temp <- aggregate(cpalseeds[,c(6,10,11)],by=list(cpalseeds$IND),FUN="sum")
i <- which(!duplicated(cpalseeds$IND))
cpalseeds <- data.frame(cpalseeds[i,c(1,2)],temp)
cpalseeds$BMseeds <- cpalseeds$seeds*cpalseeds$weight..g./cpalseeds$no..Of.seeds

cpal <- read.delim("./biomass/data/raw/pall gen.txt",sep=";",dec=".")
cpal$siteID <- cpal$site
levels(cpal$siteID) <- substr(levels(cpal$siteID),1,1)

cpal <- merge(cpal,cpalseeds,all=T)
cpal$BMseeds[is.na(cpal$BMseeds)] <- 0


#######
# Aggregated Biomass Values
# Unsure if we want these for the SeedClim OSF?
######

cpal$RBM <- cpal$BioR*1000							# Root (+rhizome) biomass per ramet in mg
#cpal$SBM <- cpal$BioS*1000*0.75				# Stem biomass per genet in mg; split 3:1 among stem and infloresence
cpal$SBM <- 0  					                # Counting all "stem" biomass as infloresence
cpal$LBM <- cpal$BioL*1000							# Leaf biomass per genet in mg
#cpal$FBM <- cpal$BioS*1000*0.25 + cpal$BMseeds*1000			# Infloresence biomass not measured (but stems are infloresences)
cpal$FBM <- cpal$BioS*1000 + cpal$BMseeds*1000  		# Infloresence biomass not measured (but stems are infloresences)
cpal$BM <- cpal$RBM+cpal$SBM+cpal$LBM			# Total sterile biomass per genet in mg
cpal$BMTOT <- cpal$BM+cpal$FBM						# Total biomass per genet in mg

#####
# Didn't copy across logging of variables, or merging with climate
#####



###########################################################
# Data prep Carex capillaris
# Based off original 'dataprep ccap.r' 
###########################################################

# Read in Carex Capillaris seed dataset
ccapseeds <- read.delim("./biomass/data/raw/ccap seeds all sites.txt",sep="\t",dec=",")
ccapseeds$IND <- gsub("\\[","",ccapseeds$IND)
ccapseeds$IND <- gsub("\\]","",ccapseeds$IND)
f <- function(x,split) unlist(strsplit(x,split))[1]
ccapseeds$IND <- sapply(ccapseeds$IND,f,split="\\(")
ccapseeds$IND <- paste(ccapseeds$Site,"-",ccapseeds$IND,sep="")
temp <- aggregate(ccapseeds[,c(7,10,11)],by=list(ccapseeds$IND),FUN="sum")
i <- which(!duplicated(ccapseeds$IND))
ccapseeds <- data.frame(ccapseeds[i,c(1,2)],temp)
ccapseeds$BMseeds <- ccapseeds$seeds*ccapseeds$weight..g./ccapseeds$no..Of.seeds

# Read in Carex capillaris trait and biomass datasest
ccap <- read.delim("./biomass/data/raw/cap gen.txt",sep=";",dec=".")
ccap$siteID <- ccap$site
levels(ccap$siteID) <- substr(levels(ccap$siteID),1,1)

# Merge seed and trait/biomass datasets
ccap <- merge(ccap,ccapseeds,all=T)
ccap$BMseeds[is.na(ccap$BMseeds)] <- 0


#######
# Aggregated Biomass Values
# Unsure if we want these for the SeedClim OSF?
######

ccap$RBM <- ccap$BioR*1000							# Root (+rhizome) biomass per ramet in mg
#ccap$SBM <- ccap$BioS*1000*0.75				# Stem biomass per genet in mg; split 3:1 among stem and infloresence
ccap$SBM <- 0  					                # Counting all "stem" biomass as infloresence
ccap$LBM <- ccap$BioL*1000							# Leaf biomass per genet in mg
#ccap$FBM <- ccap$BioS*1000*0.25 + ccap$BMseeds*1000			# Infloresence biomass not measured (but stems are infloresences)
ccap$FBM <- ccap$BioS*1000 + ccap$BMseeds*1000  		# Infloresence biomass not measured (but stems are infloresences)
ccap$BM <- ccap$RBM+ccap$SBM+ccap$LBM+ccap$FBM				# Total sterile biomass per genet in mg
ccap$BMTOT <- ccap$BM								# Total biomass per genet in mg

#####
# Didn't copy across logging of variables, or merging with climate
#####


###########################################################
# Data prep Veronica alpina
# Based off original 'dataprep vpal.r' 
###########################################################

# Read in raw files
valp <- read.delim("./biomass/data/raw/VeronicaAlpinaSimplifiedAllSites.txt",
                   dec=",",na.strings=c("?"))
valp <- valp |> 
  mutate(Site = recode(Site, "Vikafjell" = "Skjellingahaugen"))
#levels(valp$Site)[5] <- "Skjellingahaugen"
valp$siteID <- valp$Site
# converting sitenames to siteIDs
levels(valp$siteID) <- site[match(levels(valp$Site),sitename)]
# Filling in missing genet IDs
for(i in 2:length(valp$IDG)) if(valp$IDG[i]=="") valp$IDG[i] <- valp$IDG[i-1]	# New unique Ramet IDs
valp$IDR <- factor(paste(valp$siteID,".",valp$IDG,".",valp$IDS,sep=""))
# New unique Genet IDs
valp$IDGsite <- factor(paste(valp$siteID,".",valp$IDG,sep=""))	


# Traits and Biomass
# Root (+rhizome) biomass per ramet in mg
valp$RBM <- (valp$rhiz.+valp$roots)*1000
# Stem biomass per ramet in mg
valp$SBM <- valp$stems*1000
# Leaf biomass per ramet in mg
valp$LBM <- valp$leaves*1000
# Reproductive biomass (flower parts) per ramet in mg
valp$FBM <- valp$r*1000
# Total sterile biomass per ramet in mg
valp$BM <- (valp$ag + valp$bg)*1000
# Total biomass per ramet in mg
valp$BMTOT <- valp$BM + valp$FBM


# Ratio of total above-ground to root biomass; excluding shoots with no roots
valp$AB <- (valp$SBM+valp$LBM+valp$FBM)/valp$RBM; valp$AB[valp$AB==Inf] <- NA	# Ratio of shoot to root biomass; excluding shoots with no roots		
valp$SR <- valp$SBM/valp$RBM; valp$SR[valp$SR==Inf] <- NA	
# Ratio of leaf (photosynthetic) to root biomass; excluding shoots with no roots
valp$LR <- valp$LBM/valp$RBM; valp$LR[valp$LR==Inf] <- NA	
# Ratio of fertile to sterile biomass
valp$FV <- valp$FBM/valp$BM			
# Ratio of fertile to photosynthetic biomass
valp$FL <- valp$FBM/valp$LBM				


#####
# Didn't copy across logging of variables
# Not copying across trait values
####


# Site IDs
valp$site <- site[match(valp$Site,sitename)]						

# Excluding two lost shoots and ramets with only root biomass
valp <- valp[valp$ag>0,]								


#######
# Aggregated Biomass Values by genet
# Unsure if we want these for the SeedClim OSF?
######


# Data prep: summarizing biomass by genet
valp.genet <- aggregate(valp[,c("RBM","SBM","LBM","FBM","BM","BMTOT")],by=list(IDGsite=valp$IDGsite),sum)
# Ratio of total above-ground to root biomass; excluding shoots with no roots
valp.genet$AB <- (valp.genet$SBM+valp.genet$LBM+valp.genet$FBM)/valp.genet$RBM; valp.genet$AB[valp.genet$AB==Inf] <- NA			
valp.genet$SR <- valp.genet$SBM/valp.genet$RBM; valp.genet$SR[valp.genet$SR==Inf] <- NA
valp.genet$LR <- valp.genet$LBM/valp.genet$RBM; valp.genet$LR[valp.genet$LR==Inf] <- NA
valp.genet$FV <- valp.genet$FBM/valp.genet$BM
valp.genet$FL <- valp.genet$FBM/valp.genet$LBM


# NO logged traits, climate data, or non-biomass traits copied across



###########################################################
# Data prep Viola biflora
# Based off original 'dataprep vbif.r' 
###########################################################

# Data prep Viola biflora
vbif <- read.delim("./biomass/data/raw/ViolaBifloraSimplifiedAllSites.txt",
                   dec=",",na.strings=c("?"))
vbif <- vbif |> 
  mutate(Site = recode(Site, "Vikafjell" = "Skjellingahaugen"))
#levels(vbif$Site)[5] <- "Skjellingahaugen"
vbif$siteID <- vbif$Site
# converting sitenames to siteIDs
levels(vbif$siteID) <- site[match(levels(vbif$Site),sitename)]			
for(i in 2:length(vbif$IDG)) if(vbif$IDG[i]=="") vbif$IDG[i] <- vbif$IDG[i-1]	# Filling in missing genet IDs
vbif$IDR <- factor(paste(vbif$siteID,".",vbif$IDG,".",vbif$IDS,sep=""))		# New unique Ramet IDs
vbif$IDGsite <- factor(paste(vbif$siteID,".",vbif$IDG,sep=""))			# New unique Genet IDs


#######
# Aggregated Biomass Values per ramet
# Unsure if we want these for the SeedClim OSF?
######
vbif$RBM <- vbif$Tu.Rt*1000									# Root (+tuber) biomass per ramet in mg
vbif$SBM <- vbif$SStem.T*1000									# Stem biomass per ramet in mg
vbif$LBM <- vbif$Leaf.T*1000									# Leaf biomass per ramet in mg
vbif$FBM <- (vbif$PStem.T+vbif$Fruit.T)*1000						# Reproductive biomass (flower parts, including infloresence) per ramet in mg
vbif$BM <- rowSums(vbif[,c("RBM","SBM","LBM")],na.rm=T)				# Total sterile biomass per ramet in mg
vbif$BMTOT <- vbif$Total*1000									# Total biomass per ramet in mg


#######
# Ratios for Biomass
# Unsure if we want these for the SeedClim OSF?
######
vbif$AB <- (vbif$SBM+vbif$LBM+vbif$FBM)/vbif$RBM; vbif$AB[!is.finite(vbif$AB)] <- NA	# Ratio of total above-ground to below-ground biomass; excluding shoots with no roots
vbif$SR <- vbif$SBM/vbif$RBM; vbif$SR[vbif$SR==Inf] <- NA				# Ratio of shoot to root biomass; excluding shoots with no roots
vbif$LR <- vbif$LBM/vbif$RBM; vbif$LR[vbif$LR==Inf] <- NA				# Ratio of leaf (photosynthetic) to root biomass; excluding shoots with no roots
vbif$FV <- vbif$FBM/vbif$BM									# Ratio of fertile to sterile biomass
vbif$FL <- vbif$FBM/vbif$LBM									# Ratio of fertile to photosynthetic biomass


# NO logged traits, climate data, or non-biomass traits copied across



###########################################################
# Data prep Viola palustris
# Based off original 'dataprep vpal.r' 
###########################################################

# Data prep Viola palustris

vpal <- read.delim("./biomass/data/raw/ViolaPalustrisSimplifiedAllSites.txt",
                   dec=",",na.strings=c("?"))
vpal$siteID <- vpal$Site
levels(vpal$siteID) <- site[match(levels(vpal$Site),sitename)]			# converting sitenames to siteIDs
for(i in 2:length(vpal$IDG)) if(vpal$IDG[i]=="") vpal$IDG[i] <- vpal$IDG[i-1]	# Filling in missing genet IDs
vpal$IDR <- factor(paste(vpal$siteID,".",vpal$IDG,".",vpal$IDS,sep=""))		# New unique Ramet IDs
vpal$IDGsite <- factor(paste(vpal$siteID,".",vpal$IDG,sep=""))			# New unique Genet IDs

#######
# Aggregated Biomass Values per ramet
# Unsure if we want these for the SeedClim OSF?
######
vpal$RBM <- (vpal$Tu.Rt+vpal$St.Rt-vpal$Stolon)*1000					# Root (root+tuber) biomass per ramet in mg
vpal$SBM <- (vpal$SStem.T+vpal$Stolon)*1000						# Stem + stolon biomass per ramet in mg
vpal$LBM <- vpal$Leaf.T*1000									# Leaf biomass per ramet in mg
vpal$FBM <- (vpal$PStem.T+vpal$Fruit.T)*1000						# Reproductive biomass (flower parts and stem) per ramet in mg
vpal$BM <- rowSums(vpal[,c("RBM","SBM","LBM")],na.rm=T)				# Total sterile biomass per ramet in mg
vpal$BMTOT <- vpal$Total*1000									# Total biomass per ramet in mg

########################
# Biomass Ratios
vpal$AB <- (vpal$SBM+vpal$LBM+vpal$FBM)/vpal$RBM; vpal$AB[vpal$AB==Inf] <- NA			# Ratio of total above-ground to root biomass; excluding shoots with no roots
vpal$SR <- vpal$SBM/vpal$RBM; vpal$SR[vpal$SR==Inf] <- NA				# Ratio of shoot to root biomass; excluding shoots with no roots
vpal$LR <- vpal$LBM/vpal$RBM; vpal$LR[vpal$LR==Inf] <- NA				# Ratio of leaf (photosynthetic) to root biomass; excluding shoots with no roots
vpal$FV <- vpal$FBM/vpal$BM									# Ratio of fertile to sterile biomass
vpal$FL <- vpal$FBM/vpal$LBM									# Ratio of fertile to photosynthetic biomass

# NO logged traits, climate data, or non-biomass traits copied across

vpal$site <- site[match(vpal$Site,sitename)]						# Site IDs


#############################################
# Data prep: summarizing biomass by genet
#############################################
vpal.genet <- aggregate(vpal[,c("RBM","SBM","LBM","FBM","BM","BMTOT")],by=list(IDGsite=vpal$IDGsite),sum)
vpal.genet$AB <- (vpal.genet$SBM+vpal.genet$LBM+vpal.genet$FBM)/vpal.genet$RBM; vpal.genet$AB[vpal.genet$AB==Inf] <- NA	# Ratio of total above-ground to root biomass; excluding shoots with no roots
vpal.genet$SR <- vpal.genet$SBM/vpal.genet$RBM; vpal.genet$SR[vpal.genet$SR==Inf] <- NA
vpal.genet$LR <- vpal.genet$LBM/vpal.genet$RBM; vpal.genet$LR[vpal.genet$LR==Inf] <- NA
vpal.genet$FV <- vpal.genet$FBM/vpal.genet$BM
vpal.genet$FL <- vpal.genet$FBM/vpal.genet$LBM


# NO logged traits, climate data, or non-biomass traits copied across




###########################################################
# Data prep Veronica officinalis 
# Based off original 'dataprep voff.r' 
###########################################################

# Data prep Veronica officinalis 

# ramet level

voff <- read.delim("./biomass/data/raw/VeronicaOfficinalisSimplifiedCorrectedag.txt",
                   dec=",",na.strings=c("?","-","*"))
voff$siteID <- voff$Site
levels(voff$siteID) <- site[match(levels(voff$Site),sitename)]			# converting sitenames to siteIDs
for(i in 2:length(voff$IDG)) if(voff$IDG[i]=="") voff$IDG[i] <- voff$IDG[i-1]	# Filling in missing genet IDs
voff$IDR <- factor(paste(voff$siteID,".",voff$IDG,".",voff$IDS,sep=""))			# New unique Ramet IDs
voff$IDGsite <- factor(paste(voff$siteID,".",voff$IDG,sep=""))				# New unique Genet IDs
# next line does not work
#voff <- within(voff,IDGsite <- factor(siteID:IDG))


#######
# Aggregated Biomass Values per ramet
# Unsure if we want these for the SeedClim OSF?
######
voff$RBM <- voff$bg*1000									# Root (+rhizome) biomass per ramet in mg
voff$SBM <- voff$stems*1000									# Stem biomass per ramet in mg
voff$LBM <- voff$leaves*1000									# Leaf biomass per ramet in mg
voff$FBM <- voff$r*1000										# Reproductive biomass (flower parts) per ramet in mg
voff$BM <- (voff$ag + voff$bg)*1000								# Total sterile biomass per ramet in mg
voff$BMTOT <- rowSums(voff[,c("BM","FBM")],na.rm=T)					# Total biomass per ramet in mg
voff$AB <- (voff$SBM+voff$LBM+voff$FBM)/voff$RBM; voff$AB[voff$AB==Inf] <- NA			# Ratio of total above-ground to root biomass; excluding shoots with no roots
voff$SR <- voff$SBM/voff$RBM; voff$SR[voff$SR==Inf] <- NA				# Ratio of shoot to root biomass; excluding shoots with no roots
voff$LR <- voff$LBM/voff$RBM; voff$LR[voff$LR==Inf] <- NA				# Ratio of leaf (photosynthetic) to root biomass; excluding shoots with no roots
voff$FV <- voff$FBM/voff$BM									# Ratio of fertile to sterile biomass
voff$FL <- voff$FBM/voff$LBM									# Ratio of fertile to photosynthetic biomass


# Removed logged values
# Traits (probably not needed here)

voff$site <- site[match(voff$Site,sitename)]						# Site IDs


#######
# Data prep: summarizing biomass by genet
# Unsure if we want these for the SeedClim OSF?
######

voff.genet <- aggregate(voff[,c("RBM","SBM","LBM","FBM","BM","BMTOT")],
                        by=list(IDGsite=voff$IDGsite),sum)
voff.genet$AB <- (voff.genet$SBM+voff.genet$LBM+voff.genet$FBM)/voff.genet$RBM; voff.genet$AB[voff.genet$AB==Inf] <- NA			# Ratio of total above-ground to root biomass; excluding shoots with no roots
voff.genet$SR <- voff.genet$SBM/voff.genet$RBM; voff.genet$SR[voff.genet$SR==Inf] <- NA
voff.genet$LR <- voff.genet$LBM/voff.genet$RBM; voff.genet$LR[voff.genet$LR==Inf] <- NA
voff.genet$FV <- voff.genet$FBM/voff.genet$BM
voff.genet$FL <- voff.genet$FBM/voff.genet$LBM

# NO logged traits, climate data, or non-biomass traits copied across


#################################################
#
#  Align and merge datasets (6 species)
#
#################################################



#################################
# Carex Species
#################################
# Rename sites to match OSF
unique(ccap$siteID)
ccap <- ccap %>%
  mutate(siteID = ifelse(
    IND %in% c(grep("G-", IND, value = T)),
    "Gudmedalen",
    ifelse(
      IND %in% c(grep("L-", IND, value = T)),
      "Lavisdalen",
      ifelse(
        IND %in% c(grep("R-", IND, value = T)),
        "Rambera",
        ifelse(
          IND %in% c(grep("S-", IND, value = T)),
          "Skjelingahaugen",
          ifelse(
            IND %in% c(grep("A-", IND, value = T)),
            "Arhelleren",
            ifelse(
              IND %in% c(grep("H-", IND, value = T)),
              "Hogsete",
              ifelse(
                IND %in% c(grep("O-", IND, value = T)),
                "Ovstedalen",
                ifelse(IND %in% c(grep("Ø-", IND, value =
                                         T)), "Ovstedalen",
                       ifelse(IND %in% c(
                         grep("V-", IND, value = T)
                       ), "Veskre",
                       NA))
              )
            )
          )
        )
      )
    )
  ))

# ccap: Keep biomass only related traits
ccap_subset <- ccap %>% 
  select(c("siteID","IND",
           "BioR","BioS","BioL", "BioSS","BioDL",
           "BMseeds", "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT"         
  )) %>% 
  # Rename key traits
  rename(c(roots = BioR, stems = BioS, leaves = BioL))
# Add species column
ccap_subset$species <- rep("Car.cap",nrow(ccap_subset))


# Rename sites to match OSF
unique(cpal$siteID)

cpal <- cpal %>%
  mutate(siteID = ifelse(IND %in% c(grep("G-", IND, value=T)), "Gudmedalen",
                         ifelse(IND %in% c(grep("L-", IND, value=T)), "Lavisdalen",
                                ifelse(IND %in% c(grep("R-", IND, value=T)), "Rambera",
                                       ifelse(IND %in% c(grep("S-", IND, value=T)), "Skjelingahaugen",
                                              ifelse(IND %in% c(grep("A-", IND, value=T)), "Arhelleren",
                                                     ifelse(IND %in% c(grep("H-", IND, value=T)), "Hogsete",
                                                            ifelse(IND %in% c(grep("O-", IND, value=T)), "Ovstedalen",
                                                                   ifelse(IND %in% c(grep("Ø-", IND, value=T)), "Ovstedalen",
                                                                          ifelse(IND %in% c(grep("V-", IND, value=T)), "Veskre",
                                                                                 NA))))))))))

# cpal: keep biomass only related traits
cpal_subset <- cpal %>% 
  select(c("siteID","IND", 
           "BioR","BioS","BioL", "BioSS","BioDL",
           "BMseeds", "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT"         
  )) %>% 
  # Rename key traits
  rename(c(roots = BioR, stems = BioS, leaves = BioL))
# Add species column
cpal_subset$species <- rep("Car.pal",nrow(cpal_subset))

# Create a combined Carex dataframe
# THese have some different characteristics to the others
Carex_Combined <- full_join(cpal_subset, ccap_subset)

# Pivot longer and drop na's
Carex_Combined_long <- Carex_Combined %>% 
  pivot_longer(cols = roots:BMTOT,
               names_to = "Biomass_Attributes", values_to = "Values",
               values_drop_na = TRUE)

#################################
# Viola Species
#################################


# vbif: keep biomass only related traits
vbif_subset <- vbif %>% 
  select(c( "siteID", "IDR", "IDGsite", "IDG","SH.1", "PH.1", "PH0stem", "LL",
            "Leaf.T","Fruit.T","SStem.T", "PStem.T", "Tu.Rt",
            "Tuber", "Total",
            "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT", 
            "AB", "SR", "LR", "FV", "FL"
  )) 

# Rename key traits
vbif_subset<-  rename(vbif_subset, c(SH_B = SH.1, PH_B = PH.1,
                                     leaves = Leaf.T, fruits = Fruit.T,
                                     Tbiom = Total))

# Calculate some missing traits
# Root only biomass, from Tu+RT - Tuber biomass
vbif_subset$roots <- vbif_subset$Tu.Rt - vbif_subset$Tuber
# Stem biomass, SStem-T - Leaf.T (now leaves)
vbif_subset$stems <- vbif_subset$SStem.T - vbif_subset$leaves
# Add species column
vbif_subset$species <- rep("Vio.bif",nrow(vbif_subset))


# vpal: keep biomass only related traits
vpal_subset <- vpal %>% 
  select(c( "siteID","IDR", "IDGsite", "IDG","SH.1", "PH.1", "LL",
            "Leaf.T","Fruit.T","SStem.T", "PStem.T", "Tu.Rt",
            "Tuber", "St.Rt","Stolon","Total", 
            "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT", 
            "AB", "SR", "LR", "FV", "FL"
  )) 

# Rename key traits
vpal_subset<-  rename(vpal_subset, c(SH_B = SH.1, PH_B = PH.1,
                                     leaves = Leaf.T, fruits = Fruit.T,
                                     Tbiom = Total))

# Calculate some missing traits
# Root only biomass, from Tu+RT - Tuber biomass
vpal_subset$roots <- vpal_subset$Tu.Rt - vpal_subset$Tuber
# Stem biomass, SStem-T - Leaf.T (now leaves)
vpal_subset$stems <- vpal_subset$SStem.T - vpal_subset$leaves
# Add species column
vpal_subset$species <- rep("Vio.pal",nrow(vpal_subset))

# Create a combined Viola dataframe
# THese have some different characteristics to the others
Viola_Combined <- full_join(vbif_subset, vpal_subset)

# Pivot longer and drop na's
Viola_Combined_long <- Viola_Combined %>% 
  pivot_longer(cols = c(SH_B:stems, St.Rt:Stolon),
               names_to = "Biomass_Attributes", values_to = "Values",
               values_drop_na = TRUE)


#################################
# Veronica species
#################################
# voff: keep biomass only related traits
voff_subset <- voff %>% 
  select(c( "siteID", "IDR", "IDGsite", "IDG", "IDS","MS",
            "rhiz.", "roots", "stems", "leaves", "GG.1", "fruits",
            "TbiomR", "Tbiom", 
            "ag", "agr", "bg", "r", "agbg", "agrbg",
            "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT", 
            "AB", "SR", "LR", "FV", "FL"))

voff_subset$GG.1 <- gsub(",", ".", voff_subset$GG.1)
voff_subset$GG.1 <- as.numeric(as.character(voff_subset$GG.1))

# Add species column
voff_subset$species <- rep("Ver.off",nrow(voff_subset))

# valp: keep biomass only related traits
valp_subset <- valp %>% 
  select(c( "siteID", "IDR", "IDGsite", "IDG", "IDS","MS",
            "rhiz.", "roots", "stems", "leaves", "GG.1", "fruits",
            "TbiomR", "Tbiom", 
            "ag", "agr", "bg", "r", "agbg", "agrbg",
            "RBM", "SBM", "LBM", "FBM", "BM", "BMTOT", 
            "AB", "SR", "LR", "FV", "FL"
  )) 
# Add species column
valp_subset$species <- rep("Ver.alp",nrow(valp_subset))

valp_subset$GG.1 <- gsub(",", ".", valp_subset$GG.1)
valp_subset$GG.1 <- as.numeric(as.character(valp_subset$GG.1))

# Create a combined Veronica dataframe
# THese have some different characteristics to the others
Veronica_Combined <- full_join(voff_subset, valp_subset)


# Pivot longer and drop na's
Veronica_Combined_long <- Veronica_Combined %>% 
  pivot_longer(cols = rhiz.:FL,
               names_to = "Biomass_Attributes", values_to = "Values",
               values_drop_na = TRUE)

#############################
# Combine all 6 species
#############################

Veronica_Viola_long <- full_join(Veronica_Combined_long, Viola_Combined_long)
Biomass_Combined_long <- full_join(Veronica_Viola_long, Carex_Combined_long)


#############################
# Data file with variables, 2 col, data name, description, and raw/aggregate  
# R script
# For example, then left join and it'll give a new column called data_type
Biomass_Data_Type <- read.table(header = TRUE,
                                stringsAsFactors = FALSE,
                                text = "Old_Biomass_Attr Biomass_Attributes Data_Type
                                  SH_B Shoot_Height_B Raw
                                  PH_B Flowering_Shoot_Stem_B Raw
                                  PH0stem Flowering_Shoot_B Raw
                                  LL Largest_Leaf Raw
                                  SStem.T Shoots_w_Leaves Raw
                                  PStemT Shoots_w_Flowers Raw
                                  Tu.Rt Tuber_w_Roots Raw
                                  Tuber Tuber Raw
                                  St.Rt Stolen_w_Roots Raw
                                  Stolon Stolon Raw
                                  rhiz. Rhizome Raw
                                  roots Roots Raw
                                  stems Stems Raw
                                  leaves Leaves Raw
                                  fruits Fruits Raw
                                  TbiomR Total_Biomass_Ramet Raw
                                  Tbiom Total_Biomass_Genet Raw
                                  GG.1 Generative_Growth Raw
                                  ag Above_Ground Raw
                                  agr Above_Ground_Repro Raw
                                  bg Below_Ground Raw
                                  r Reproductive Raw
                                  agbg Above_w_Below_Ground Raw
                                  agrbg Above_w_Below_Ground_Repro Raw
                                  RBM Root_w_Rhizome_mg_ramet Calculated
                                  SBM Stem_mg_ramet Calculated
                                  LBM Leaf_mg_ramet Calculated
                                  FBM Reproductive_mg_ramet Calculated
                                  BM Total_Sterile_B_mg_ramet Calculated
                                  BMTOT Total_Biomass_mg_ramet Calculated
                                  AB Ratio_AB_BG_ramet Calculated
                                  SR Ratio_Stem_BG_ramet Calculated
                                  LR Ratio_Leaf_BG_ramet Calculated
                                  FV Ratio_Fertile_Sterile_ramet Calculated
                                  FL Ratio_Fertile_Photosynthetic_ramet Calculated
                                  BioDL Biomass_Dead_Leaves Raw
                                  BioSS Biomass_Sterile_Stems Raw
                                  BMseeds Biomass_Seeds Calculated
                                  "
)


# Then once all species look the same then...
# add tables
Biomass_Combined_long_cleaned <- left_join(Biomass_Combined_long,
                                           Biomass_Data_Type,
                                           by = c("Biomass_Attributes" = "Old_Biomass_Attr")) 

# Remove cols and rename
Biomass_Combined_long_cleaned <- Biomass_Combined_long_cleaned %>% 
  rename("Biomass_Attributes_Old" = "Biomass_Attributes") %>% 
  rename("Biomass_Attributes" = "Biomass_Attributes.y")

# Add year columns
# July, 2009
Biomass_Combined_long_cleaned$year <- 2009
Biomass_Combined_long_cleaned$month <- "July"

Biomass_Combined_long_cleaned <- Biomass_Combined_long_cleaned |> 
  mutate(siteID = recode(siteID, 
                              'Skjellingahaugen' = "Skjelingahaugen",
                              'Vikafjell' = "Skjelingahaugen",
                              'Ovstedal' = "Ovstedalen"))


# Tidy up order of columns
SG8_Biomass <- Biomass_Combined_long_cleaned %>% 
  select(year, month, siteID, genet = IDG, ramet = IDS, mother_shoot = MS, individualID = IND, 
         species, biomass_attributes = Biomass_Attributes, value = Values, data_type = Data_Type, 
         biomass_attributes_old = Biomass_Attributes_Old)

# Save file
write_csv(SG8_Biomass,
           file = "biomass/SG8_clean_biomass_2009.csv")
