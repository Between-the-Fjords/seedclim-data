#########################################################################
#Description: to be written at some point?







#########################################################################

####FUNCTIONS

#A function that finds and replaces a string (using regex) in a dataframe
library(stringr)

replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}

#A function that takes a dataframe, and a list of problems and resolutions, and runs them through replace_all() 
probfixes=function(df, old, new){
  for(i in 1:length(old)){
    df=replace_all(df,old[i],new[i])
  }
  return(df)
}

#a function if we want to look only (up to) the top 5 maximum values (e.g. for max potential height)
top5=function(df){
  list=unique(df$binomial)
  df=df[order(df$binomial, -df$vals),]
  df2=df[FALSE,]
  for (i in 1:length(list)){
    df3=df[df$binomial==list[i],][1:5,]
    df2=rbind(df2,df3[!is.na(df3$vals),])
  }
  return(df2)
}

#A function that returns the number of replicates, means, and SE for (x=factors, y=data)
means<-function(x, y){
  
  #calculate mean, SD, number of replicates, SE.
  xx<-tapply(y,x,mean)
  yy<-tapply(y,x,sd)
  zz<-tapply(y,x,length)
  er<-yy/sqrt(zz)
  return(data.frame(binomial=rownames(xx), N=zz, mean=xx, SE=er, row.names=NULL))
  
}

#a function to clean up raw trait data and calculate means

cleanup=function(df,top5){

  #remove empty entries
  df = df[!is.na(df$binomial),][df$vals!=0,]

  #correct naming inconsistencies
  df=probfixes(df,as.character(probs$old),as.character(probs$new))
  
  #if we want to use only the top 5 values...
  if(top5==TRUE){
    df=top5(df)  
  }
  
  #calculate species means
  df.means = means(df$binomial, df$vals)
  df = as.data.frame(df)
}

#a function to merge two dataframes and report any inconsistencies
merge.them=function(my.df, LEDA.df, trait.name){
  trait.all=merge(splist, LEDA.df, by = "binomial", all.x=TRUE)
  trait.all=merge(trait.all, my.df, by="binomial", all.x=TRUE, all.y=TRUE)
  colnames(trait.all)=c("binomial", "speciescode",paste("LEDA",trait.name,"N",sep="."),paste("LEDA",trait.name,"mean",sep='.'),paste("LEDA",trait.name,"SE",sep='.'),paste("my",trait.name,"N",sep="."),paste("my",trait.name,"mean",sep='.'),paste("my",trait.name,"SE",sep='.'))
  return(trait.all)
  
  #QC
  print("Species in my trait data that aren't in the seedclim list (there should be none):")
  spp=data.frame(spp=unique(my.df$binomial[!my.df$binomial %in% splist$binomial]))
  print(spp)
  print("Species in the seedclim list that don't have entries in LEDA (there may be many):")
  spp=data.frame(spp=unique(splist$binomial[!splist$binomial %in% LEDA.df$binomial]))
  print(spp)
  
}

#a plotting function to compare the trait values for species shared from two sources (e.g. my data and LEDA data)
comparison.plot=function(my.dats, other.dats, labs){
  lims=c(0,max(my.dats*1.05, other.dats*1.05, na.rm=TRUE))
  plot(other.dats, my.dats, xlim=lims, ylim=lims, xlab=labs[1], ylab=labs[2])
  abline(a=0, b=1)
}

#a function for error bars (should i need them)
  error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
      stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  }


#########################################################################

##### MAKING THE SEEDCLIM SPECIES LIST

#Read in seedclim data
splist <-  my.GR.data %>%
    select(binomal = species) %>%
    distinct(binomal) %>%
    filter(binomal != "Caru.car") %>%
    filter(binomal != "Pic.abi")
  

#Rename incorrect species in SEEDCLIM list (maybe not a great idea in the long run -- these should be changed in the database, not here)
splist=replace_all(splist,"Comastoma tenellum", "Gentianella tenella")
splist=replace_all(splist,"Arctous alpinus", "Arctostaphylos alpinus")

#eliminate any sp./sp inconsistencies
splist$binomial=gsub("\\ssp\\.$"," sp",splist$binomial)  



#########################################################################

##SPECIES FIXES

#list of species names that haven't aligned well with the SEEDclim List, and their resolutions
probs=as.data.frame(matrix(c(
  "Anemona nemorosa","Anemone nemorosa",
  "Deschampsia flexuosa","Avenella flexuosa",
  "Empetrum nigrum","Empetrum hermaphroditum",
  "Euphrasia frigida","Euphrasia sp",
  "Euphrasia stricta","Euphrasia sp",
  "Noccia sp","Noccaea caerulescens",
  "Polygonum viviparum","Bistorta vivipara",
  "Pyrola norvegica","Pyrola rotundifolia",
  "Sagina saginoides","Sagina sp",
  "Taraxacum","Taraxacum sp",
  "Carex ovina","Carex leporina",
  "Hypochoeris radicata","Hypochaeris radicata",
  "Thlaspi caerulescens","Noccaea caerulescens",
  "Saxifraga media", "Saxifraga aizoides"#,
  #"Anthoxanthum odoratum","Anthoxanthum alpinum" #Dunno if i should change this one -- LEDA leaf size only exists for A. alpinum
), byrow=TRUE, ncol=2, dimnames=list(c(),c("old","new"))))



#########################################################################

#############TRAIT ANALYSES

##########SLA

#Read in my raw SLA data + rename cols
my.sla = read.csv("cleaning_code/5_trait_data/data/RawTraitData_SLA.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
my.sla <- my.sla %>%
  select(binomial = Species, vals = SLA..m2.kg.1.) %>%
  separate(binomial, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(binomial, genus, species, sep = ".") %>%
  as.data.frame()
  
#Read in LEDA SLA data + remove dubious records + rename cols
LEDA.sla = read.csv("cleaning_code/5_trait_data/data/LEDA_SLA_RawData.csv", stringsAsFactors = FALSE)
LEDA.sla = LEDA.sla[LEDA.sla$general.method!="laboratory/greenhouse/garden experiment",]
LEDA.sla = LEDA.sla %>%
  select(binomial = SBS.name, vals = Single.Value..LEDA.) %>%
  separate(binomial, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(binomial, genus, species, sep = ".") %>%
  
  as.data.frame()


#cleanup and means
my.sla = cleanup(my.sla, top5 = FALSE)
LEDA.sla = cleanup(LEDA.sla, top5 = FALSE)

#merge them with seedclim names
sla.all <- full_join(my.sla, LEDA.sla, by = "binomial")
  
sla.all <- left_join(splist, sla.all, by = "binomial")

sla.all=merge.them(my.sla, LEDA.sla, trait.name="sla")

#plot compare LEDA and my sla values for the same species... Do i need any adjustments?
x11();
comparison.plot(sla.all$my.sla.mean, sla.all$LEDA.sla.mean, labs=c("LEDA mean SLA","My mean SLA"))



##########LEAFSIZE

#Read in my raw leafsize data + rename cols
my.leafsize = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/RawTraitData_leafsize.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
my.leafsize = data.frame(binomial=my.leafsize$Species, vals=my.leafsize$LeafSize)

#Read in raw LEDA leafsize data + rename cols
LEDA.leafsize = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/LEDA_leafSize_RawData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
LEDA.leafsize = LEDA.leafsize[LEDA.leafsize$general.method!="laboratory/greenhouse/garden experiment",]
LEDA.leafsize = data.frame(binomial=LEDA.leafsize$SBS.name, vals=LEDA.leafsize$single.value..mm.2.)

##adjust units, and then fix any stupid values that are too big -- 50cm in this case 
my.leafsize$vals=my.leafsize$vals*100
LEDA.leafsize=LEDA.leafsize[LEDA.leafsize$vals<5000,]

#cleanup and find means
my.leafsize = cleanup(my.leafsize, top5=FALSE)
LEDA.leafsize = cleanup(LEDA.leafsize, top5=FALSE)

#merge them with seedclim names
leafsize.all=merge.them(my.leafsize, LEDA.leafsize, trait.name="leafsize")

#plot to compare LEDA and my values for the same species... Do i need any adjustments?
x11();
comparison.plot(leafsize.all$my.leafsize.mean, leafsize.all$LEDA.leafsize.mean, labs=c("LEDA mean leafsize","My mean leafsize"))



#############HEIGHT

#Read in my raw height data + rename cols
my.height = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/RawTraitData_height.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
my.height = data.frame(binomial=my.height$SPECIES,vals=my.height$HEIGHT)

#Read in raw height leafsize data + rename cols
LEDA.height = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/LEDA_Height_RawData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
LEDA.height = data.frame(binomial=LEDA.height$SBS.name, vals=LEDA.height$single.value..m.)

#adjust my units, and then fix any stupid values that are too big -- 2m in this case
my.height$vals=my.height$vals/100
my.height = my.height[my.height$vals<1,]
LEDA.height = LEDA.height[LEDA.height$vals<1,]

#cleanup and find means
my.height = cleanup(my.height, top5=TRUE)
LEDA.height = cleanup(LEDA.height, top5=TRUE)

#merge them with seedclim names
height.all=merge.them(my.height, LEDA.height, trait.name="max.height")

#plot to compare LEDA and my values for the same species... Do i need any adjustments?
x11();
comparison.plot(height.all$my.max.height.mean, height.all$LEDA.max.height.mean, labs=c("LEDA mean max. height","My mean max height"))



#############LEAF CN

#Read in raw leafCN data
my.leafCN = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/RawTraitData_leafCN.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

#find binomials from seedclim list + condense/rename
my.leafCN = data.frame(binomial=splist$binomial[match(my.leafCN$Species, splist$speciescode)],vals=my.leafCN$CN_Ratio)

#calculate species averages
my.leafCN = means(my.leafCN$binomial, my.leafCN$vals) 
my.leafCN = my.leafCN[order(my.leafCN$mean),]

#rename columns(it is important later)
colnames(my.leafCN)=c("binomial","my.leafCN.N","my.leafCN.mean","my.leafCN.SE")

#set old par, since i have to pess with par; define new par
x11();
old.par <- par(mar = c(0, 0, 0, 0))
par(mar=c(11,5,2,2))

meanz=barplot(my.leafCN$my.leafCN.mean, ylim=c(0,max(my.leafCN$my.leafCN.mean)*1.2), xaxt="n", ylab="My mean leaf C:N")
error.bar(meanz, as.numeric(my.leafCN$my.leafCN.mean), as.numeric(my.leafCN$my.leafCN.SE))
labs <- as.character(my.leafCN$binomial)
text(cex=1, x=meanz, y=-1, labs, xpd=TRUE, srt=60, adj=1)

#reset old par
par(old.par)




#############SEED MASS
#Read in raw seedmass data + rename cols
LEDA.seedmass = read.table("C:/Users/John/Documents/michigan/research/Norway/data and analysis/LEDA_SeedMass_RawData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
LEDA.seedmass = data.frame(binomial=LEDA.seedmass$SBS.name, vals=LEDA.seedmass$single.value..mg.)

#filter out only those species in seedclim list
LEDA.seedmass=merge(splist, LEDA.seedmass, by="binomial", all.x=TRUE)

#remove outliers
LEDA.seedmass = LEDA.seedmass[LEDA.seedmass$vals<50,]

#calculate means
LEDA.seedmass = means(LEDA.seedmass$binomial, LEDA.seedmass$vals)
LEDA.seedmass = LEDA.seedmass[order(-LEDA.seedmass$mean),]

#rename columns(it is important later)
colnames(LEDA.seedmass)=c("binomial","LEDA.seedmass.N","LEDA.seedmass.mean","LEDA.seedmass.SE")

#plot
x11();
meanz=barplot(LEDA.seedmass$LEDA.seedmass.mean, xaxt="n", ylab="LEDA seedmass", xlab="SEEDclim Species", las=1, names=0.5)



####Putting it all together (merging, removing duplicate columns, reorganizing columns)

all.traits=merge(sla.all,leafsize.all, by="binomial", all.x=TRUE, all.y=TRUE)
all.traits=merge(all.traits,height.all, by="binomial", all.x=TRUE, all.y=TRUE)
all.traits=merge(all.traits,my.leafCN, by="binomial", all.x=TRUE, all.y=TRUE)
all.traits=merge(all.traits,LEDA.seedmass, by="binomial", all.x=TRUE, all.y=TRUE)
all.traits=all.traits[,!colnames(all.traits) %in% c("speciescode.x","speciescode.y")]
all.traits=data.frame(binomial=all.traits$binomial, speciescode=all.traits$speciescode, all.traits[,!colnames(all.traits) %in% c("binomial","speciescode")])

#output file
#write.csv(all.traits, row.names=FALSE, file="C:/Users/John/Documents/michigan/research/Norway/data and analysis/traits_means2014.csv")
#write.csv(all.traits,row.names=FALSE, file = file.choose(new = TRUE))
