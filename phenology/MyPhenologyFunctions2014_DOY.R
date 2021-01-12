#####################################
#### MY PHENOLOGY FUNCTIONS  DOY ####
#####################################

# FIND GOOD TAXA
# good.taxa function for community data (taxa that occur on this turf in 2009)
good.taxa <- function(tu){
  good.taxa <- unique(dat$species[dat$Year=="2009" & dat$turfID==tu]) #dat is community data
  return(good.taxa)
}


# GET RID OF INVADERS
get.rid.of.invaders <- function(pheno.var){
  good.species <- apply(pheno.var, 1, function(x){
    tID <- x["turfID"]
    print(paste(x["turfID"],  x["species"], sep=" "))
    if(tID %in% turfs$turfID){ # community turfs
      good.sp <- good.taxa(tID)
      spp <- x["species"]
      x[!spp %in% good.sp] <- NA
      x
    }
    else { # demography turfs
      good.sp <- good.taxa.dem(tID)
      spp <- x["species"]
      x[!spp %in% good.sp] <- NA
      x
    }
    x
  })
  good.species <- as.data.frame(t(good.species))
  good.species <- good.species[!is.na(good.species$turfID),]
  return(good.species)
}



#### PLOTTING ####


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#### FIGURES ####

#### FUNCITON TO RESHAPE FOR FIGURES ####
# Make separate columns for each treatment
# used for graphs
# data: pheno.long
# control.truf: destination or origin
ReshapeToMakeFigure <- function(dd, control.turf){
  if(control.turf == "destination"){
    dd <- dd[,c("destSiteID", "species", "pheno.stage", "pheno.var", "TTtreat", "value", "turfID")]
    dd2 <- dd %>%
      group_by(destSiteID, species, pheno.stage, pheno.var, TTtreat) %>%
      summarise(mean = mean(value)) %>%
      spread(key = TTtreat, value = mean)

  }
  else if(control.turf == "origin"){
    dd <- dd[,c("siteID", "species", "pheno.stage", "pheno.var", "TTtreat", "value", "turfID")]
    dd2 <- dd %>%
      group_by(siteID, species, pheno.stage, pheno.var, TTtreat) %>%
      summarise(mean = mean(value, na.rm = TRUE)) %>%
      spread(key = TTtreat, value = mean)
  }
  return(dd2)
}


#### MAKE PLOT ####
# data : pheno.treat
# control.truf: destination or origin
# p.var: peak, end, first, duration
# p.stage: flower, bud, seed
MakePlot <- function(dd, control.turf, p.var, p.stage){
  if(control.turf == "destination"){
    warm <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT2, color = factor(destP_level), shape = factor(destT_level), size = 1.5)) +
      #xlim(range(control, na.rm=TRUE)) +
      #ylim(range(cbind("TT2", "TT3", "TT4"), na.rm=TRUE)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("warm") + ylab("Transplant") + xlab("") +
      geom_text(aes(x = TTC, y = TT2, label=species),hjust=0, vjust=0)
    
    wet <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT3, color = factor(destP_level), shape = factor(destT_level), size = 1.5)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("lightblue","blue", "darkblue")) +
      theme(legend.position="none") +
      ggtitle("wet") + ylab("") + xlab("Control") +
      geom_text(aes(x = TTC, y = TT3, label=species),hjust=0, vjust=0)
    
    ww <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT4, color = factor(destP_level), shape = factor(destT_level), size = 1.5)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(name = "Precipitation", labels = c("dry", "intermediate", "wet"), values = c("lightblue","blue", "darkblue")) +
      scale_shape_manual(name = "Temperature", labels = c("alpine", "subalpine"), values = c(17,16)) +
      theme(legend.position=c(0.2,0.8)) +
      ggtitle("warm & wet") + ylab("") + xlab("") +
      geom_text(aes(x = TTC, y = TT4, label=species),hjust=0, vjust=0, show.legend = FALSE)
  }
  else if(control.turf == "origin"){
    warm <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT2, color = factor(Precipitation_level), shape = factor(Temperature_level), size = 1.5)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("warm") + ylab("Transplant") + xlab("") +
      geom_text(aes(x = TTC, y = TT2, label=species),hjust=0, vjust=0)
    
    wet <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT3, color = factor(Precipitation_level), shape = factor(Temperature_level), size = 1.5)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(values = c(17,16)) +
      theme(legend.position="none") +
      ggtitle("wet") + ylab("") + xlab("Control") +
      geom_text(aes(x = TTC, y = TT3, label=species),hjust=0, vjust=0)
    
    ww <- dd %>%
      filter(pheno.var == p.var) %>%
      filter(pheno.stage == p.stage) %>%
      ggplot() + 
      geom_point(aes(x = TTC, y = TT4, color = factor(Precipitation_level), shape = factor(Temperature_level), size = 1.5)) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed") +
      scale_colour_manual(name = "Precipitation", labels = c("very dry", "dry", "intermediate", "wet"), values = c("white","lightblue","blue", "darkblue")) +
      scale_shape_manual(name = "Temperature", labels = c("alpine", "subalpine"), values = c(17,16)) +
      theme(legend.position=c(0.2,0.8)) +
      ggtitle("warm & wet") + ylab("") + xlab("") +
      geom_text(aes(x = TTC, y = TT4, label=species),hjust=0, vjust=0, show.legend = FALSE)
  }
  
  multiplot(warm, wet, ww, cols=3)
}


#### ANALYSIS ####


### CHECK MODELS
fix.check <- function(mod){    #function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))  #should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}