######## PCR plotting ##########
source("/inst/graminoidRemovals/loadData.R")
source("/Users/fja062/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/multiplot_function.R")
library(ggvegan)
##### SITE BY SITE BASIS #######

ulv <- my.GR.data %>%
  filter(functionalGroup != "graminoid")  %>%
  filter(!is.na(siteID)) %>% 
  #filter(siteID %in% c("Ulvhaugen", "Fauske", "Skjellingahaugen", "Ovstedal")) %>% 
  ungroup()#%>%  & specialism == "alpine" to filter by specialism

scores <- numeric(0)
plots <- list()
siteID <- unique(ulv$siteID)
#temp <- unique(ulv$temp)
#prec <- unique(ulv$prec)

for (i in siteID) {
  df <- filter(ulv, siteID == i) %>%
    mutate(turfID = as.factor(turfID))
  
  spp <- xtabs(cover ~ ID + species, data = df)
  spp <- as.data.frame(unclass(spp))
  spp <- spp[,colSums(spp > 0) > 0] #remove empty spp
  
  ulv_env <- distinct(df, ID, .keep_all = TRUE)
  
  year <- as.factor(ulv_env$Year)
  treat <- ulv_env$TTtreat
  
  prc1 <- prc(response = spp, treat, year)
  abundance <- as.data.frame(colSums(spp)) # to select spp above k abundance
  
  scores <- c(scores, prc1$CCA$biplot[,2])
  #
  p1 <- plot(prc1, main = i, abundance >4)#autoplot(prc1, leg.pos = "", label = spp) + labs(title = i)
  plots[[i]] <- p1 #add each plot to the empty list

}

gr <- gridExtra::arrangeGrob(grobs = plots)
plot(gr)

w <- comp2 %>% mutate(turfID = factor(turfID)) %>% filter(Year == 2017) %>% group_by(siteID) %>% 
  do({
    spp <- xtabs(cover ~ turfID + species, data = .)
    spp <- as.data.frame(unclass(spp))
    spp <- spp[,colSums(spp > 0) > 0]
    
    ulv_env <- distinct(., ID, .keep_all = TRUE)
    treat <- as.factor(ulv_env$TTtreat)
    
    prc1 <- prc(response = spp, treat)
    abundance <- as.data.frame(colSums(spp)) # to select spp above k abundance
    
  })


w$`c.scores..prc1.CCA.biplot...2..`[is.na(w$`c.scores..prc1.CCA.biplot...2..`)] <- w$`c(scores, prc1$CCA$biplot[, 2])`[is.na(w$`c.scores..prc1.CCA.biplot...2..`)] # merge the GRtreat and TTtreat into one column
w$`c(scores, prc1$CCA$biplot[, 2])` <- NULL
w <- w %>% 
  gather(key = Year, value = prc, c(3:ncol(w))) %>% 
  mutate(Year = as.numeric(substr(Year, 6, 9)))


plot(w)
autoplot.prc(w)

multiplot(plotlist = plots, cols = 3)  


spec <- my.GR.data %>% ungroup() %>% filter(TTtreat == "RTC", functionalGroup == "forb") %>%  group_by(Year, siteID, species) %>% mutate(meanCov = mean(cover))

spec %>% group_by(species, siteID) %>% filter(max(meanCov) > 6) %>% ungroup() %>%
  ggplot(aes(x = Year, y = meanCov, colour = species)) +
  geom_line() +
  facet_wrap(Temperature_level~Precipitation_level, scales = "free_y") +
  scale_color_manual(values = if_else("SLA_mean" > 200, colorRampPalette(brewer.pal(n = 12, name = "Paired")), if_else("SLA_mean" < 100, "grey90", colorRampPalette(brewer.pal(n = 12, name = "Paired")))))



`autoplot.prc` <- function(object, select, xlab, ylab,
                           title = NULL, subtitle = NULL, caption = NULL,
                           legend.position = "top", ...) {
  ## fortify the model object
  fobj <- fortify(object, ...)
  ## levels of factors - do this now before we convert things
  TimeLevs <- levels(fobj$Time)
  TreatLevs <- levels(fobj$Treatment)
  ## convert Time to a numeric
  fobj$Time <- as.numeric(as.character(fobj$Time))
  ## process select
  ind <- fobj$Score != "Sample"
  if(missing(select)) {
    select <- rep(TRUE,sum(ind))
  } else {
    stopifnot(isTRUE(all.equal(length(select), sum(ind))))
  }
  ## samples and species "scores"
  samp <- fobj[!ind, ]
  spp <- fobj[ind,][select, ]
  ## base plot
  plt <- ggplot(data = samp,
                aes_string(x = 'Time', y = 'Response', group = 'Treatment', 
                           colour = 'Treatment', linetype = 'Treatment'))
  ## add the control
  plt <- plt + geom_hline(yintercept = 0)
  ## add species rug
  plt <- plt +
    geom_rug(data = spp,
             sides = "r",
             mapping = aes_string(group = NULL, x = NULL,      
                                  colour = NULL, linetype = NULL))
  ## add the coefficients
  plt <- plt + geom_line() +
    theme(legend.position = legend.position) +
    scale_x_continuous(breaks = as.numeric(TimeLevs), minor_breaks = NULL)
  
  ## add labels
  if(missing(xlab)) {
    xlab <- 'Time'
  }
  if(missing(ylab)) {
    ylab <- 'Treatment'
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                    caption = caption)
  ## return
  plt
  
}
#alrust = lotus cor
#arhelleren = potentilla ere
#fauske = alchemilla sp
#gudmedalen = viola bif
#hogsete = achillea mil
#lavisdalen = silene aca
#ovstedal = potentilla ere
#rambera = potentilla ere
#skjellingahaugen = silene aca
#ulvhaugen = achillea mil
#veskre = hieracium pil
#vikesland = galium ver

scores <- as.data.frame(cbind(scores,
      rep(c(2011,2012,2013,2015,2016),3),
      rep(siteID,each=5)
))

colnames(scores) <- c('scores', 'year', 'sites')
rownames(scores) <- NULL
scores$scores <- as.numeric(paste(scores$scores))
scores$year <- as.numeric(paste(scores$year))
#scores$temp <- as.factor(paste(scores$temp))
#scores$prec <- as.factor(paste(scores$prec))

# prc plot all sites forbs
ggplot(scores, aes(x = year, y = scores, colour = sites)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values = cbPalette)



rda1 <- rda(cover ~ TTtreat * Year, data = comp2)
coef1 <- coef(summary(rda1, axis = 1, scaling = "species", digits = 4))
  
ctrl <- how(within = Within(type = "series"), #, constant = TRUE <- is this necessary?
            plots = Plots(strata = ulv_env$blockID),
            blocks = ulv_env$siteID)
anova(prc1, permutations = ctrl)
  


rda1 <- rda(spp ~ TTtreat * Year, data = ulv_env)
coefs <- as.data.frame(coef(rda1))
  


out <- NULL
for (i in levels(year)) {
  take_spec <- spp[year == i, ]
  take_dose <- treatment[year == i]
  out[[i]] <- anova(rda(take_spec ~ take_dose), by = "terms", step = 1000)
}
sapply(out, function(x) x[1, 5])



#Coefficients for explan$TTtreat + explan$Year:explan$TTtreat interaction
#which are contrasts to explan$TTtreat TTC 
#rows are explan$TTtreat, columns are explan$Year
#      2011    2012 2013  2015
#RTC -2.203 -0.5149 10.7 7.943


rdafunction<-function(site, dat=species, ord=prc, ...){
  
  keep<-explan$siteID==site
  
  TT<-explan$TTtreat[keep]
  year<-explan$Year[keep]
  
  dat<-dat[keep,]
  #browser()  
  #run ord
  mod<-ord(dat, ...)
  plot(mod)
}

sapply(levels(explan$siteID), function(siteID){
  par(mfrow=c(1,1))
  rdafunction(site, dat=species, ord=prc)
  title(main=siteID)
})




#Plotting
plot(prc1, axis=1, cex.axis=1.5, cex.lab=1.5, type="b", pch=19, lwd=2, lty=c(1,5,1,5,1,5,1), legpos=NA, xlab="Year", ylab="PRC1", cex=1.2) #  select=abundance>25, col=c("black","red","red", "green","green","blue","blue")
legend("topleft", inset=.05, c("Control","Warming", "Nutrient addition","Warming + Nutrient addition", "", "Solid line = herbivore exclosure", "Dashed line = herbivores present"), pch=19, col=c(1, 2, 3, 4, 0, 0,0),horiz=FALSE,bty="n",cex=1,pt.cex=c(1.2,1.2,1.2,1.2))

library(vegan)



#### FORBS ONLY ####
forbs <- my.GR.data %>%
  filter(functionalGroup != "graminoid") %>%
  filter(temp == 10.5) #%>%
  filter(TTtreat == "RTC")

forbcover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = forbcom)
forbcover <- as.data.frame(unclass(forbcover))
forbcover <- forbcover[,colSums(forbcover > 0) > 0] #remove empty spp


row.names(forbcom) <- forbcom$ID

env.var <- forbcom %>%
  filter(cover>0) %>% 
  arrange(ID) %>%
  #filter(TTtreat == "RTC") %>%
  #filter(Temperature_level == 10.5) %>%
  select(richness, diversity, evenness, functionalGroup, TTtreat, Year, Temperature_level, Precipitation_level, TTtreat)
  
x <- rda(forbcover ~  Year + TTtreat + Precipitation_level, data = env.var) # can't put both site and explan variables into analyses wmean_leafSize+ wmean_seedMass+ sumcover+ wmean_LDMC_global+ wmean_SLA_global+ wmean_CN_global+ wmean_LTH_global+ wmean_LA_global + wmean_height_global + 
plot(x, display = c("cn", "sites"))
autoplot(x)
ggsave(autoplot(x), filename = "forbcom_rda.jpg", height = 6, width = 6)


#Structural: Height,SM,LA
#growth economic: SLA,LDMC,CN

#### WHOLE COMMUNITY ####

all <- forbcom %>%
  filter(TTtreat == "RTC", Year == "2011")

env.var.all <- my.GR.data %>%
  arrange(ID) %>%
  filter(TTtreat == "RTC", Year == "2011") %>%
  select(wmean_height, wmean_leafSize, wmean_seedMass, wmean_maxheight, wmean_minheight, sumcover, wmean_LDMC_global, wmean_SLA_global, wmean_CN_global, wmean_LTH_global, wmean_LA_global, wmean_height_global, richness, diversity, evenness, functionalgroup, TTtreat, Year)


#############################
#### dissimilarity index ####

spp <- my.GR.data %>% 
  ungroup() %>% 
  filter(!functionalGroup == "graminoid", !is.na(cover), !is.na(Year)) %>% 
  mutate(cover = as.numeric(cover)) %>% 
  distinct(species, ID, .keep_all = TRUE) %>%
  select(species, ID, cover) %>% 
  spread(key = species, value = cover, fill = 0)

siteEnv <- my.GR.data %>% 
  ungroup() %>% 
  filter(!functionalGroup == "graminoid", !is.na(cover), !is.na(Year)) %>% 
  mutate(cover = as.numeric(cover)) %>% 
  distinct(species, ID, .keep_all = TRUE) %>% 
  select(siteID, turfID, temp, precip, TTtreat, Year) %>% 
  distinct(turfID, Year, .keep_all = TRUE)

rownames(spp) <- spp$ID
spp$ID <- NULL
spp[is.na(spp[,1:length(spp)])] <- 0
rda1 <- rda(spp, siteEnv)
plot(rda1)

distMat <- vegdist(spp, method = "jaccard")
distMat <- as.matrix(distMat)
distMat <- distMat %>%
  as.data.frame() %>%
  rownames_to_column("ID") %>% 
  separate(ID, into = c("turfID", "Year"), sep = "_") %>%
  filter(Year == 2011) %>% 
  gather(key = ID, value = dist, -turfID, -Year) %>% 
  separate(ID, into = c("turfIDcomp", "Yearcomp"), sep = "_") %>% 
  group_by(turfID) %>% 
  filter(turfIDcomp == turfID)

distmatPlot <- distMat %>% 
  full_join(siteEnv, by = "turfID") %>% 
  filter(!is.na(Yearcomp)) %>% 
  ggplot(aes(x = Yearcomp, y = dist, colour = TTtreat)) + 
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), size = 0.8) +
  #stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(temp~precip) +
  scale_colour_manual("", values = c("grey60", "Black")) +
  theme_classic() +
  axis.dimLarge +
  labs(x = "Year", y = "Dissimilarity") +
  theme(axis.text.x = element_text(angle = 25))

  
ggsave(distmatPlot, file = "~/OneDrive - University of Bergen/Research/FunCaB/figures/distmatPlot.jpg")

