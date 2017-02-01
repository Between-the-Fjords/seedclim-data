######## PCR plotting ##########
source("/inst/graminoidRemovals/loadData.R")
source("/Users/fja062/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/multiplot_function.R")
library(ggvegan)
##### SITE BY SITE BASIS #######

ulv <- my.GR.data %>%
  filter(functionalgroup != "graminoid")  #%>%  & specialism == "alpine" to filter by specialism

scores <- numeric(0)
plots <- list()
siteID <- unique(ulv$siteID)
#temp <- unique(ulv$temp)
#prec <- unique(ulv$prec)

for (i in siteID) {
  df <- filter(ulv, siteID == i) %>%
    mutate(turfID = as.factor(turfID))
  
  spp <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = df)
  spp <- as.data.frame(unclass(spp))
  spp <- spp[,colSums(spp > 0) > 0] #remove empty spp
  
  ulv_env <- distinct(df, ID, .keep_all = TRUE)
  
  year <- ulv_env$Year
  treat <- ulv_env$TTtreat
  
  prc1 <- prc(response = spp, treat, year)
  abundance <- as.data.frame(colSums(spp)) # to select spp above k abundance
  
  scores <- c(scores, prc1$CCA$biplot[,2])
  #par(mfrow = c(3,4))
  p1 <- plot(prc1, select = abundance > 10)#autoplot(prc1, leg.pos = "", label = spp) + labs(title = i)
  plots[[i]] <- p1 #add each plot to the empty list

}

multiplot(plotlist = plots, cols = 3)  

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
  scale_color_brewer(palette = "Dark2")



rda1 <- rda(spp ~ TTtreat * Year, data = ulv_env)
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

