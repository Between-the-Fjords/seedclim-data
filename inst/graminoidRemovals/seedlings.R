################## SEEDLING DATA #######################
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")

#need to adjust the SQL query
seedlingGR <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, 
plots.plotID, turfs.turfID,turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, 
subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN subTurfCommunity ON turfs.turfID = subTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
HAVING subTurfCommunity.Year=2013 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"
))


con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")
seedEnvironment <- tbl(con, "subTurfEnvironment") %>% 
  collect() %>% 
  filter(year == 2013) %>%
  replace_na(list(acro = 0, pleuro = 0, soil = 0)) %>%
  mutate(bryophytes = acro + pleuro)

seedlingGR <- seedlingGR %>% 
  left_join(seedEnvironment)

levels(seedlingGR$TTtreat) <- c(levels(seedlingGR$TTtreat), levels(seedlingGR$GRtreat))
seedlingGR$TTtreat[seedlingGR$TTtreat == ""|is.na(seedlingGR$TTtreat)] <- seedlingGR$GRtreat[seedlingGR$TTtreat == ""|is.na(seedlingGR$TTtreat)]
seedlingGR$GRtreat <- NULL
seedlingGR$TTtreat <- factor(seedlingGR$TTtreat)
seedlingGR <- seedlingGR[!seedlingGR$blockID%in%remsites,]



seedlingGR <- seedlingGR %>%
  filter(!bad == "x") %>% 
  group_by(turfID) %>% 
  mutate(seedlings = sum(seedlings), juvenile = sum(juvenile), soil = mean(soil), bryophytes = mean(bryophytes), litter = mean(litter), rock = mean(rock)) %>%
  distinct(turfID, .keep_all = TRUE) %>% 
  select(-c(species, subTurf, TTtreat, fertile, dominant, Temperature_level, Precipitation_level, acro, pleuro))

seedlingGR <- wholecom %>% 
  select(siteID, turfID, Year, sumcover, TTtreat, Temperature_level, Precipitation_level, blockID) %>% 
  filter(Year == 2013) %>% 
  distinct(turfID, .keep_all = TRUE) %>% 
  left_join(seedlingGR)

seedlingGR %>% gather(wmean_trait, measurement, c(sumcover, seedlings, juvenile)) %>% 
  unite(wmean_trait_treatment, wmean_trait, TTtreat) %>% 
  #spread(wmean_trait_treatment, measurement) %>% 
  ggplot(aes(x = wmean_trait_treatment[wmean_trait_treatment == "sumcover_TTC"], y = Temperature_level))

seedlingdelta <- sapply(1:nrow(seedlingGR[seedlingGR$TTtreat == "RTC",]), function(i){
  R <- seedlingGR[seedlingGR$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("seedlings", "juvenile", "bryophytes", "soil")
  friend <- seedlingGR$blockID == R$blockID & seedlingGR$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- seedlingGR[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
seedlingdelta <- as.data.frame(t(seedlingdelta))
colnames(seedlingdelta) <- paste0("delta", colnames(seedlingdelta))
seedlingGR <- cbind((seedlingGR[seedlingGR$TTtreat == "RTC",]), seedlingdelta)

seedlingGR <- wholecom %>% filter(Year == 2013, TTtreat == "TTC") %>%
  select(funYear, sumcover, blockID) %>% 
  group_by(blockID) %>% 
  mutate(TTCsumcover = sum(sumcover)) %>% 
  distinct(blockID, .keep_all = TRUE) %>% 
  select(-funYear, -sumcover) %>% 
  full_join(seedlingGR, by = "blockID")

ggplot(seedlingGR, aes(x = Precipitation_level, y = deltaseedlings)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Temperature_level) +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  axis.dim

ggplot(seedlingGR, aes(x = deltabryophytes, y = deltaseedlings)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "gam") +
  facet_wrap(~Temperature_level) +
  theme_classic() +
  axis.dim
  ggsave(filename = "seedling_cover.pdf", height = 5, width = 5, dpi = 300)

  
ggplot(seedlingGR, aes(x = Precipitation_level, y = deltabryophytes)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Temperature_level)
    
mod <- lmer(deltaseedlings ~ deltabryophytes + Temperature_level + Precipitation_level + deltabryophytes:Precipitation_level + deltabryophytes:Temperature_level + (1|siteID), data = seedlingGR, na.action = "na.fail", REML = FALSE, family = poisson)

#analysis
library(lme4)
mod <- glmer(seedlings ~ Precipitation_level + TTtreat + TTtreat:Precipitation_level + (1|siteID), data = seedlingGR, REML = FALSE, family = poisson)

summary(mod)

mod <- glmer(deltaseedlings ~ Temperature_level + Precipitation_level + Precipitation_level:Temperature_level + (1|siteID), data = seedlingGR, na.action = "na.fail", REML = FALSE, family = poisson)
mod
drop1(mod, test = "Chisq")

output.dsc <- model.average(mod, percent.thresh = 0.95, print = TRUE)
plot.estimates(output.dsc, title = "Sum of cover")

Estimate <- fixef(mod)
se <- sqrt(diag(vcov(mod)))
Estimates <- as.data.frame(cbind(Estimate, se)) %>%
  rownames_to_column(var = "explan.var") %>%
  mutate(CI.upper = Estimate + (se * 1.96), CI.lower = Estimate - (se * 1.96))
plot.estimates(Estimates, title = "estimates")

vif.mer(model.dsc.ba)

preds <- predict(avg.mod)
