###### weighted means for species present before and after ONLY
bfComm2016 <- my.GR.data %>%
  ungroup() %>%
  filter(Year== 2016) %>%
  select(species, turfID)

bfComm2011 <- my.GR.data %>%
  ungroup() %>%
  filter(Year== 2011) %>%
  select(species, turfID)

diffComm <- bfComm2016 %>% 
  inner_join(bfComm2011)

bfComm <- my.GR.data %>% 
  right_join(diffComm) %>% 
  group_by(ID, functionalGroup) %>%
  mutate(sumcover = sum(cover),
         wmeanLDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE),
         wmeanSLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE),
         wmeanLTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE),
         wmeanLA_local = weighted.mean(LA_mean, cover, na.rm = TRUE),
         wmeanheight_local = weighted.mean(Height_mean, cover, na.rm = TRUE),
         wmeanCN_local = weighted.mean(CN_mean, cover, na.rm = TRUE),
         wmeanseedMass_local = weighted.mean(seedMass, cover, na.rm = TRUE),
         cwvLDMC_local = wt.var(LDMC_mean, wt = cover),
         cwvSLA_local = wt.var(SLA_mean, wt = cover),
         cwvLTH_local = wt.var(Lth_mean, wt = cover),
         cwvLA_local = wt.var(LA_mean, wt = cover),
         cwvheight_local = wt.var(Height_mean, wt = cover),
         cwvCN_local = wt.var(CN_mean, wt = cover),
         cwvseedMass_local = wt.var(seedMass, wt = cover)) %>% 
  ungroup() %>%
  select(-(height:seedMass), -(Max_height:SLA), -recorder) %>%
  #filter(!(functionalgroup == "graminoid" & Year == 2012) & !(functionalgroup == "graminoid" & Year == 2013) & !(functionalgroup == "graminoid" & Year == 2015) & !(functionalgroup == "graminoid" & Year == 2016)) %>%
  distinct(ID, functionalGroup, .keep_all = TRUE) %>%
  as.data.frame()

bfComm$funYear <- as.factor(paste(bfComm$functionalGroup, bfComm$Year, sep = "_"))

bfComm <- bfComm %>%
  filter(functionalGroup == "forb") 

timedeltacalcbfComm <- sapply(1:nrow(bfComm[bfComm$Year != 2011,]), function(i){
  R <- bfComm[bfComm$Year != 2011,][i,]
  #browser()
  cols <- c("sumcover", "wmeanseedMass_local", "diversity", "richness", "evenness", "wmeanLDMC_local", "wmeanSLA_local", "wmeanLTH_local", "wmeanLA_local", "wmeanheight_local", "wmeanCN_local", "cwvLDMC_local", "cwvSLA_local", "cwvLTH_local", "cwvLA_local", "cwvheight_local", "cwvCN_local", "cwvseedMass_local")
  friend <- bfComm$turfID == R$turfID & bfComm$Year == 2011
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- bfComm[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
timedeltacalcbfComm <- as.data.frame(t(timedeltacalcbfComm))
colnames(timedeltacalcbfComm) <- paste0("delta", colnames(timedeltacalcbfComm))
timedeltabfComm <- cbind((bfComm[bfComm$Year != 2011,]), timedeltacalcbfComm)
timedeltabfComm <- filter(timedeltabfComm, deltasumcover > -80)


bfComm <- bfComm %>% 
  filter(!is.infinite(cwvSLA_local)) %>% 
  mutate(SannPrecip = as.numeric(scale(annPrecip)),
         Ssummer_temp = as.numeric(scale(summer_temp)),
         SYear = as.numeric(scale(Year)),
         wmeanLDMC_local = as.numeric(scale(wmeanLDMC_local)),
         wmeanseedMass_local = as.numeric(scale(wmeanseedMass_local)),
         wmeanCN_local = as.numeric(scale(wmeanCN_local)),
         wmeanheight_local = as.numeric(scale(wmeanheight_local)),
         wmeanSLA_local = as.numeric(scale(wmeanSLA_local)),
         wmeanLA_local = as.numeric(scale(wmeanLA_local)),
         wmeanLTH_local = as.numeric(scale(wmeanLTH_local)),
         sumcover = as.numeric(scale(sumcover)),
         evenness = as.numeric(scale(evenness)),
         richness = as.numeric(scale(richness)),
         cwvLDMC_local = as.numeric(scale(cwvLDMC_local)),
         cwvseedMass_local = as.numeric(scale(cwvseedMass_local)),
         cwvCN_local = as.numeric(scale(cwvCN_local)),
         cwvheight_local = as.numeric(scale(cwvheight_local)),
         cwvSLA_local = as.numeric(scale(cwvSLA_local)),
         cwvLA_local = as.numeric(scale(cwvLA_local)),
         cwvLTH_local = as.numeric(scale(cwvLTH_local)))


mod1tempBF <- bfComm %>% 
  gather(key = trait, value = measurement, c(richness, evenness, sumcover, wmeanLDMC_local:cwvseedMass_local)) %>% 
  filter(!is.na(measurement)) %>% 
  #filter(trait %in% c(tempTraits$trait)) %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ TTtreat*Ssummer_temp*SannPrecip*SYear - TTtreat:Ssummer_temp:SannPrecip:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  filter(term %in% c("TTtreatRTC","TTtreatRTC:Ssummer_temp:SYear", "TTtreatRTC:SannPrecip:SYear", "TTtreatRTC:SYear")) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

mod1tempBF <- mod1tempBF %>% 
  mutate(test = case_when(
    grepl("wmean", trait) ~ "Mean",
    grepl("cwv", trait) ~ "Variance",
    grepl("^s|^r|^e", trait) ~ "Mean"),
    term = case_when(
      grepl("TTtreatRTC:Ssummer_temp:SYear", term) ~ "t x year x removal",
      grepl("TTtreatRTC:SannPrecip:SYear", term) ~ "P x year x removal", 
      grepl("TTtreatRTC:SYear", term) ~ "Year x removal",
      grepl("TTtreatRTC", term) ~ "removal")) %>% 
  mutate(trait = if_else(grepl("wmean", trait), substr(trait, 6, regexpr("_", trait)),
                         if_else(grepl("cwv", trait), substr(trait, 4, regexpr("_", trait)), trait))) %>% 
  mutate(trait = if_else(grepl("_", trait), substr(trait, 1, (nchar(trait) -1)), trait)) %>% 
  mutate(sign = recode(trait, sumcover = 1, evenness = 1, richness = 1, seedMass = 1, height = 0, LA = 0, LTH = 0, LDMC = 0, CN = 1, SLA = 1))

write.csv(mod1tempBF, file = "~/OneDrive - University of Bergen/Research/mod1tempBFOUT.csv")

