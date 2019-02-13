###### weighted means by specialism
specialism <- my.GR.data %>%
  group_by(ID, specialism) %>%
  filter(functionalGroup != "graminoid") %>%
  mutate(wmean_height = weighted.mean(height, cover), wmean_leafSize = weighted.mean(leafSize, cover), wmean_seedMass = weighted.mean(seedMass, cover), wmean_SLA = weighted.mean(SLA, cover), Year = Year, sumcover = sum(cover)) %>%
  mutate(wmean_LDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE), wmean_SLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE), wmean_LTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE), wmean_LA_local = weighted.mean(LA_mean, cover, na.rm = TRUE), wmean_height_local = weighted.mean(Height_mean, cover, na.rm = TRUE)) %>%
  distinct(ID, specialism, .keep_all = TRUE) %>%
  select(-(functionalGroup:seedMass), -species) %>%
  as.data.frame()

specialism$specYear <- as.factor(paste(specialism$specialism, specialism$Year, sep = "_"))
specialism[specialism$specialism == "alpine",] <- "other"


spectimedeltacalc <- sapply(1:nrow(specialism[specialism$Year != 2011,]), function(i){
  R <- forbcom[forbcom$Year != 2011,][i,]
  #browser()
  cols <- c("wmean_height","wmean_leafSize", "sumcover", "totalBryophytes", "wmean_seedMass", "wmean_maxheight", "wmean_minheight", "diversity", "richness", "evenness", "wmeanLDMC_global", "wmeanSLA_global", "wmeanLTH_global", "wmeanLA_global", "wmeanheight_global", "wmeanCN_global", "wmeanLDMC_local", "wmeanSLA_local", "wmeanLTH_local", "wmeanLA_local", "wmeanheight_local", "wmeanCN_local")
  friend <- forbcom$turfID == R$turfID & forbcom$Year == 2011
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- forbcom[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
timedeltacalc <- as.data.frame(t(spectimedeltacalc))
colnames(spectimedeltacalc) <- paste0("delta", colnames(spectimedeltacalc))
spectimedelta <- cbind((forbcom[forbcom$Year != 2011,]), spectimedeltacalc)
spectimedelta <- filter(spectimedelta, deltasumcover > -80)



special <- sapply(1:nrow(specialism[specialism$TTtreat == "RTC",]), function(i){
  R <- specialism[specialism$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("wmean_height","wmean_SLA","wmean_leafSize", "wmean_seedMass", "sumcover", "wmean_LDMC_local", "wmean_SLA_local", "wmean_LTH_local", "wmean_LA_local", "wmean_height_local")
  friend <- specialism$Year == R$Year & specialism$blockID == R$blockID & specialism$specialism == R$specialism & specialism$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- specialism[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
  })

special <- as.data.frame(t(special))
colnames(special) <- paste0("delta", colnames(special))
special <- cbind((specialism[specialism$TTtreat == "RTC",]), special)


## ---- Explanatory.variable.deltas.end ---- 