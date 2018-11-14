# making model coefficient heat maps


test<-lmer(deltarichness ~ temp + Year + temp:Year + (1|siteID/blockID/turfID), na.action=na.omit, REML=F, data=rtcmeta)

coefs <- data.frame(coef(test)[3])
coefs$ranef.intercept <- ranef(test)$siteID[1]

coef(test)$siteID[,"temp"]

ggplot(coefs, aes(x = precp, y= tempr, fill = Intercept)) +
  geom_tile()



precip.lab <- list(c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- list(c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
6.5,8.5,10.5
+
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp


coefs$precp <- c("0.6", "2", "0.6", "2", "1.2", "1.2", "2.7", "2", "2.7", "0.6", "2.7", "1.2")
coefs$tempr <- c("8.5", "10.5", "10.5", "6.5", "8.5", "6.5", "10.5", "8.5", "6.5", "6.5", "8.5", "10.5")
