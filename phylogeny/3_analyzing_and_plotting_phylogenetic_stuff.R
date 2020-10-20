#Seedclim: analyzing and plotting phylogenetic patterns
#Brian Maitner.  BMaitner at gmail
#####################
#Metadata:

#Treaments (TTtreat)
#Levels: TTC TT1 TT2 TT3 TT4
#warmer (TT2) 
#wetter (TT3)
#warmer and wetter (TT4) climates, 

#transplanting within blocks (to control for the transplanting itself)(TT1), 
#untouched control plot (TTC)

#####################

#########################
#Adding flux data

load("phylogeny/Cflux_Norway_FunCaB_XC_TTC.RData")
cflux<-CO2_GPP_1516
rm(CO2_GPP_1516)
cover.meta$turfID2<-gsub(pattern = " ",replacement = "",x = cover.meta$turfID)
cflux<-cflux[c("turfID.x","year","Reco15","GPP700")]
cover.meta<-merge(x = cover.meta,y = cflux,by.x = c("turfID2","year"),by.y = c("turfID.x","year"),all.x = T)

#Reco15 and GPP700

#Reco15 as response 

    #pd	mpd	mntd	vpd	vntd	
    reco_v_pd <- lm(Reco15 ~pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_mpd<-lm(Reco15 ~mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_mntd<-lm(Reco15 ~mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_vpd<-lm(Reco15 ~vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_vntd<-lm(Reco15 ~vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    #traitd	mtraitd	mntraitd	vtraitd	vntraitd	
    
    #reco_v_traitd<-lm(Reco15 ~pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_mtraitd<-lm(Reco15 ~mtraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_mntraitd<-lm(Reco15 ~mntraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_vtraitd<-lm(Reco15 ~vtraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_vntraitd<-lm(Reco15 ~vntraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    #trait_pd	trait_mpd	trait_mntd	trait_vpd	trait_vntd	
    
    reco_v_trait_pd<-lm(Reco15 ~trait_pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_trait_mpd<-lm(Reco15 ~trait_mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_trait_mntd<-lm(Reco15 ~trait_mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_trait_vpd<-lm(Reco15 ~trait_vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_trait_vntd<-lm(Reco15 ~trait_vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)

    #trait_rate_pd	trait_rate_mpd	trait_rate_mntd	trait_rate_vpd	trait_rate_vntd

    reco_v_trait_rate_pd<-lm(Reco15 ~trait_rate_pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_trait_rate_mpd<-lm(Reco15 ~trait_rate_mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_trait_rate_mntd<-lm(Reco15 ~trait_rate_mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    reco_v_trait_rate_vpd<-lm(Reco15 ~trait_rate_vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    reco_v_trait_rate_vntd<-lm(Reco15 ~trait_rate_vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    
    
    library(stargazer)
    
    
    stargazer(... = reco_v_pd, reco_v_mpd, reco_v_mntd, reco_v_vpd, reco_v_vntd,   
              reco_v_mtraitd,reco_v_mntraitd,reco_v_vtraitd,reco_v_vntraitd,
              reco_v_trait_pd,reco_v_trait_mpd, reco_v_trait_mntd,reco_v_trait_vpd,reco_v_trait_vntd,
              reco_v_trait_rate_pd,reco_v_trait_rate_mpd, reco_v_trait_rate_mntd,reco_v_trait_rate_vpd, reco_v_trait_rate_vntd,
              out = "C:/Users/Brian/Desktop/current_projects/seedclimComm/phylogeny/plots_figures/reco_stargazer.txt",dep.var.caption = "",type = "text",title="")
    

    
#GPP700 as response 
    
    #pd	mpd	mntd	vpd	vntd	
    gpp_v_pd<-lm(GPP700 ~pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_mpd<-lm(GPP700 ~mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_mntd<-lm(GPP700 ~mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_vpd<-lm(GPP700 ~vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_vntd<-lm(GPP700 ~vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    #traitd	mtraitd	mntraitd	vtraitd	vntraitd	
    
    #gpp_v_traitd<-lm(GPP700 ~pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_mtraitd<-lm(GPP700 ~mtraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_mntraitd<-lm(GPP700 ~mntraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_vtraitd<-lm(GPP700 ~vtraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_vntraitd<-lm(GPP700 ~vntraitd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    #trait_pd	trait_mpd	trait_mntd	trait_vpd	trait_vntd	
    
    gpp_v_trait_pd<-lm(GPP700 ~trait_pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_trait_mpd<-lm(GPP700 ~trait_mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_trait_mntd<-lm(GPP700 ~trait_mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_trait_vpd<-lm(GPP700 ~trait_vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_trait_vntd<-lm(GPP700 ~trait_vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    #trait_rate_pd	trait_rate_mpd	trait_rate_mntd	trait_rate_vpd	trait_rate_vntd
    
    gpp_v_trait_rate_pd<-lm(GPP700 ~trait_rate_pd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_trait_rate_mpd<-lm(GPP700 ~trait_rate_mpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_trait_rate_mntd<-lm(GPP700 ~trait_rate_mntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    
    gpp_v_trait_rate_vpd<-lm(GPP700 ~trait_rate_vpd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)
    gpp_v_trait_rate_vntd<-lm(GPP700 ~trait_rate_vntd_abd_std + annualPrecipitation +summerTemperature,data = cover.meta)

    
stargazer(... = gpp_v_pd, gpp_v_mpd, gpp_v_mntd, gpp_v_vpd, gpp_v_vntd,   
              gpp_v_mtraitd,gpp_v_mntraitd,gpp_v_vtraitd,gpp_v_vntraitd,
              gpp_v_trait_pd,gpp_v_trait_mpd, gpp_v_trait_mntd,gpp_v_trait_vpd,gpp_v_trait_vntd,
              gpp_v_trait_rate_pd,gpp_v_trait_rate_mpd, gpp_v_trait_rate_mntd,gpp_v_trait_rate_vpd, gpp_v_trait_rate_vntd,
              out = "C:/Users/Brian/Desktop/current_projects/seedclimComm/phylogeny/plots_figures/gpp_stargazer.txt",dep.var.caption = "",type = "text",title="")
    
  
library(rsq)

rsq.partial(objF = gpp_v_mtraitd)

summary(lm(GPP700 ~mtraitd_abd_std,data = cover.meta))


plot(cover.meta$mtraitd_abd_std~cover.meta$summerTemperature)


#########################
pairs(cover.meta[14:ncol(cover.meta)])

colnames(cover.meta)
