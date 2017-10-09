##### CSR analysis #####

library("vegan")
library("ggtern")


CSR_df <-read.csv("Traits/data/CSR_df_LDMC.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

CSR_df<-CSR_df%>%
  select(Species, C, S, R, Strategy_class)


CSR_community<- left_join(wcommunity_df, CSR_df, by=c("Species"="Species"))

CSR_community<-CSR_community%>%
  filter(!LDMC>1)%>%
  group_by(Species)%>%
  mutate(var_LDMC=var(LDMC)/LDMC_mean_global,
         var_SLA=var(SLA)/SLA_mean_global,
         var_Lth=var(Lth_ave)/Lth_mean_global,
         var_Height=var(Height)/Height_mean_global,
         var_LA=var(Leaf_area)/LA_mean_global,
         var_CN=var(CN.ratio)/CN_ratio_mean_global)%>%
  ungroup()


#library(gridExtra)

TheLucky4<-c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces")

CSR_community$highlight <- ifelse(CSR_community$Species == TheLucky4, "highlight", "normal")

multiplot(plot2, plot4, plot6, plot8, plot1, plot3, plot5, plot7, cols=2)

#plot1 <- 
ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = var_SLA))+
  #scale_shape_manual(values=c(1, 16))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  #guides(colour=FALSE, size=FALSE)+
  labs(title="Variance of SLA", colour="Distribution")+
  scale_colour_manual(labels = c("Alpine", "Generalist", "Lowland"), values=rev(c("#FF6666","#FFCC33","#99CCFF")))+
  guides(size=FALSE)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(data=subset(CSR_community, Species == TheLucky4), aes(label=Species), hjust = 0, vjust = 0.5, show.legend = FALSE)
  #geom_point(data=subset(CSR_community, Species == TheLucky4), aes(shape=1))
  


geom_point(data=mydata[10:13, ], aes(x=a, y=b), colour="red", size=5)

geom_text(data=subset(mtcars, wt > 4 | mpg > 25),
          aes(wt,mpg,label=name))
  #geom_text(aes(label=ifelse(Species==TheLucky4,Species,''), hjust=0.5,vjust=0.5))

plot2 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = SLA_mean_global))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)+
  labs(title="Mean")+
  theme(plot.title = element_text(hjust = 0.5))

plot3 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = var_LDMC))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)

plot4 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = LDMC_mean_global))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)


plot5 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = var_Lth))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)

plot6 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = Lth_mean_global))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)

plot7 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = var_Height))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)

plot8 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
  geom_point(aes(size = Height_mean_global))+
  tern_limits(T=1.1, L=1.1, R=1.1)+
  guides(colour=FALSE, size = FALSE)

#Bruker ikke CN ratio fordi jeg bare har maks tre målinger av CN ratio, så den vil ikke kalkulere CN_var

#plot9 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
#  geom_point(aes(size = var_CN))+
#  tern_limits(T=1.1, L=1.1, R=1.1)+
#  guides(colour=FALSE, size = FALSE)

#plot10 <- ggtern(CSR_community, aes(R,C,S, colour=occurrence))+
#  geom_point(aes(size = CN_ratio_mean_global))+
#  tern_limits(T=1.1, L=1.1, R=1.1)+
#  guides(colour=FALSE, size = FALSE)



#### Making the PCA that I didn't use ####

#CSR_data <- wcommunity_df%>%
#  select(species, LDMC_mean_global, SLA_mean_global, LA_mean_global)%>%
#  distinct()%>%
#  mutate(LA=log(LA_mean_global))%>%
#  filter(!is.na(LDMC_mean_global))%>%
#  select(-LA_mean_global)


#mod <- rda(select(CSR_data, -species), scale= TRUE)

#plot(mod)


#### Multiplot function ####

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
