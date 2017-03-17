CN <-read.csv2("Innsamlet data/CN_Ragnhild.csv", header=TRUE, sep=";")
library("tidyr")
library("dplyr")
library("ggplot2")

CN$Info<- as.character(CN$Info)

CN<-CN %>%
  filter(Info=="")%>%
  mutate(Site= substr(Name, 1,2), Species=as.factor(substr(Name, 3,6)), Individual=substr(Name, 7,7), CN.ratio=as.numeric(CN.ratio))%>%
  filter(Species!="etan")%>%
  group_by(Species)%>%
  as.data.frame()

ggplot(CN, aes(x=Site, y=CN.ratio))+
    geom_boxplot()+
      facet_grid(as.formula(.~Species))+
  geom_point()
