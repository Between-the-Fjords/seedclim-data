#Package to make plots
library(RColorBrewer)
library(cowplot)
library(wesanderson)

#source("/Users/fja062/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/multiplot_function.R")


#### palettes and labelling ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3", "white")

plot(1:length(cbPalette), col = cbPalette, pch = 16, cex = 5) #check colour and order

precip.lab <-   scale_x_discrete("Precipitation [mm y-1]",
                                 labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- scale_x_discrete("Temperature [C]",
                             labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))

axis.dim <- theme(axis.text=element_text(size=11),
                  axis.title=element_text(size=14),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=11),
                  legend.title = element_text(size=13),
                  strip.text.x = element_text(size = 13),
                  strip.text.y = element_text(size = 13))

axis.dimLarge <- theme(axis.text=element_text(size=13),
                  axis.title=element_text(size=16),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=16),
                  strip.text.x = element_text(size = 16),
                  strip.text.y = element_text(size = 16))


legend.title.prec <- "Treatment and \n precipitation"
legend.title.temp <- "Treatment \n and temperature"
legend.title.treat <- "Treatment"
legend.title.weat <- " \n "
legend.title.climate <- ""


pal1 <- wes_palette(7, name = "Darjeeling2", type = "continuous")
pal2 <- wes_palette(7, name = "Cavalcanti1", type = "continuous")
