#intra vs interspecific variability
library(dplyr)
library(cati)


load("/Users/fja062/Documents/seedclimComm/myGRdata.RData")
load("/Users/fja062/Documents/seedclimComm/traitScale.RData")
load("/Users/fja062/Documents/seedclimComm/traitVar.RData")

traitScale <- filter(traitScale, scale == "local")

test <- decompCTRE(traits = traitScale$measurement, formula = ~1, )



#example
data(finch.ind)

res.decomp <- decompCTRE(traits = traits.finch, sp = sp.finch, 
                         ind.plot = ind.plot.finch, print = FALSE)

barplot.decompCTRE(res.decomp)

par(mfrow = c(2,2))
barplot.decompCTRE(res.decomp, resume = FALSE)
par(mfrow = c(1,1))


# create example data frame
height.trait<- data.frame(MOWING=as.factor(c(0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0)))
height.trait$FERTIL <- as.factor(c(0,1,0,1,1,0,1,0,1,0,1,0))
height.trait$specific <- c( 58.16354, 62.34342, 31.43701, 62.14333,
                            51.98859, 29.95968, 55.48009, 50.68146, 51.75618, 31.13289,
                            47.53024, 56.44128)
height.trait$nonspec<- c( 47.93985, 57.09998, 43.06760, 51.58106,
                          44.52435, 40.85160, 50.85945, 44.48371, 43.20859, 43.92655,
                          45.15222, 47.83641)

x1<-traitflex.anova(~1, specific, nonspec, data=height.trait)
x1
plot(x1)

x2<-traitflex.anova(~ MOWING*FERTIL, specific, nonspec, data=height.trait)
x2
plot(x2)
