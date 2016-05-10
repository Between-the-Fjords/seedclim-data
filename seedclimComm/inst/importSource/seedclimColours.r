#seedclim colours and symbols



ppt.colours<-colorRampPalette(c("lightblue", "darkblue"))(4)
tmp.colours<-colorRampPalette(c("pink", "red","#ba0000"))(3)
plot(1:4, pch=16, col=ppt.colours)
points(1:3, rep(1.5,3), pch=16, col=tmp.colours)


ppt.pch<-1:4
tmp.pch<-c(24,22,25)

treats<-paste("TT",c("C",1:4), sep="")
TT.colours<-c("grey40", "grey80", "red", "blue", "purple")
names(TT.colours)<-treats
