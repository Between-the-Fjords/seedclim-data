#lazarus
apply(ex,1,diff)
exdiff<-apply(ex,1,diff)
sort(exdiff[1,])
sort(exdiff[2,])


turfs[exdiff[1,]<(-3),] #2011-2012 lazarus
turfs[exdiff[2,]<(-3),] #2012-2013 lazarus


boxplot(exdiff[1,]~cover.meta$recorder[cover.meta$Year==2011])
boxplot(exdiff[2,!is.na(exdiff[1,])]~cover.meta$recorder[cover.meta$Year==2012])

mapply(function(x1,x2){list(both=intersect(x1$extinct, x2$extinct),only11=setdiff(x1$extinct, x2$extinct), only12=setdiff(x2$extinct,x1$extinct))}, x1=w9.11[exdiff[1,]<(-3)], x2=w9.12[exdiff[1,]<(-3)], SIMPLIFY=FALSE)

mapply(function(x1,x2){list(both=intersect(x1$extinct, x2$extinct),only12=setdiff(x1$extinct, x2$extinct), only13=setdiff(x2$extinct,x1$extinct))}, x1=w9.12[exdiff[2,]<(-3)], x2=w9.13[exdiff[2,]<(-3)], SIMPLIFY=FALSE)



mapply(function(x1,x2){list(lazarus=setdiff(x1$extinct, x2$extinct), extras=setdiff(x1$immigrant,x2$immigrant))}, x1=w9.11[exdiff[1,]<(-3)], x2=w9.12[exdiff[1,]<(-3)], SIMPLIFY=FALSE)

mapply(function(x1,x2){list(lazarus=setdiff(x1$extinct, x2$extinct), extras=setdiff(x1$immigrant,x2$immigrant))}, x1=w9.12[exdiff[2,]<(-3)], x2=w9.13[exdiff[2,]<(-3)], SIMPLIFY=FALSE)
