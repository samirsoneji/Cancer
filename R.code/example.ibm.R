
year <- 0:12

ex1 <- 0:7
ex2 <- 0:4
ex3 <- 0:11
ex4 <- 0:12

plot(ex1,rep(4,length(ex1)),bty="l",ylab=NA,xlab=NA,axes=FALSE,type="l",xlim=c(-1,12),ylim=c(0,4.5))
abline(v=10,col="grey",lwd=3)

points(ex1[1],4,pch=19,cex=2)
points(ex1[length(ex1)],4,pch=4,cex=2,lwd=2)
text(ex1[1],4,"Regional",pos=3)
text(ex1[length(ex1)],4,"Breast Cancer",pos=3)

lines(ex2,rep(3,length(ex2)))
points(ex2[1],3,pch=19,cex=2)
points(ex2[length(ex2)],3,pch=4,cex=2,lwd=2)
text(ex2[1],3,"Localized",pos=3)
text(ex2[length(ex2)],3,"CVD",pos=3)

lines(ex3,rep(2,length(ex3)))
points(ex3[1],2,pch=19,cex=2)
points(ex3[length(ex3)],2,pch=4,cex=2,lwd=2)
text(ex3[1],2,"Localized",pos=3)
text(ex3[length(ex3)],2,"Breast Cancer",pos=3)

arrows(0,1,ex4[length(ex4)],1,length=0.15)
points(ex4[1],1,pch=19,cex=2)
text(ex4[1],1,"In Situ",pos=3)
text(ex4[length(ex4)],1,"Alive",pos=3)


axis(1,at=c(0,10),paste(c("Diagnosis","Diagnosis+10")))
