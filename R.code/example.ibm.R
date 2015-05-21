
year <- 0:12

ex1 <- 0:7
ex2 <- 0:4
ex3 <- 0:11
ex4 <- 0:12

pdf("~/Dropbox/talks/figures/ibm.pdf", height=8.5, width=14, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
plot(ex1,rep(4,length(ex1)),bty="l",ylab=NA,xlab=NA,axes=FALSE,type="l",xlim=c(-1,12),ylim=c(0,4.5))
abline(v=10,col="grey",lwd=3)

points(ex1[1],4,pch=19,cex=3.5)
points(ex1[length(ex1)],4,pch=4,cex=2,lwd=2)
text(ex1[1],4.1,"2-5cm",pos=3)
text(ex1[length(ex1)],4,"Breast Cancer",pos=3)

lines(ex2,rep(3,length(ex2)))
points(ex2[1],3,pch=19,cex=1.75)
points(ex2[length(ex2)],3,pch=4,cex=2,lwd=2)
text(ex2[1],3,"1-2cm",pos=3)
text(ex2[length(ex2)],3,"CVD",pos=3)

lines(ex3,rep(2,length(ex3)))
points(ex3[1],2,pch=19,cex=1.25)
points(ex3[length(ex3)],2,pch=4,cex=2,lwd=2)
text(ex3[1],2,"1-2cm",pos=3)
text(ex3[length(ex3)],2,"Breast Cancer",pos=3)

arrows(0,1,ex4[length(ex4)],1,length=0.15)
points(ex4[1],1,pch=19,cex=0.75)
text(ex4[1],1,"<1cm",pos=3)
text(ex4[length(ex4)],1,"Alive",pos=3)


axis(1,at=c(0,10),paste(c("Diagnosis","Diagnosis+10")))
dev.off()
