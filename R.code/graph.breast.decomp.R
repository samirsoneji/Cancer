library(RColorBrewer)
load("~/Desktop/Cancer/results/breast.decomp.Rdata")
color <- brewer.pal(9,"RdBu")

#col=c(color[1],color[8],rep(c(color[9],color[7])\
b <- apply(breast,1,function(x) c(x[3],sum(x[4:7]),sum(x[c(12,14,16,18)]),sum(x[c(13,15,17,19)])))
rownames(b) <- c("total gain in life exp","stage shift","mortality, cancer", "mortality, other")
colnames(b) <- c("1973-1981","1981-1991","1991-2001")
pdf("~/Desktop/Cancer/figures/breast.pdf", height=5.5, width=5.5, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
barplot(b[2:4,],beside=FALSE,border=FALSE,col=c("red","light blue","dark blue"),las=1,xaxt="n")
legend("topright",c("stage shift","mortality, cancer","mortality, other"),col=c("red","light blue","dark blue"),pch=15,text.col=c("red","light blue","dark blue"),bty="n")
axis(1,at=barplot(b[2:3,],beside=FALSE,plot=FALSE),paste(c("1973 vs 1981 cohort","1981 vs 1991 cohort","1991 vs 2001 cohort")))
dev.off()

b2 <- apply(breast,1,function(x) c(x[3],sum(x[4:7]),sum(x[c(12,14,16,18)]),sum(x[c(13,15,17,19)]),
                                  x[12],x[13],x[14],x[15],x[16],x[17],x[18],x[19]))
rownames(b2)[1:4] <- c("total gain in life exp","stage shift","mortality, cancer", "mortality, other")
colnames(b2) <- c("1973-1981","1981-1991","1991-2001")

b3 <- b2[5:12,]
b4 <- matrix(b3,nrow=2,ncol=12)
b4[1,1] <- 1*b4[1,1] #tmp fix for spacing
space.values <- c(0.1,0.1,0.1,0.1,2,0.1,0.1,0.1,2,0.1,0.1,0.1)
ymin <- -0.2
ymax <- max(b4)
xpos <- barplot(b4,space=space.values)
pdf("~/Desktop/Cancer/figures/breast.decomp.pdf", height=5.5, width=5.5, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
barplot(b4,las=1,col=rep(c(color[9],color[7]),6),
        space=space.values,yaxt="n",cex.names=0.75,border=rep(c(color[9],color[7]),6),ylim=c(ymin,ceiling(ymax)))
abline(h=0,col="grey")
axis(2,las=1,at=seq(0,ceiling(ymax),0.25))
text(xpos,0,rep(c("IS","L","R","D"),4),pos=1)
text(c(mean(xpos[2:3]),mean(xpos[6:7]),mean(xpos[10:11])),
     ymin,c("1973-1981","1981-1991","1991-2001"),pos=3)
dev.off()
