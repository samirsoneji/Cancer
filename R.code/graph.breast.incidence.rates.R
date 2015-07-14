rm(list=ls())
load("~/Desktop/Cancer/data/mx.breast.size.Rdata") #incidence rates by age group (10-year increments) and size are size.rate3

scale <- 100000

pdf("~/Desktop/Cancer/figures/breast_incidence.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white", cex=1,cex.main=1)
for (s in 1:dim(size.rate3)[3]) {
  matplot(1975:2002,scale*size.rate3[3:30,,s],type="l",lty=1,las=1,xlab="year",ylab="incidence rate (per 100,000)",bty="l",col=1:8)
  title(paste("size:",s))
  text(2002,scale*size.rate3["2002",,s],paste(seq(40,80,10)),col=1:8,pos=4)
}
dev.off()  
