age <- read.csv("~/Dropbox/talks/figures/age.results.csv")
age.mat <- as.matrix(age[1:3,3:9])
colnames(age.mat) <- c("40-49","50-59","60-69","70-79","80-89","90-99","100+")

pdf("~/Dropbox/talks/figures/screening_age_contribution.pdf", height=8.5, width=14, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
barplot(age.mat[1,],las=1,axes=FALSE,border=FALSE,col="grey20",ylim=c(0,0.775))
text(barplot(age.mat[1,],plot=FALSE),age.mat[1,],paste(round(age.mat[1,],2)),pos=3)
mtext("Age Group (Years Old)",side=1,line=-0.75,outer=TRUE,at=1/2,cex=2)
dev.off()
 
