library(RColorBrewer)
load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 
source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.fxn.R")
source("~/Desktop/Cancer/R.code/results.fxn.R")

wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}

year.list <- as.character(c(1981,2001))
stage <- t(prop.breast[year.list,])
mx <- mx.breast["75",as.character(c(2001)),]
ex.stage <- as.matrix(create.datos.fxn(mx.breast,prop.breast,as.numeric(year.list))[c("ex.localized","ex.regional","ex.distant")])
ex.overall <- c(stage[,1] %*% ex.stage[2,]*0.9,stage[,2] %*% ex.stage[2,]) #*0.9 to create more vertical distance between two bars in panel C


color <- brewer.pal(7,"YlGnBu")[c(3,5,7)]
stage.list <- c("Localized","Regional","Distant")
width <- 0.2
offset <- 0.025
grey.color <- brewer.pal(4,"Greys")[3]


pdf("~/Dropbox/talks/figures/example_decomp1a1.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for(i in 1:2) {
  text(xpos[i],100*stage[1,i]/2,"Localized")
  text(xpos[i],100*(stage[1,i]+stage[2,i]/2),"Regional")
  text(xpos[i],100*(stage[1,i]+stage[2,i]+stage[3,i]/2),"Distant",col="white")
}
mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
dev.off()


pdf("~/Dropbox/talks/figures/example_decomp1a2.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for(i in 1:2) {
  text(xpos[i],100*stage[1,i]/2,"Localized")
  text(xpos[i],100*(stage[1,i]+stage[2,i]/2),"Regional")
  text(xpos[i],100*(stage[1,i]+stage[2,i]+stage[3,i]/2),"Distant",col="white")
}

#mx <- mx.breast["75",as.character(c(1981,2001)),]
mx <- mx.breast.cause[,"75",as.character(c(1981,2001)),]
mx["other","2001",] <- mx["other","1981",]
mx["breast","2001",3] <- 0.5*mx["breast","2001",3]
mx["breast","2001",2] <- 0.5*mx["breast","2001",2]
mx["breast",,3] <- 1.5*mx["breast",,3]
mx["breast",,2] <- 0.75*mx["breast",,2]
mx["other",,2] <- 0.09+mx["other",,2]
mx["other",,3] <- 0.01+mx["other",,3]
matplot(log(mx[,,1]),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(min(log(mx)),max(log(mx))),xlim=c(0.85,2.15),cex=2,col=color[1],ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
for (i in 1:3) {
    matpoints(log(t(mx[,,i])),col=color[i],pch=19)
    matlines(log(t(mx[,,i])),col=color[i],lty=1:2,lwd=2)
}
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(3)],pos=3,col=color[3],paste(c("Distant, Cancer")),srt=-30)
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(1,2)],pos=3,col=color[1:2],paste(c("Localized, Cancer","Regional, Cancer")),srt=-30)
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(4,5,6)],pos=3,col=color,paste(c("Localized, Other","Regional, Other","Distant, Other")))

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
dev.off()


pdf("~/Dropbox/talks/figures/example_decomp1a3.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for(i in 1:2) {
  text(xpos[i],100*stage[1,i]/2,"Localized")
  text(xpos[i],100*(stage[1,i]+stage[2,i]/2),"Regional")
  text(xpos[i],100*(stage[1,i]+stage[2,i]+stage[3,i]/2),"Distant",col="white")
}

#mx <- mx.breast["75",as.character(c(1981,2001)),]
mx <- mx.breast.cause[,"75",as.character(c(1981,2001)),]
mx["other","2001",] <- mx["other","1981",]
mx["breast","2001",3] <- 0.5*mx["breast","2001",3]
mx["breast","2001",2] <- 0.5*mx["breast","2001",2]
mx["breast",,3] <- 1.5*mx["breast",,3]
mx["breast",,2] <- 0.75*mx["breast",,2]
mx["other",,2] <- 0.09+mx["other",,2]
mx["other",,3] <- 0.01+mx["other",,3]
matplot(log(mx[,,1]),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(min(log(mx)),max(log(mx))),xlim=c(0.85,2.15),cex=2,col=color[1],ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
for (i in 1:3) {
    matpoints(log(t(mx[,,i])),col=color[i],pch=19)
    matlines(log(t(mx[,,i])),col=color[i],lty=1:2,lwd=2)
}
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(3)],pos=3,col=color[3],paste(c("Distant, Cancer")),srt=-30)
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(1,2)],pos=3,col=color[1:2],paste(c("Localized, Cancer","Regional, Cancer")),srt=-30)
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(4,5,6)],pos=3,col=color,paste(c("Localized, Other","Regional, Other","Distant, Other")))

barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,3]/2,paste(stage.list),srt=90,col=c(1,1,"white",1,1,"white"))
axis(1,at=c(mean(xpos[1:3]),mean(xpos[5:8])),paste(c("Time 1","Time 2")),tick=FALSE)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
dev.off()


pdf("~/Dropbox/talks/figures/example_decomp1a4.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for(i in 1:2) {
  text(xpos[i],100*stage[1,i]/2,"Localized")
  text(xpos[i],100*(stage[1,i]+stage[2,i]/2),"Regional")
  text(xpos[i],100*(stage[1,i]+stage[2,i]+stage[3,i]/2),"Distant",col="white")
}

#mx <- mx.breast["75",as.character(c(1981,2001)),]
mx <- mx.breast.cause[,"75",as.character(c(1981,2001)),]
mx["other","2001",] <- mx["other","1981",]
mx["breast","2001",3] <- 0.5*mx["breast","2001",3]
mx["breast","2001",2] <- 0.5*mx["breast","2001",2]
mx["breast",,3] <- 1.5*mx["breast",,3]
mx["breast",,2] <- 0.75*mx["breast",,2]
mx["other",,2] <- 0.09+mx["other",,2]
mx["other",,3] <- 0.01+mx["other",,3]
matplot(log(mx[,,1]),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(min(log(mx)),max(log(mx))),xlim=c(0.85,2.15),cex=2,col=color[1],ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
for (i in 1:3) {
    matpoints(log(t(mx[,,i])),col=color[i],pch=19)
    matlines(log(t(mx[,,i])),col=color[i],lty=1:2,lwd=2)
}
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(3)],pos=3,col=color[3],paste(c("Distant, Cancer")),srt=-30)
text(rep(1.5,3),log(c(t(apply(mx,c(1,3),mean))))[c(1,2)],pos=3,col=color[1:2],paste(c("Localized, Cancer","Regional, Cancer")),srt=-30)
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(4,5,6)],pos=3,col=color,paste(c("Localized, Other","Regional, Other","Distant, Other")))

barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,3]/2,paste(stage.list),srt=90,col=c(1,1,"white",1,1,"white"))
axis(1,at=c(mean(xpos[1:3]),mean(xpos[5:8])),paste(c("Time 1","Time 2")),tick=FALSE)

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5),col=1)
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(1.3,ex.overall[1],1.3,ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall)+2.5,
     wrap_sentence("Gain in life exp. entirely due to decreases in stage specific mort. rates from cancer",12),cex=0.8)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("D. Overall Life Expectancy",side=3,line=0,outer=TRUE,at=7/8,cex=1)
dev.off()
