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
ex.stage <- as.matrix(create.datos.fxn(mx.breast,prop.breast,as.numeric(year.list))[c("ex.insitu","ex.localized","ex.regional","ex.distant")])
ex.overall <- c(stage[,1] %*% ex.stage[2,]*0.9,stage[,2] %*% ex.stage[2,]) #*0.9 to create more vertical distance between two bars in panel C


color <- brewer.pal(6,"YlGnBu")[-c(1,2)]
stage.list <- c("In Situ","Localized","Regional","Distant")
width <- 0.2
offset <- 0.025
grey.color <- brewer.pal(4,"Greys")[3]


pdf("~/Desktop/Cancer/figures/example_decomp1a.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for (i in 1:2) {
    for (j in c(1,4))
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],": ",round(100*stage[j,i]),"%",sep=""),cex=0.8)
    for (j in c(2,3))
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],":\n ",round(100*stage[j,i]),"%",sep=""),cex=0.8)
}

#mx <- mx.breast["75",as.character(c(1981,2001)),]
mx <- mx.breast.cause[,"75",as.character(c(1981,2001)),]
mx["other","2001",] <- mx["other","1981",]
mx["breast","2001",3:4] <- 0.5*mx["breast","2001",3:4]
mx["breast",,3] <- 0.85*mx["breast",,3]
mx["breast",,4] <- 0.1+mx["breast",,4]
mx["other",,2] <- 0.09+mx["other",,2]
mx["other",,3] <- 0.01+mx["other",,3]
mx["other",,4] <- 0.01+mx["other",,4]
matplot(log(mx[,,1]),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(min(log(mx)),max(log(mx))),xlim=c(0.85,2.15),cex=2,col=color[1],ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
for (i in 1:4) {
    matpoints(log(t(mx[,,i])),col=color[i],pch=19)
    matlines(log(t(mx[,,i])),col=color[i],lty=1:2,lwd=2)
}
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(4)],pos=3,col=color[4],paste(c("Distant, Cancer")),srt=-15)
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(1,2,3)],pos=3,col=color[c(1,2,3)],paste(c("In Situ, Cancer","Localized, Cancer","Regional, Cancer")),srt=-20)
text(rep(1.5,8),log(c(t(apply(mx,c(1,3),mean))))[c(5,6,7,8)],pos=3,col=color[c(1,2,3,4)],paste(c("In Situ, Other","Localized, Other","Regional, Other","Distant, Other")))
barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,4]/2,paste(stage.list),srt=90)
axis(1,at=c(mean(xpos[1:4]),mean(xpos[5:8])),paste(c("Time 1","Time 2")),tick=FALSE)

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5),col=1)
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(1.3,ex.overall[1],1.3,ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall)+1.5,
     wrap_sentence("Gain in life exp. entirely due to decreases in stage specific mort. rates from cancer",12),cex=0.8)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("D. Overall Life Expectancy",side=3,line=0,outer=TRUE,at=7/8,cex=1)
dev.off()



stage <- t(prop.breast[year.list,])
pdf("~/Desktop/Cancer/figures/example_decomp1b.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
for (i in 1:2) {
    for (j in c(1,4))
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],": ",round(100*stage[j,i]),"%",sep=""),cex=0.8)
    for (j in c(2,3))
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],":\n ",round(100*stage[j,i]),"%",sep=""),cex=0.8)
}

mx <- mx.breast["75",as.character(c(1981,2001)),]
mx[2,] <- mx[1,]
matplot(mx,yaxt="n",xaxt="n",bty="l",pch=19,ylim=c(min(mx),max(mx)),xlim=c(0.85,2.15),cex=2,col=color,ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
matlines(mx,col=color,lty=1,lwd=2)
text(rep(1.5,4),apply(mx,2,mean)+c(-0.0025,0,0,-0.0025),pos=3,col=color,paste(c("In Situ","Localized","Regional","Distant")))

ex.stage[2,] <- ex.stage[1,]
barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,4]/2,paste(stage.list),srt=90)
axis(1,at=c(mean(xpos[1:4]),mean(xpos[5:8])),paste(c("Time 1","Time 2")),tick=FALSE)

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5),col=1)
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(1.3,ex.overall[1],1.3,ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall)+1,
     wrap_sentence("Gain in life exp. entirely due to stage shift",12),cex=0.8)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("D. Overall Life Expectancy",side=3,line=0,outer=TRUE,at=7/8,cex=1)
dev.off()


prop.breast["2001",] <- prop.breast["1981",]
mx.breast.cause[,,"1981",] <- (1+2) * mx.breast.cause[,,"1981",]
mx.breast.cause["breast",,"1981","0. in situ"] <- (1+100) * mx.breast.cause["breast",,"1981","0. in situ"]
mx.breast.cause["other",,"2001","4. distant"] <- (1-0.99) * mx.breast.cause["other",,"2001","4. distant"]
mx.breast.cause["breast",,"2001","4. distant"] <- (1-0.99) * mx.breast.cause["breast",,"2001","4. distant"]
mx.breast.cause["other",,"2001","0. in situ"] <- (1-1) * mx.breast.cause["other",,"2001","0. in situ"]
mx.breast.cause["breast",,"2001","0. in situ"] <- (1-1) * mx.breast.cause["breast",,"2001","0. in situ"]
mx.breast[,"1981",] <- mx.breast.cause["other",,"1981",] +  mx.breast.cause["breast",,"1981",]
mx.breast[,"2001",] <- mx.breast.cause["other",,"2001",] +  mx.breast.cause["breast",,"2001",]
breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(1981,2001))
ex <- create.datos.fxn(mx.breast,prop.breast,c(1981,2001))[,"ex.overall"]
decomp <- unlist(c(breast[12],breast[14],breast[16],breast[18],sum(breast[c(13,15,17,19)])))
decomp.mat <- as.matrix(cbind(rep(NA,5),decomp))

pdf("~/Desktop/Cancer/figures/example_decomp2.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

load("~/Desktop/Cancer/data/mx.breast.Rdata")
mx <- mx.breast.cause[,"75",c("1981","2001"),]
matplot(t(mx[,,2]),yaxt="n",xaxt="n",bty="l",pch=NA,xlim=c(0.85,2.15),ylab=NA,axes=FALSE,
        ylim=c(min(mx),max(mx)))
matpoints(t(mx[,,1]),pch=18,col=color[1])
matpoints(t(mx[,,2]),pch=19,col=color[2])
matpoints(t(mx[,,3]),pch=17,col=color[3])
matpoints(t(mx[,,4]),pch=15,col=color[4])
matlines(t(mx[,,1]),col=color[1],lty=1:2)
matlines(t(mx[,,2]),col=color[2],lty=1:2)
matlines(t(mx[,,3]),col=color[3],lty=1:2)
matlines(t(mx[,,4]),col=color[4],lty=1:2)
legend(1.35,0.25,paste(c("Distant, Cancer","Distant, Other","Regional, Cancer","Regional, Other","Localized, Cancer","Localized, Other","In Situ, Cancer","In Situ, Other")),
       ncol=1,lty=1:2,col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),
       text.col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),
       pch=c(15,15,17,17,19,19,18,18),bty="n")
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)

barplot(ex,col=1,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(ex.stage[,3],plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(1.3,ex[1],1.3,ex[2],code=3,length=0.1)
text(1.3,mean(ex),"Gain",pos=2)

barplot(decomp.mat,xlim=c(1,2.75),ylim=c(0,max(ex)),axes=FALSE,xlab=NA,names.arg=c(NA,"Time 2 - Time 1"),
        border=c(color,"darkgrey"),col=c(color,"darkgrey"))
arrows(1.2,0,1.2,diff(ex),code=3,length=0.1)
text(1.2,diff(ex)/2,"Gain",pos=2)
text(1.9,1.2,"In Situ, Cancer",col="black")
text(1.9,5,"Localized, Cancer",col="black")
text(1.9,11,"Regional, Cancer",col="black")
text(1.9,15,"Distant, Cancer",col="white")
text(1.9,20,"Other Causes",col=1)
text(1.9,40,
     wrap_sentence("Contribution of Reductions in Mortality from Cancer (By Stage of Diagnosis) and Mortality from All Other Causes to Gain in Life Expectancy",20))


mtext("A. Stage-Specific Cancer Mortality Rates\nBy Cause of Death",side=3,line=0,outer=TRUE,at=1/6)
mtext("B. Life Expectancy",side=3,line=0,outer=TRUE,at=3/6)
mtext("C. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=5/6)
dev.off()
