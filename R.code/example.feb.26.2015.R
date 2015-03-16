library(RColorBrewer)
load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 
source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.fxn.R")

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


pdf("~/Desktop/Cancer/figures/example.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(2,5),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

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

mx <- mx.breast["75",as.character(c(1981,2001)),]
matplot(mx,yaxt="n",xaxt="n",bty="l",pch=19,ylim=c(min(mx),max(mx)),xlim=c(0.85,2.15),cex=2,col=color,ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
matlines(mx,col=color,lty=1,lwd=2)
text(rep(1.5,4),apply(mx,2,mean)+c(-0.025,0,0,0.01),pos=3,col=color,paste(c("In Situ","Localized","Regional","Distant")))

barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,4]/2,paste(stage.list),srt=90)
axis(1,at=c(mean(xpos[1:4]),mean(xpos[5:8])),paste(c("Time 1","Time 2")),tick=FALSE)

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5),col=1)
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(xpos[1],ex.overall[1],xpos[1],ex.overall[2],code=3,length=0.1)
text(xpos[1]-0.3,mean(ex.overall),paste("Gain"))

barplot(diff(ex.overall),border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)),xlim=c(0,2),col=1)
xpos <- barplot(diff(ex.overall),xlim=c(0,2),plot=FALSE)
axis(1,at=xpos,paste("Time 2 - Time 1"),tick=FALSE)
arrows(xpos,diff(ex.overall),xpos,diff(ex.overall)+10,code=1,length=0.1)
text(xpos,diff(ex.overall)+10,wrap_sentence("Gain in life exp. entirely due to decreases in stage specific mort. rates that increased stage specific life exp.",12),pos=3)

mx <- matrix(c(3,10,3,10-2.86697),2,2)
rownames(mx) <- c("cancer","noncancer")
colnames(mx) <- c("1","2")
matplot(t(mx),yaxt="n",xaxt="n",bty="l",pch=19,ylim=c(0,12),xlim=c(0.85,2.15),cex=2,col=1,ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
matlines(t(mx),col=1,lty=1,lwd=2)
text(1.5,2.5,"Cancer",col=1)
text(1.5,7.3,"Other\nCauses",col=1)

matplot(t(mx),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(0,12),xlim=c(0.75,2.15),ylab=NA,axes=FALSE)
axis(1,at=c(1.1,1.9),labels=c("Time 1","Time 2"),tick=FALSE)
rect(0.90,0,0.90+width,6,col=grey.color,border=NA)
rect(0.90+width+offset,0,0.90+width+offset+width,7,col=grey.color,border=grey.color,density=30)
rect(1.70,0,1.70+width,7,col="black",border=NA)
rect(1.70+width+offset,0,1.70+width+offset+width,10,col="black",border="black",density=30)
arrows(0.9+width/2,6,0.9+width/2,7,code=3,length=0.1,col="darkgrey")
arrows(1.7+width/2,7,1.7+width/2,10,code=3,length=0.1,col="black")
text(0.82,6.5,expression("PYLL"[1]),col=grey.color,cex=0.66)
text(1.65,8.5,expression("PYLL"[2]),col="black",cex=0.66)
legend("topleft",ncol=1,bty="n",paste(c("Time 1: Non-Cancer & Cancer", "Time 1: Only Non-Cancer","Time 2: Non-Cancer & Cancer", "Time 2: Only Non-Cancer")),cex=0.66,border=c("darkgrey","darkgrey","black","black"),fill=c(grey.color,grey.color,"black","black"),text.col=c(grey.color,grey.color,"black","black"),col=c(grey.color,grey.color,"black","black"),density=c(200,40,200,40))

matplot(t(mx),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(0,11),xlim=c(0.75,2.15),ylab=NA,axes=FALSE)
axis(1,at=c(1.1,1.9),labels=c("Time 1","Time 2"),tick=FALSE)
rect(0.9,0,0.9+width+width,1,col=grey.color,border=NA)
rect(1.7,0,1.7+width+width,3,col="black",border=NA)
arrows(1.55,1,1.55,3,code=3,length=0.1,col="black")
text(1.1,0.5,expression("PYLL"[1]),col="white",cex=0.66)
text(1.9,1.5,expression("PYLL"[2]),col="white",cex=0.66)
text(1.23,2.4,"Growth=",col="black",cex=0.66)
text(1.23,1.8,expression(PYLL[2]-PYLL[1]),col="black",cex=0.66)

matplot(t(mx),yaxt="n",xaxt="n",bty="l",pch=NA,ylim=c(0,11),xlim=c(0.75,2.15),ylab=NA,axes=FALSE)
axis(1,at=1.5,labels="Time 2 - Time 1",tick=FALSE)
rect(1.5-width,0,1.5+width,2,col="black",border=NA)
arrows(1.5,5.5,1.5,2.15,code=2,col="black",lwd=2)
text(1.5,1.4,expression(PYLL[2]-phantom(.)),col="white",cex=0.66)
text(1.455,0.9,expression(PYLL[1]),col="white",cex=0.66)
text(1.5,7.2,
     wrap_sentence("Growth in cancer PYLL entirely due to declining other-cause mortality rates",12),cex=1)





mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/10,cex=1)
mtext("B. Stage-Specific\nMortality Rates",side=3,line=0,outer=TRUE,at=3/10,cex=1)
mtext("C. Stage-Specific\nLife Expectancy",side=3,line=0,outer=TRUE,at=5/10,cex=1)
mtext("D. Overall\nLife Expectancy",side=3,line=0,outer=TRUE,at=7/10,cex=1)
mtext("E. Gain in\nLife Expectancy",side=3,line=0,outer=TRUE,at=9/10,cex=1)
mtext("F. Regional Cancer\nMortality Rates",side=3,line=-26.5,outer=TRUE,at=1/10)
mtext("G. Life Expectancy",side=3,line=-25.5,outer=TRUE,at=3/10)
mtext("H. Cancer PYLL",side=3,line=-26.5,outer=TRUE,at=5/10)
mtext("I. Growth in Cancer PYLL",side=3,line=-26.5,outer=TRUE,at=7/10)
mtext("Decomposition 2",side=2,line=0,outer=TRUE,at=1/4,cex=1)
mtext("Decomposition 1",side=2,line=0,outer=TRUE,at=3/4,cex=1)
dev.off()

