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


breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(1981,2001))

color <- brewer.pal(6,"YlGnBu")[-c(1,2)]
stage.list <- c("In Situ","Localized","Regional","Distant")
width <- 0.2
offset <- 0.025
grey.color <- brewer.pal(4,"Greys")[3]


pdf("~/Desktop/Cancer/figures/example_decomp1.pdf", height=5.5, width=11, paper="special")
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
arrows(1.3,ex.overall[1],1.3,ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall)+1,
     wrap_sentence("Gain in life exp. entirely due to decreases in stage specific mort. rates",12),cex=0.8)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("D. Overall Life Expectancy",side=3,line=0,outer=TRUE,at=7/8,cex=1)
dev.off()



pdf("~/Desktop/Cancer/figures/example_decomp2.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

mx <- mx.breast.cause[,"75",c("1981","2001"),3]
matplot(t(mx),yaxt="n",xaxt="n",bty="l",pch=19,xlim=c(0.85,2.15),cex=2,col=c("black","grey"),ylab=NA,axes=FALSE)
axis(1,at=1:2,labels=c("Time 1","Time 2"),tick=TRUE)
matlines(t(mx),col=c("black","grey"),lty=1,lwd=2)
text(1.5,apply(mx,1,mean),c("Cancer","Other Causes"),col=c("black","grey"),pos=3)

barplot(ex.stage[,3],col=1,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(ex.stage[,3],plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")),tick=FALSE)
arrows(1.3,ex.stage[1,3],1.3,ex.stage[2,3],code=3,length=0.1)
text(1.3,mean(ex.stage[,3]),"Gain",pos=2)
   


mtext("A. Regional Cancer Mortality Rates",side=3,line=0,outer=TRUE,at=1/6)
mtext("B. Life Expectancy",side=3,line=0,outer=TRUE,at=3/6)
mtext("C. Cancer PYLL",side=3,line=0,outer=TRUE,at=5/6)
dev.off()
