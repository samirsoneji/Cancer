library(RColorBrewer)
load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 

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


pdf("~/Desktop/Cancer/figures/example.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(2,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")))
for (i in 1:2)
    for (j in 1:4)
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],": ",round(100*stage[j,i]),"%",sep=""),cex=0.8)

barplot(rep(ex.stage[2,],2),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(rep(ex.stage[2,],2),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,4]/2,paste(stage.list),srt=90)
axis(1,at=c(mean(xpos[1:4]),mean(xpos[5:8])),paste(c("Time 1","Time 2")))

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5))
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")))
arrows(xpos[1],ex.overall[1],xpos[1],ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall),paste("Gain"),pos=2)

barplot(diff(ex.overall),border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)),xlim=c(0,2))
xpos <- barplot(diff(ex.overall),xlim=c(0,2),plot=FALSE)
axis(1,at=xpos,paste("Time 2 - Time 1"))
arrows(xpos,diff(ex.overall),xpos,diff(ex.overall)+10,code=1,length=0.1)
text(xpos,diff(ex.overall)+10,"Gain in life expectancy\nentirely due to shift\nin stage of diagnosis",pos=3)

stage[,2] <- stage[,1]
barplot(100*stage,col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(100*stage,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")))
for (i in 1:2)
    for (j in 1:4)
        text(xpos[i],100*apply(stage,2,function(x) c(x[1]/2,cumsum(x)[-length(cumsum(x))]+(cumsum(x)[-1]-cumsum(x)[-length(cumsum(x))])/2))[j,i],
             paste(stage.list[j],": ",round(100*stage[j,i]),"%",sep=""),cex=0.8)

barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),col=color,border=FALSE,las=1,xaxt="n",yaxt="n")
xpos <- barplot(c(t(ex.stage)),space=c(0.1,0.1,0.1,0.1,1,0.1,0.1,0.1),plot=FALSE)
text(xpos,ex.stage[2,4]/2,paste(stage.list),srt=90)
axis(1,at=c(mean(xpos[1:4]),mean(xpos[5:8])),paste(c("Time 1","Time 2")))

barplot(ex.overall,border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)+5))
xpos <- barplot(ex.overall,plot=FALSE)
axis(1,at=xpos,paste(c("Time 1","Time 2")))
arrows(xpos[1],ex.overall[1],xpos[1],ex.overall[2],code=3,length=0.1)
text(xpos[1],mean(ex.overall),paste("Gain"),pos=2)

barplot(diff(ex.overall),border=FALSE,las=1,xaxt="n",yaxt="n",ylim=c(0,max(ex.overall)),xlim=c(0,2))
xpos <- barplot(diff(ex.overall),xlim=c(0,2),plot=FALSE)
axis(1,at=xpos,paste("Time 2 - Time 1"))
arrows(xpos,diff(ex.overall),xpos,diff(ex.overall)+10,code=1,length=0.1)
text(xpos,diff(ex.overall)+10,wrap_sentence("Gain in life exp. entirely due to decreases in stage specific mort. rates that increased stage specific life exp.",14),pos=3)

mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("B. Stage-Specific Life Expectancy",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("C. Overall Life Expectancy",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("D. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=7/8,cex=1)
mtext("Scenario 2",side=2,line=0,outer=TRUE,at=1/4,cex=1)
mtext("Scenario 1",side=2,line=0,outer=TRUE,at=3/4,cex=1)
dev.off()

