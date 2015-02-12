library(RColorBrewer)
load("~/Desktop/Cancer/results/results.Rdata")

color <- brewer.pal(9,"RdBu")

plot.fxn <- function(res,cancer,ymin.adj,ymax.adj) {
tmp <- matrix(0,nrow=2,ncol=5)
tmp[,1] <- c(sum(res[4:7]),sum(res[8:11]))
tmp[,2] <- c(res[[12]],res[[13]])
tmp[,3] <- c(res[[14]],res[[15]])
tmp[,4] <- c(res[[16]],res[[17]])
tmp[,5] <- c(res[[18]],res[[19]])
colnames(tmp) <- c("Total","In Situ","Localized","Regional","Distant")
ymin <- min(tmp)-ymin.adj
ymax <- max(tmp)+ymax.adj
space.values <- c(0,0.04,1.25,0.04,0.5,0.04,0.5,0.04,0.5,0.04)
barplot(tmp,las=1,beside=TRUE,col=c(color[1],color[8],rep(c(color[9],color[7]),5)),ylim=c(ymin,ymax),
        space=space.values,main=cancer,yaxt="n",cex.names=0.9,border=c(color[1],color[8],rep(c(color[9],color[7]),5)))
xpos <- barplot(tmp,beside=TRUE,plot=FALSE,space=space.values)
text(xpos[1,1],tmp[1,1]/2,paste("Stage Shift"),col="white",srt=90,cex=0.9)
text(xpos[2,1],tmp[2,1]/2,paste("Mortality"),col="white",srt=90,cex=0.9)
for (i in c(1:5)) {
     text(xpos[1,i],tmp[1,i],paste(round(tmp[1,i],2)),pos=ifelse(tmp[1,i]>0,3,1),cex=0.75)
     text(xpos[2,i],tmp[2,i],paste(round(tmp[2,i],2)),pos=ifelse(tmp[2,i]>0,3,1),cex=0.75)
}
abline(h=0,col="grey")
}

plot.lymphoma.fxn <- function(res,cancer,ymin.adj,ymax.adj) {
tmp <- matrix(0,nrow=2,ncol=5)
tmp[,1] <- c(sum(res[4:7]),sum(res[8:11]))
tmp[,2] <- c(res[[12]],res[[13]])
tmp[,3] <- c(res[[14]],res[[15]])
tmp[,4] <- c(res[[16]],res[[17]])
tmp[,5] <- c(res[[18]],res[[19]])
colnames(tmp) <- c("Total","Stage 1","Stage II","Stage III","Stage IV")
ymin <- min(tmp)-ymin.adj
ymax <- max(tmp)+ymax.adj
space.values <- c(0,0.04,1.25,0.04,0.5,0.04,0.5,0.04,0.5,0.04)
barplot(tmp,las=1,beside=TRUE,col=c(color[1],color[8],rep(c(color[9],color[7]),5)),ylim=c(ymin,ymax),
        space=space.values,main=cancer,yaxt="n",cex.names=0.9,border=c(color[1],color[8],rep(c(color[9],color[7]),5)))
xpos <- barplot(tmp,beside=TRUE,plot=FALSE,space=space.values)
text(xpos[1,1],tmp[1,1]/2,paste("Stage Shift"),col="white",srt=90,cex=0.9)
text(xpos[2,1],tmp[2,1]/2,paste("Mortality"),col="white",srt=90,cex=0.9)
for (i in c(1:5)) {
     text(xpos[1,i],tmp[1,i],paste(round(tmp[1,i],2)),pos=ifelse(tmp[1,i]>0,3,1),cex=0.75)
     text(xpos[2,i],tmp[2,i],paste(round(tmp[2,i],2)),pos=ifelse(tmp[2,i]>0,3,1),cex=0.75)
}
abline(h=0,col="grey")
}

plot.prostate.fxn <- function(res,cancer,ymin.adj,ymax.adj) {
tmp <- matrix(0,nrow=2,ncol=4)
tmp[,1] <- c(sum(res[4:6]),sum(res[7:9]))
tmp[,2] <- c(res[[10]],res[[11]])
tmp[,3] <- c(res[[12]],res[[13]])
tmp[,4] <- c(res[[14]],res[[15]])
colnames(tmp) <- c("Total","In Situ","Localized-Regional","Distant")
ymin <- min(tmp)-ymin.adj
ymax <- max(tmp)+ymax.adj
space.values <- c(0,0.04,1.25,0.04,0.5,0.04,0.5,0.04)
barplot(tmp,las=1,beside=TRUE,col=c(color[1],color[8],rep(c(color[9],color[7]),4)),ylim=c(ymin,ymax),
        space=space.values,main=cancer,yaxt="n",cex.names=0.75,border=c(color[1],color[8],rep(c(color[9],color[7]),4)))
xpos <- barplot(tmp,beside=TRUE,plot=FALSE,space=space.values)
text(xpos[1,1],tmp[1,1]/2,paste("Stage Shift"),col="white",srt=90,cex=0.9)
text(xpos[2,1],tmp[2,1]/2,paste("Mortality"),col="white",srt=90,cex=0.9)
for (i in c(1:4)) {
     text(xpos[1,i],tmp[1,i],paste(round(tmp[1,i],2)),pos=ifelse(tmp[1,i]>0,3,1),cex=0.75)
     text(xpos[2,i],tmp[2,i],paste(round(tmp[2,i],2)),pos=ifelse(tmp[2,i]>0,3,1),cex=0.75)
}
abline(h=0,col="grey")
}

pdf("~/Desktop/Cancer/figures/decomp.results.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(2,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white", cex=1.0,cex.main=1)
plot.fxn(results[[1]],"Breast",0,0.6)
plot.fxn(results[[12]],"Cervix",2.5,2.5)
plot.fxn(results[[13]],"Uterus",0.5,0.6)
plot.fxn(results[[14]],"Ovary",0.5,0.6)
plot.fxn(results[[2]],"Lung, Female",0.2,0.6)
plot.fxn(results[[3]],"Lung, Male",0.2,0.6)
plot.fxn(results[[4]],"Colorectal, Female",0,0.6)
plot.fxn(results[[5]],"Colorectal, Male",0.4,0.6)
plot.fxn(results[[6]],"Esophagus, Female",0.3,0.6)
plot.fxn(results[[7]],"Esophagus, Male",0.1,0.1)
plot.fxn(results[[8]],"Stomach, Female",0.3,0.6)
plot.fxn(results[[9]],"Stomach, Male",0.2,0.2)
plot.fxn(results[[10]],"Pancreas, Female",0.3,0.6)
plot.fxn(results[[11]],"Pancreas, Male",0.1,0.15)
plot.fxn(results[[16]],"Bladder, Female",0.3,0.6)
plot.fxn(results[[17]],"Bladder, Male",0.5,0.5)
plot.fxn(results[[18]],"Kidney, Female",0.6,0.6)
plot.fxn(results[[19]],"Kidney, Male",0.5,0.5)
plot.fxn(results[[20]],"Melanoma, Female",2,1.5)
plot.fxn(results[[21]],"Melanoma, Male",1,1)
plot.fxn(results[[22]],"Head & Neck, Female",2,1.5)
plot.fxn(results[[23]],"Head & Neck, Male",1,1)
plot.prostate.fxn(results[[15]],"Prostate",0.25,0.25)
plot.lymphoma.fxn(results[[24]],"Lymphoma, Male",0.5,1)
plot.lymphoma.fxn(results[[25]],"Lymphoma, Female",0.5,0.5)
dev.off()


lapply(results[-15],function(x) c(sum(x[8:11]),
                             sum(x[12:19])))


decomp1 <- lapply(results[-c(15)],function(x) c(sum(x[4:7]),sum(x[8:11])))
decomp1.table <- matrix(c(unlist(decomp1)),ncol=24,nrow=2)
decomp1.table <- cbind(decomp1.table,
                       c(sum(results[[15]][4:6]),sum(results[[15]][7:9])))
colnames(decomp1.table) <- c(names(results)[-c(15)],names(results)[15])

decomp1.plot.fxn <- function(d) {
    barplot(d,beside=TRUE,las=1,col=rep(c(color[1],color[8]),2),space=c(0,0,1.25,0),border=FALSE)
    xpos <- barplot(d,beside=TRUE,space=c(0,0,1.25,0),plot=FALSE)
    for (i in 1:2) {
     text(xpos[1,i],d[1,i],paste(round(d[1,i],2)),pos=ifelse(d[1,i]>0,1,3),cex=0.75)
     text(xpos[2,i],d[2,i],paste(round(d[2,i],2)),pos=ifelse(d[2,i]>0,1,3),cex=0.75)
}
abline(h=0,col="grey")
}

decomp1.plot.fxn2 <- function(d,name) {
    barplot(d,beside=TRUE,space=c(0,0),border=FALSE,col=c(color[1],color[8]),las=1,main=name)
    xpos <- barplot(d,beside=TRUE,space=c(0,0),plot=FALSE)
    text(xpos[1,1],d[1],paste(round(d[1],2)),pos=ifelse(d[1]>0,1,3),cex=0.75)
    text(xpos[2,1],d[2],paste(round(d[2],2)),pos=ifelse(d[2]>0,1,3),cex=0.75)
abline(h=0,col="grey")
}


pdf("~/Desktop/Cancer/figures/decomp1.results.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(2,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
decomp1.plot.fxn(decomp1.table[,2:3])
decomp1.plot.fxn(decomp1.table[,4:5])
decomp1.plot.fxn(decomp1.table[,6:7])
decomp1.plot.fxn(decomp1.table[,8:9])
decomp1.plot.fxn(decomp1.table[,10:11])
decomp1.plot.fxn(decomp1.table[,15:16])
decomp1.plot.fxn(decomp1.table[,17:18])
decomp1.plot.fxn(decomp1.table[,19:20])
decomp1.plot.fxn(decomp1.table[,21:22])
decomp1.plot.fxn(decomp1.table[,23:24])
decomp1.plot.fxn2(decomp1.table[,1],"breast")
decomp1.plot.fxn2(decomp1.table[,12],"cervix")
decomp1.plot.fxn2(decomp1.table[,13],"uterus")
decomp1.plot.fxn2(decomp1.table[,14],"ovary")
decomp1.plot.fxn2(decomp1.table[,25],"prostate")
dev.off()





