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
plot.fxn(results[[12]],"Cervix",0.35,0.35)
plot.fxn(results[[13]],"Uterus",0.5,0.4)
plot.fxn(results[[14]],"Ovary",0.5,0.5)
plot.prostate.fxn(results[[15]],"Prostate",0.25,0.25)
plot.fxn(results[[2]],"Lung",0.2,0.6)
plot.fxn(results[[5]],"Colorectal",0.4,0.6)
plot.fxn(results[[8]],"Esophagus",0.1,0.1)
plot.fxn(results[[11]],"Stomach",0.3,0.6)
plot.fxn(results[[14]],"Pancreas",0.2,0.2)
plot.fxn(results[[21]],"Bladder",0.4,0.6)
plot.fxn(results[[24]],"Kidney",0.6,0.6)
plot.fxn(results[[27]],"Melanoma",2,1.5)
plot.fxn(results[[30]],"Head & Neck",2,1.5)
plot.lymphoma.fxn(results[[33]],"Lymphoma",0.5,1)
dev.off()

#decomp1 <- lapply(results[-c(15)],function(x) c(sum(x[4:7]),sum(x[8:11])))
#decomp1.table <- matrix(c(unlist(decomp1)),ncol=24,nrow=2)
#decomp1.table <- cbind(decomp1.table,
#                       c(sum(results[[15]][4:6]),sum(results[[15]][7:9])))
#colnames(decomp1.table) <- c(names(results)[-c(15)],names(results)[15])

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


#pdf("~/Desktop/Cancer/figures/decomp1.results.pdf", height=8.5, width=11, paper="special")
#par (mfrow=c(2,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
#decomp1.plot.fxn(decomp1.table[,2:3])
#decomp1.plot.fxn(decomp1.table[,4:5])
#decomp1.plot.fxn(decomp1.table[,6:7])
#decomp1.plot.fxn(decomp1.table[,8:9])
#decomp1.plot.fxn(decomp1.table[,10:11])
#decomp1.plot.fxn(decomp1.table[,15:16])
#decomp1.plot.fxn(decomp1.table[,17:18])
#decomp1.plot.fxn(decomp1.table[,19:20])
#decomp1.plot.fxn(decomp1.table[,21:22])
#decomp1.plot.fxn(decomp1.table[,23:24])
#decomp1.plot.fxn2(decomp1.table[,1],"breast")
#decomp1.plot.fxn2(decomp1.table[,12],"cervix")
#decomp1.plot.fxn2(decomp1.table[,13],"uterus")
#decomp1.plot.fxn2(decomp1.table[,14],"ovary")
#decomp1.plot.fxn2(decomp1.table[,25],"prostate")
#dev.off()




keep <- c("breast","lung","crc","esophagus","stomach","pancreas","cervix","uterus","ovary","prostate","bladder","kidney",
          "melanoma","headneck","lymphoma")
total <- unlist(lapply(results, function(x) x[["ex.overall.diff"]]))

decomp.mat <- matrix(NA,nrow=length(keep),ncol=4)
colnames(decomp.mat) <- c("total gain","stage shift","mort, cancer","mort, other")
rownames(decomp.mat) <- keep
for (i in 1:length(keep)) {
    if (!(keep[i] %in% c("prostate","lymphoma","cervix")))
        decomp.mat[i,] <- c(results[[keep[i]]][["ex.overall.diff"]],
                            sum(results[[keep[i]]][c("change.stage.insitu","change.stage.localized","change.stage.regional","change.stage.distant")]),
                            sum(results[[keep[i]]][c("change.ex.insitu.cancer","change.ex.localized.cancer","change.ex.regional.cancer","change.ex.distant.cancer")]),
                            sum(results[[keep[i]]][c("change.ex.insitu.other","change.ex.localized.other","change.ex.regional.other","change.ex.distant.other")]))
    if (keep[i]=="prostate")
        decomp.mat[i,] <- c(results[[keep[i]]][["ex.overall.diff"]],
                            sum(results[[keep[i]]][c("change.stage.insitu","change.stage.localized.regional","change.stage.distant")]),
                            sum(results[[keep[i]]][c("change.ex.insitu.cancer","change.ex.localized.regional.cancer","change.ex.distant.cancer")]),
                            sum(results[[keep[i]]][c("change.ex.insitu.other","change.ex.localized.regional.other","change.ex.distant.other")]))
    if (keep[i]=="lymphoma")
       decomp.mat[i,] <- c(results[[keep[i]]][["ex.overall.diff"]],
                            sum(results[[keep[i]]][c("change.stage.stageI","change.stage.stageII","change.stage.stageIII","change.stage.stageIV")]),
                            sum(results[[keep[i]]][c("change.ex.stageI.cancer","change.ex.stageII.cancer","change.ex.stageIII.cancer","change.ex.stageIV.cancer")]),
                            sum(results[[keep[i]]][c("change.ex.stageI.other","change.ex.stageII.other","change.ex.stageIII.other","change.ex.stageIV.other")]))
    if (keep[i]=="cervix")
        decomp.mat[i,] <- c(results[[keep[i]]][["ex.overall.diff"]],
                            sum(results[[keep[i]]][c("change.stage.localized","change.stage.regional","change.stage.distant")]),
                            sum(results[[keep[i]]][c("change.ex.localized.cancer","change.ex.regional.cancer","change.ex.distant.cancer")]),
                            sum(results[[keep[i]]][c("change.ex.localized.other","change.ex.regional.other","change.ex.distant.other")]))
 
}
decomp.mat <- decomp.mat[order(decomp.mat[,1]),]
xmin <- min(decomp.mat[,1])
xmax <- max(decomp.mat[,1])
ypos <- barplot(t(decomp.mat[,1]),horiz=TRUE,plot=FALSE)
pdf("~/Desktop/Cancer/figures/decomp.results2.pdf", height=5.5, width=11, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
for (i in 1:4) {
    if (i==1) barplot(t(decomp.mat[,1]),horiz=TRUE,border=FALSE,names.arg=rownames(decomp.mat),las=1,xaxt="n",xlim=c(xmin,xmax))
     if (i>1) barplot(t(decomp.mat[,i]),horiz=TRUE,border=FALSE,yaxt="n",las=1,xaxt="n",xlim=c(xmin,xmax))
    axis(1,at=seq(floor(xmin),ceiling(xmax),1),cex=0.75)
      abline(v=0,col="grey")
    abline(h=ypos,col="grey",lty=2)
    if (i==4) text(xmax,ypos,rownames(decomp.mat),pos=2)
}     
mtext("Years",side=1,line=0,outer=TRUE,at=1/8,cex=1)
mtext("Years",side=1,line=0,outer=TRUE,at=c(3,5,7)/8,cex=1)
mtext("Total Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("Stage Shift",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("Mortality, Cancer",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("Mortality, Competing Causes",side=3,line=0,outer=TRUE,at=7/8,cex=1)
dev.off()


b <- apply(breast,1,function(x) c(x[3],sum(x[4:7]),sum(x[c(12,14,16,18)]),sum(x[c(13,15,17,19)])))
rownames(b) <- c("total gain in life exp","stage shift","mortality, cancer", "mortality, other")
colnames(b) <- c("1973-1981","1981-1991","1991-2001")
pdf("~/Desktop/Cancer/figures/breast.pdf", height=5.5, width=5.5, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.66,cex.main=0.66)
barplot(b[2:4,],beside=FALSE,border=FALSE,col=c("red","light blue","dark blue"),las=1,xaxt="n")
legend("topright",c("stage shift","mortality, cancer","mortality, other"),col=c("red","light blue","dark blue"),pch=15,text.col=c("red","light blue","dark blue"),bty="n")
axis(1,at=barplot(b[2:3,],beside=FALSE,plot=FALSE),paste(c("1973 vs 1981 cohort","1981 vs 1991 cohort","1991 vs 2001 cohort")))
dev.off()

b <- apply(breast,1,function(x) c(x[3],sum(x[4:7]),sum(x[c(12,14,16,18)]),sum(x[c(13,15,17,19)]),
                                  x[12],x[13],x[14],x[15],x[16],x[17],x[18],x[19]))
