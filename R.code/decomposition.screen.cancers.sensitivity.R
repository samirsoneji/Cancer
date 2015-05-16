rm(list=ls())
load("~/Desktop/Cancer/data/mx.breast.size.Rdata")
load("~/Desktop/Cancer/data/mx.prostate.grade.Rdata")

prostate <- read.csv("~/Desktop/Cancer/data/prostate.grade.txt",sep=";",header=FALSE)
names(prostate) <- c("grade","year.dx","rate","count","popl")
#adjusted to year 2000 age distribution, rate per 100000
prostate <- subset(prostate,year.dx>0)
prostate$year.dx <- prostate$year.dx+1972
prostate$grade[prostate$grade==0] <- "grade I"
prostate$grade[prostate$grade==1] <- "grade II"
prostate$grade[prostate$grade==2] <- "grade III"
prostate$grade[prostate$grade==3] <- "grade IV"
prostate$grade[prostate$grade %in% 4:8] <- NA
prostate.grade.array <- by(prostate$rate,list(prostate$year.dx,prostate$grade), function(x) x)

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.sens.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.sens.fxn.R")
source("~/Desktop/Cancer/R.code/results.sens.fxn.R")

summary.fxn2 <- function(x)
    c(x[3],sum(x[4:5]),x[8],x[10],sum(x[seq(9,11,2)]))
summary.fxn3 <- function(x)
    c(x[3],sum(x[4:7]),x[12],x[14],x[16],x[18],sum(x[seq(13,19,2)]))

breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(1975,2002))
prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1975,2002))
summary.breast <- summary.fxn3(breast)
summary.prostate <- summary.fxn2(prostate)
names(summary.breast) <-  c("gain in life expectancy","size","mort, <1cm", "mort, 1-2cm", "mort, 2-5cm", "mort, 5+cm", "mort, other")
names(summary.prostate) <-  c("gain in life expectancy","grade","mort, grade I-II", "mort, grade III", "mort, other")


library(RColorBrewer)
color <- brewer.pal(9,"YlGnBu")[c(3,5,7,9)]
ymin <- -5
ymax <- max(summary.breast[[1]],summary.breast[[2]],sum(unlist(summary.breast[3:6])))
popl <- read.table("~/Desktop/Cancer/data/Population5.txt",skip=2,header=TRUE)
stand.female <- prop.table(c(subset(popl,Year==2000)$Female[10:21],sum(subset(popl,Year==2000)$Female[22:24])))
stand.male <- prop.table(c(subset(popl,Year==2000)$Male[10:21],sum(subset(popl,Year==2000)$Male[22:24])))
stand.all <- prop.table(c(subset(popl,Year==2000)$Total[10:21],sum(subset(popl,Year==2000)$Total[22:24])))

pdf("~/Desktop/Cancer/figures/decomp_breast1.pdf", height=4, width=8.5, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
year <- 1975:2002

scale <- 100000
matplot(year,scale*stand.size.rate[as.character(year),],col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l")
axis(1,at=seq(1975,2002,9))
axis(2,las=1,at=seq(0,600,100))
legend("bottomright",cex=0.7,ncol=2,paste(c("<1cm","1-2cm","2-5cm","5+cm")),lty=1,col=color,text.col=color,bty="n")

plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
polygon(c(year,rev(year)),
        100*c(rep(0,length(year)),rev(prop.breast[as.character(year),1])),
        col=color[1],border=FALSE)
polygon(c(year,rev(year)),
        100*c(prop.breast[as.character(year),1],rev(apply(prop.breast[as.character(year),1:2],1,sum))),
        col=color[2],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.breast[as.character(year),1:2],1,sum),rev(apply(prop.breast[as.character(year),1:3],1,sum))),
        col=color[3],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.breast[as.character(year),1:3],1,sum),rev(apply(prop.breast[as.character(year),1:4],1,sum))),
        col=color[4],border=FALSE)
axis(1,at=seq(1975,2002,9))
axis(2,at=seq(0,100,10),las=1)
text(1988,100*(prop.breast["1988",1]/2),"<1cm",cex=1)
text(1988,100*(sum(prop.breast["1988",1])+ prop.breast["1988",2]/2),"1-2cm",cex=1)
text(1988,100*(sum(prop.breast["1988",1:2])+ prop.breast["1988",3]/2),"2-5cm",cex=1)
text(1988,100*(sum(prop.breast["1988",1:3])+ prop.breast["1988",4]/2),"5+cm",cex=1,col="white")

mx.breast.cause <- apply(mx.breast.cause, c(1,2,3,4), function(x) min(12,x))
stand.mx <- apply(mx.breast.cause,c(1,3,4),function(x) sum(x * stand.breast, na.rm=TRUE))
matplot(year,(stand.mx["breast",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="o",pch=19,cex=1,ylim=c(0,max(stand.mx[,as.character(year),])))
matlines(year,(stand.mx["other",as.character(year),]),col=color,lty=1)
axis(1,at=seq(1975,2002,9))
#axis(2,at=seq(-5,0,0.5),paste(round((10^seq(-5,0,0.5)),1)),las=1)
axis(2,las=1,at=seq(0,0.12,0.02))#,at=seq(-5,0,0.5),paste(round((10^seq(-5,0,0.5)),1)),las=1)
legend(1975,0.08,ncol=2,cex=0.6,paste(c("5+cm, Cancer","5+cm, Other","2-5cm, Cancer","2-5cm, Other","1-2cm, Cancer","1-2cm, Other","<1cm, Cancer","<1cm, Other")),lty=1,pch=c(19,NA),col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),text.col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),bty="n")


mtext("A. Incidence Rates (Per 100,000)",side=3,line=0,outer=TRUE,at=1/6,cex=0.8)
mtext("B. Grade Distribution (%)",side=3,line=0,outer=TRUE,at=3/6,cex=0.8)
mtext("C. Incidence-Based Mortality Rates",side=3,line=0,outer=TRUE,at=5/6,cex=0.8)
dev.off()

pdf("~/Desktop/Cancer/figures/decomp_breast2.pdf", height=5.5, width=5.5, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
year <- 1975:2002
xpos <- barplot(c(summary.breast[["gain in life expectancy"]],summary.breast[["size"]],NA),plot=FALSE)
barplot(c(summary.breast[["gain in life expectancy"]],NA,NA),las=1,axes=FALSE,ylim=c(-6,12),border=FALSE,col=c("black","darkgrey"))
barplot(matrix(c(rep(NA,2),unlist(breast[c(4,5)]),rep(NA,2)),ncol=3,nrow=2),add=TRUE,axes=FALSE,col=color[c(1,2)],border=color[c(1,2)])
barplot(matrix(c(rep(NA,2),unlist(breast[c(6,7)]),rep(NA,2)),ncol=3,nrow=2),add=TRUE,axes=FALSE,col=color[c(3,4)],border=color[c(3,4)])
barplot(matrix(c(rep(NA,10),unlist(summary.breast[c(3,4,5,6,7)])),ncol=3,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(-6,12,2),las=1)
axis(1,at=xpos,paste(c("Overall","Size","Mortality")),tick=FALSE)
abline(h=0,col="grey")
text(xpos[2],breast[[4]]/2,"<1cm",cex=0.9)
text(xpos[2],breast[[4]]+breast[[5]]/2,"1-2cm",cex=0.9)
text(xpos[2],breast[[6]]/2,"2-5cm",cex=0.9)
text(xpos[2],breast[[6]]+breast[[7]]/2,"5+cm",cex=0.9,col="white")
text(xpos[3],sum(unlist(summary.breast[3]))/2,"<1cm, Cancer",cex=0.9)
text(xpos[3],sum(unlist(summary.breast[3]))+summary.breast[[4]]/2,"1-2cm, Cancer",cex=0.9)
text(xpos[3],sum(unlist(summary.breast[3:4]))+summary.breast[[5]]/2,"2-5cm, Cancer",cex=0.9)
text(xpos[3],sum(unlist(summary.breast[3:5]))+summary.breast[[6]]/2,"5+cm, Cancer",cex=0.5,col="white")
text(xpos[3],sum(unlist(summary.breast[3:6]))+summary.breast[[7]]/2,"All Other Causes",cex=0.9)
mtext("Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=1/2,cex=0.9)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=0.9)
dev.off()


##############
###Prostate###
##############
color <- brewer.pal(7,"YlGnBu")[c(3,7)]
color2 <- brewer.pal(7,"YlGnBu")[c(3,7)]
ymin <- min(unlist(prostate[-c(1,2)]))
ymax <- max(summary.prostate[[1]],summary.prostate[[2]],sum(unlist(summary.prostate[3:5])))

pdf("~/Desktop/Cancer/figures/decomp_prostate1.pdf", height=4, width=8.5, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1975:2002
matplot(year,prostate.grade.array[as.character(year),1:3],col=color,lty=1,bty="l",type="l",xlab=NA,ylab=NA,axes=FALSE)
axis(1,at=seq(1975,2002,9))
axis(2,las=1)
legend("topleft",cex=0.8,paste(c("Grade I-II","Grade III")),lty=1,col=color,text.col=color,bty="n")

plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
polygon(c(year,rev(year)),
        100*c(rep(0,length(year)),rev(prop.prostate[as.character(year),1])),
        col=color[1],border=FALSE)
polygon(c(year,rev(year)),
        100*c(prop.prostate[as.character(year),1],rev(apply(prop.prostate[as.character(year),1:2],1,sum))),
        col=color[2],border=FALSE)
axis(1,at=seq(1975,2002,9))
axis(2,at=seq(0,100,10),las=1)
text(1988,100*(sum(prop.prostate["1988",1]))/2,"Grade I-II",cex=1)
text(1988,100*(sum(prop.prostate["1988",1])+ prop.prostate["1988",2]/2),"Grade III",cex=1)

mx.prostate.cause <- apply(mx.prostate.cause, c(1,2,3,4), function(x) ifelse(x==Inf,1,x))
mx.prostate.cause <- apply(mx.prostate.cause, c(1,2,3,4), function(x) ifelse(x>=1,1,x))
stand.mx <- apply(mx.prostate.cause,c(1,3,4),function(x) sum(x * stand.prostate, na.rm=TRUE))
matplot(year,(stand.mx["prostate",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="o",pch=19,cex=1)
matlines(year,(stand.mx["other",as.character(year),]),col=color,lty=1)
axis(1,at=seq(1975,2002,9))
axis(2,las=1)
legend("topright",ncol=1,cex=0.6,paste(c("Grade III, Cancer","Grade III, Other","Grade II, Cancer","Grade II, Other","Grade I, Cancer","Grade I, Other")),lty=1,pch=c(19,NA),col=c(color[3],color[3],color[2],color[2],color[1],color[1]),
       text.col=c(color[3],color[3],color[2],color[2],color[1],color[1]),bty="n")

matplot(1975:2002,100*prop.died.prostate.prostate[as.character(1975:2002),],pch=19,ylab="",xlab="",lty=1,type="l",bty="l",las=1,ylim=c(0,max(100*prop.died.prostate.prostate[as.character(1975:2002),])),col=color,lwd=1,axes=FALSE)
axis(1,at=seq(1975,2002,9))
axis(2,las=1)
legend("topright",cex=0.8,paste(c("Grade I","Grade II","Grade III")),lty=1,col=color,text.col=color,bty="n")

mtext("A. Incidence Rates (Per 100,000)",side=3,line=0,outer=TRUE,at=1/8,cex=0.8)
mtext("B. Grade Distribution (%)",side=3,line=0,outer=TRUE,at=3/8,cex=0.8)
mtext("C. Incidence-Based\nMortality Rates",side=3,line=0,outer=TRUE,at=5/8,cex=0.8)
mtext("D. Death From Prostate Cancer\nWithin 10-Yrs. (%)",side=3,line=0,outer=TRUE,at=7/8,cex=0.8)
dev.off()

pdf("~/Desktop/Cancer/figures/decomp_prostate2.pdf", height=5.5, width=5.5, paper="special")
par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1975:2002
xpos <- barplot(c(summary.prostate[["gain in life expectancy"]],summary.prostate[["grade"]],NA),plot=FALSE)
barplot(c(summary.prostate[["gain in life expectancy"]],NA,NA),las=1,axes=FALSE,ylim=c(-2,18),border=FALSE,col=c("black","darkgrey"))
barplot(matrix(c(rep(NA,1),unlist(prostate[c(4)]),rep(NA,1)),ncol=3,nrow=1),add=TRUE,axes=FALSE,col=color2[c(1,3)],border=color2[c(1,3)])
barplot(matrix(c(rep(NA,1),unlist(prostate[c(5)]),rep(NA,1)),ncol=3,nrow=1),add=TRUE,axes=FALSE,col=color2[2],border=color2[2])
#barplot(matrix(c(rep(NA,1),summary.prostate[[2]],rep(NA,1)),ncol=3,nrow=1),add=TRUE,axes=FALSE,col=1,border=1)
barplot(matrix(c(rep(NA,6),unlist(summary.prostate[c(3,4,5)])),ncol=3,nrow=3),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(-14,18,2),las=1)
axis(1,at=xpos,paste(c("Overall","Grade","Mortality")),tick=FALSE)
abline(h=0,col="grey")
text(xpos[2],prostate[[4]]/2,"Grade I",cex=0.9)
text(xpos[2],prostate[[5]]/2,"Grade II",cex=0.9)
text(xpos[2],prostate[[4]]+prostate[[6]]/2,"Grade III",cex=0.9,col="white")
text(xpos[3],sum(unlist(summary.prostate[3]))/2,"Grade I-II,Cancer",cex=0.9)
text(xpos[3],sum(unlist(summary.prostate[3]))+summary.prostate[[4]]/2,"Grade III, Cancer",cex=0.9)
text(xpos[3],sum(unlist(summary.prostate[3:4]))+summary.prostate[[5]]/2,"All Other Causes",cex=0.9)
mtext("Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=1/2,cex=0.9)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=0.9)
dev.off()


