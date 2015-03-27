rm(list=ls())

load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 

load("~/Desktop/Cancer/data/mx.crc.Rdata")
load("~/Desktop/Cancer/data/prop.crc.Rdata")
prop.crc <- t(prop.crc)
load("~/Desktop/Cancer/data/mx.crc.female.Rdata")
load("~/Desktop/Cancer/data/prop.crc.female.Rdata") 
prop.crc.female <- t(prop.crc.female)
load("~/Desktop/Cancer/data/mx.crc.male.Rdata")
load("~/Desktop/Cancer/data/prop.crc.male.Rdata") 
prop.crc.male <- t(prop.crc.male)

load("~/Desktop/Cancer/data/mx.cervix.Rdata")
load("~/Desktop/Cancer/data/prop.cervix.Rdata") 

load("~/Desktop/Cancer/data/mx.prostate.Rdata")
load("~/Desktop/Cancer/data/prop.prostate.Rdata") 

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.fxn.R")
source("~/Desktop/Cancer/R.code/results.fxn.R")

year0 <- 1973
year1 <- 2001

breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(year0,year1))
crc <- results.fxn(mx.crc, mx.crc.cause, prop.crc, "crc", c(year0,year1))
crc.female <- results.fxn(mx.crc.female, mx.crc.female.cause, prop.crc.female, "crc", c(year0,year1))
crc.male <- results.fxn(mx.crc.male, mx.crc.male.cause, prop.crc.male, "crc", c(year0,year1))
cervix <- results.fxn(mx.cervix, mx.cervix.cause, prop.cervix, "cervix", c(year0,year1))
prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1995,year1))

summary.fxn <- function(x)
    c(x[3],sum(x[4:7]),x[12],x[14],x[16],x[18],sum(x[seq(13,19,2)]))

summary.fxn2 <- function(x)
    c(x[3],sum(x[4:6]),x[10],x[12],x[14],sum(x[seq(11,15,2)]))

summary.breast <- summary.fxn(breast)
summary.crc <- summary.fxn(crc)
summary.prostate <- summary.fxn2(prostate)
summary.cervix <- summary.fxn2(cervix)

names(summary.breast) <- names(summary.crc) <-
    c("gain in life expectancy","stage","mort, in situ","mort, loc", "mort, reg", "mort, dis", "mort, other")
names(summary.prostate) <-  c("gain in life expectancy","stage","mort, in situ","mort, locreg", "mort, dis", "mort, other")
names(summary.cervix) <- 
    c("gain in life expectancy","stage","mort, loc", "mort, reg", "mort, dis", "mort, other")


library(RColorBrewer)
color <- brewer.pal(6,"YlGnBu")[-c(1,2)]
ymax <- max(summary.breast[[2]],sum(unlist(summary.breast[3:7])))
popl <- read.table("~/Desktop/Cancer/data/Population5.txt",skip=2,header=TRUE)
stand.female <- prop.table(c(subset(popl,Year==2000)$Female[10:21],sum(subset(popl,Year==2000)$Female[22:24])))
stand.male <- prop.table(c(subset(popl,Year==2000)$Male[10:21],sum(subset(popl,Year==2000)$Male[22:24])))
stand.all <- prop.table(c(subset(popl,Year==2000)$Total[10:21],sum(subset(popl,Year==2000)$Total[22:24])))

pdf("~/Desktop/Cancer/figures/decomp_breast.pdf", height=4.5, width=8.5, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1973:2001
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
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(0,100,10),las=1)
text(1987,100*prop.breast["1987",1]/2,"In Situ",cex=0.66)
text(1987,100*(prop.breast["1987",1]+ prop.breast["1987",2]/2),"Localized",cex=0.66)
text(1987,100*(sum(prop.breast["1987",1:2])+ prop.breast["1987",3]/2),"Regional",cex=0.66)
text(1987,100*(sum(prop.breast["1987",1:3])+ prop.breast["1987",4]/2),"Distant",cex=0.66)

stand.mx <- apply(mx.breast.cause,c(1,3,4),function(x) sum(x * stand.female, na.rm=TRUE))
matplot(year,log10(stand.mx["breast",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l")
matlines(year,log10(stand.mx["other",as.character(year),]),col=color,lty=2)
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(-5,0,0.5),paste(round(100*(10^seq(-5,0,0.5)),1)),las=1)
legend(1973,log10(0.2),ncol=2,cex=0.5,paste(c("Distant, Cancer","Distant, Other","Regional, Cancer","Regional, Other","Localized, Cancer","Localized, Other","In Situ, Cancer","In Situ, Other")),
       lty=1:2,col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),
       text.col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),bty="n")

xpos <- barplot(c(summary.breast[["stage"]],NA),plot=FALSE)
barplot(c(summary.breast[["stage"]],NA),las=1,axes=FALSE,ylim=c(0,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,5),unlist(summary.breast[3:7])),ncol=2,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(0,ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.breast[[3]]/2,"In Situ, Cancer",cex=0.66)
text(xpos[2],summary.breast[[3]]+summary.breast[[4]]/2,"Localized, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.breast[3:4]))+summary.breast[[5]]/2,"Regional, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.breast[3:5]))+summary.breast[[6]]/2,"Distant, Cancer",cex=0.35,col="white")
text(xpos[2],sum(unlist(summary.breast[3:6]))+summary.breast[[7]]/2,"All Other Causes",cex=0.66)
   
mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/6,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/6,cex=1)
mtext("C. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=5/6,cex=1)
dev.off()


pdf("~/Desktop/Cancer/figures/decomp_cervix.pdf", height=4.5, width=8.5, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1973:2001
plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
polygon(c(year,rev(year)),
        100*c(rep(0,length(year)),rev(prop.cervix[as.character(year),1])),
        col=color[2],border=FALSE)
polygon(c(year,rev(year)),
        100*c(prop.cervix[as.character(year),1],rev(apply(prop.cervix[as.character(year),1:2],1,sum))),
        col=color[3],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.cervix[as.character(year),1:2],1,sum),rev(apply(prop.cervix[as.character(year),1:3],1,sum))),
        col=color[4],border=FALSE)
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(0,100,10),las=1)
text(1987,100*(prop.cervix["1987",1]/2),"Localized",cex=0.66)
text(1987,100*(sum(prop.cervix["1987",1])+ prop.cervix["1987",2]/2),"Regional",cex=0.66)
text(1987,100*(sum(prop.cervix["1987",1:2])+ prop.cervix["1987",3]/2),"Distant",cex=0.66)

stand.mx <- apply(mx.cervix.cause,c(1,3,4),function(x) sum(x * stand.female, na.rm=TRUE))
matplot(year,log10(stand.mx["cervix",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l")
matlines(year,log10(stand.mx["other",as.character(year),]),col=color,lty=2)
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(-5,0,0.5),paste(round(100*(10^seq(-5,0,0.5)),1)),las=1)
legend(1973,log10(0.2),ncol=2,cex=0.5,paste(c("Distant, Cancer","Distant, Other","Regional, Cancer","Regional, Other","Localized, Cancer","Localized, Other")),
       lty=1:2,col=c(color[4],color[4],color[3],color[3],color[2],color[2]),
       text.col=c(color[4],color[4],color[3],color[3],color[2],color[2]),bty="n")

xpos <- barplot(c(summary.cervix[["stage"]],NA),plot=FALSE)
barplot(c(summary.cervix[["stage"]],NA),las=1,axes=FALSE,ylim=c(0,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,4),unlist(summary.cervix[3:6])),ncol=2,nrow=4),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(0,ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.cervix[[3]]/2,"Localized, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.cervix[3]))+summary.cervix[[4]]/2,"Regional, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.cervix[3:4]))+summary.cervix[[5]]/2,"Distant, Cancer",cex=0.35,col="white")
text(xpos[2],sum(unlist(summary.cervix[3:5]))+summary.cervix[[6]]/2,"All Other Causes",cex=0.66)
    
mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/6,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/6,cex=1)
mtext("C. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=5/6,cex=1)
dev.off()


pdf("~/Desktop/Cancer/figures/decomp_crc.pdf", height=4.5, width=8.5, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1973:2001
plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
polygon(c(year,rev(year)),
        100*c(rep(0,length(year)),rev(prop.crc[as.character(year),1])),
        col=color[1],border=FALSE)
polygon(c(year,rev(year)),
        100*c(prop.crc[as.character(year),1],rev(apply(prop.crc[as.character(year),1:2],1,sum))),
        col=color[2],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.crc[as.character(year),1:2],1,sum),rev(apply(prop.crc[as.character(year),1:3],1,sum))),
        col=color[3],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.crc[as.character(year),1:3],1,sum),rev(apply(prop.crc[as.character(year),1:4],1,sum))),
        col=color[4],border=FALSE)
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(0,100,10),las=1)
text(1987,100*prop.crc["1987",1]/2,"In Situ",cex=0.66)
text(1987,100*(prop.crc["1987",1]+ prop.crc["1987",2]/2),"Localized",cex=0.66)
text(1987,100*(sum(prop.crc["1987",1:2])+ prop.crc["1987",3]/2),"Regional",cex=0.66)
text(1987,100*(sum(prop.crc["1987",1:3])+ prop.crc["1987",4]/2),"Distant",cex=0.66)

stand.mx <- apply(mx.crc.cause,c(1,3,4),function(x) sum(x * stand.female, na.rm=TRUE))
matplot(year,log10(stand.mx["crc",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l")
matlines(year,log10(stand.mx["other",as.character(year),]),col=color,lty=2)
axis(1,at=c(1973,1981,1991,2001))
axis(2,at=seq(-5,0,0.5),paste(round(100*(10^seq(-5,0,0.5)),1)),las=1)
legend(1973,log10(0.2),ncol=2,cex=0.5,paste(c("Distant, Cancer","Distant, Other","Regional, Cancer","Regional, Other","Localized, Cancer","Localized, Other","In Situ, Cancer","In Situ, Other")),
       lty=1:2,col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),
       text.col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),bty="n")

xpos <- barplot(c(summary.crc[["stage"]],NA),plot=FALSE)
barplot(c(summary.crc[["stage"]],NA),las=1,axes=FALSE,ylim=c(0,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,5),unlist(summary.crc[3:7])),ncol=2,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(0,ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.crc[[3]]/2,"In Situ, Cancer",cex=0.66)
text(xpos[2],summary.crc[[3]]+summary.crc[[4]]/2,"Localized, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.crc[3:4]))+summary.crc[[5]]/2,"Regional, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.crc[3:5]))+summary.crc[[6]]/2,"Distant, Cancer",cex=0.35,col="white")
text(xpos[2],sum(unlist(summary.crc[3:6]))+summary.crc[[7]]/2,"All Other Causes",cex=0.66)
   
mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/6,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/6,cex=1)
mtext("C. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=5/6,cex=1)
dev.off()

pdf("~/Desktop/Cancer/figures/decomp_prostate.pdf", height=4.5, width=8.5, paper="special")
par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

year <- 1995:2001
plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
polygon(c(year,rev(year)),
        100*c(rep(0,length(year)),rev(prop.prostate[as.character(year),1])),
        col=color[2],border=FALSE)
polygon(c(year,rev(year)),
        100*c(prop.prostate[as.character(year),1],rev(apply(prop.prostate[as.character(year),1:2],1,sum))),
        col=color[3],border=FALSE)
polygon(c(year,rev(year)),
        100*c(apply(prop.prostate[as.character(year),1:2],1,sum),rev(apply(prop.prostate[as.character(year),1:3],1,sum))),
        col=color[4],border=FALSE)
axis(1,at=c(1995:2001))
axis(2,at=seq(0,100,10),las=1)
text(1997,100*(prop.prostate["1997",1]/2),"In Situ",cex=0.66)
text(1997,100*(sum(prop.prostate["1997",1])+ prop.prostate["1997",2]/2),"Localized-Regional",cex=0.66)
text(1997,100*(sum(prop.prostate["1997",1:2])+ prop.prostate["1997",3]/2),"Distant",cex=0.66)

stand.mx <- apply(mx.prostate.cause,c(1,3,4),function(x) sum(x * stand.female, na.rm=TRUE))
matplot(year,log10(stand.mx["prostate",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l")
matlines(year,log10(stand.mx["other",as.character(year),]),col=color,lty=2)
axis(1,at=c(1995:2001))
axis(2,at=seq(-5,0,0.5),paste(round(100*(10^seq(-5,0,0.5)),1)),las=1)
legend(1995,log10(0.2),ncol=2,cex=0.5,paste(c("Distant, Cancer","Distant, Other","Localized-Regional, Cancer","Localized-Regional, Other","In Situ, Cancer","In Situ, Other")),
       lty=1:2,col=c(color[4],color[4],color[3],color[3],color[2],color[2]),
       text.col=c(color[4],color[4],color[3],color[3],color[2],color[2]),bty="n")

xpos <- barplot(c(summary.prostate[["stage"]],NA),plot=FALSE)
barplot(c(summary.prostate[["stage"]],NA),las=1,axes=FALSE,ylim=c(0,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,4),unlist(summary.prostate[3:6])),ncol=2,nrow=4),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(0,ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.prostate[[3]]/2,"In Situ, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.prostate[3]))+summary.prostate[[4]]/2,"Localized-Regional, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.prostate[3:4]))+summary.prostate[[5]]/2,"Distant, Cancer",cex=0.35,col="white")
text(xpos[2],sum(unlist(summary.prostate[3:5]))+summary.prostate[[6]]/2,"All Other Causes",cex=0.66)
    
mtext("A. Stage Distribution",side=3,line=0,outer=TRUE,at=1/6,cex=1)
mtext("B. Stage-Specific Mortality Rates",side=3,line=0,outer=TRUE,at=3/6,cex=1)
mtext("C. Gain in Life Expectancy",side=3,line=0,outer=TRUE,at=5/6,cex=1)
dev.off()


pdf("~/Desktop/Cancer/figures/decomp_cancers.pdf", height=4.0, width=8.5, paper="special")
par (mfrow=c(1,4),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)

ymin <- min(0,min(summary.cervix[[2]],sum(unlist(summary.cervix[3:6]))),min(summary.prostate[[2]],sum(unlist(summary.prostate[3:6]))))
ymax <- max(max(summary.breast[[2]],sum(unlist(summary.breast[3:7]))), max(summary.cervix[[2]],sum(unlist(summary.cervix[3:6]))), max(summary.cervix[[2]],sum(unlist(summary.cervix[3:6]))),max(summary.prostate[[2]],sum(unlist(summary.prostate[3:6]))))

#breast cancer
xpos <- barplot(c(summary.breast[["stage"]],NA),plot=FALSE)
barplot(c(summary.breast[["stage"]],NA),las=1,axes=FALSE,ylim=c(ymin,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,5),unlist(summary.breast[3:7])),ncol=2,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(floor(ymin),ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.breast[[3]]/2,"In Situ, Cancer",cex=0.5)
text(xpos[2],summary.breast[[3]]+summary.breast[[4]]/2,"Localized,\nCancer",cex=0.66)
text(xpos[2],sum(unlist(summary.breast[3:4]))+summary.breast[[5]]/2,"Regional,\nCancer",cex=0.66)
text(xpos[2],sum(unlist(summary.breast[3:5]))+summary.breast[[6]]/2,"Distant, Cancer",cex=0.35)
text(xpos[2],sum(unlist(summary.breast[3:6]))+summary.breast[[7]]/2,"All Other\nCauses",cex=0.66)
abline(h=0,col="grey")

#cervical cancer
xpos <- barplot(c(summary.cervix[["stage"]],NA),plot=FALSE)
barplot(c(summary.cervix[["stage"]],NA),las=1,axes=FALSE,ylim=c(ymin,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,4),unlist(summary.cervix[3:6])),ncol=2,nrow=4),add=TRUE,axes=FALSE,col=c(color[-1],"darkgrey"),border=c(color[-1],"darkgrey"))
axis(2,at=seq(floor(ymin),ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.cervix[[3]]/2,"Localized,\nCancer",cex=0.66)
text(xpos[2],summary.cervix[[3]]+summary.cervix[[4]]/2,"Regional,\nCancer",cex=0.66)
text(xpos[2],sum(unlist(summary.cervix[3:4]))+summary.cervix[[5]]/2,"Distant, Cancer",cex=0.33)
text(xpos[2],sum(unlist(summary.cervix[3:5]))+summary.cervix[[6]]/2,"All Other\nCauses",cex=0.66)
abline(h=0,col="grey")

#crc cancer
xpos <- barplot(c(summary.crc[["stage"]],NA),plot=FALSE)
barplot(c(summary.crc[["stage"]],NA),las=1,axes=FALSE,ylim=c(ymin,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,5),unlist(summary.crc[3:7])),ncol=2,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"))
axis(2,at=seq(floor(ymin),ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
text(xpos[2],summary.crc[[3]]/2,"In Situ, Cancer",cex=0.33)
text(xpos[2],summary.crc[[3]]+summary.crc[[4]]/2,"Localized,\nCancer",cex=0.66)
text(xpos[2],sum(unlist(summary.crc[3:4]))+summary.crc[[5]]/2,"Regional,\nCancer",cex=0.66)
text(xpos[2],sum(unlist(summary.crc[3:5]))+summary.crc[[6]]/2,"Distant, Cancer",cex=0.66)
text(xpos[2],sum(unlist(summary.crc[3:6]))+summary.crc[[7]]/2,"All Other\nCauses",cex=0.66)
abline(h=0,col="grey")

#prostate cancer
xpos <- barplot(c(summary.prostate[["stage"]],NA),plot=FALSE)
barplot(c(summary.prostate[["stage"]],NA),las=1,axes=FALSE,ylim=c(ymin,ymax),border=FALSE,col="black")
barplot(matrix(c(rep(NA,4),unlist(summary.prostate[3:6])),ncol=2,nrow=4),add=TRUE,axes=FALSE,col=c(color[-1],"darkgrey"),border=c(color[-1],"darkgrey"))
axis(2,at=seq(floor(ymin),ceiling(ymax)),las=1)
axis(1,at=xpos,paste(c("Stage","Mortality")),tick=FALSE)
#text(xpos[2],summary.prostate[[3]]/2,"In Situ,\nCancer",cex=0.66)
text(xpos[2],summary.prostate[[3]]+summary.prostate[[4]]/2,"Localized-Regional,\n Cancer",cex=0.4)
#text(xpos[2],sum(unlist(summary.prostate[3:4]))+summary.prostate[[5]]/2,"Distant,\nCancer",cex=0.4)
text(xpos[2],sum(unlist(summary.prostate[3:5]))+summary.prostate[[6]]/2,"All Other\nCauses",cex=0.66)
abline(h=0,col="grey")

mtext("Breast Cancer",side=3,line=0,outer=TRUE,at=1/8,cex=1)
mtext("Cervical Cancer",side=3,line=0,outer=TRUE,at=3/8,cex=1)
mtext("Colorectal Cancer",side=3,line=0,outer=TRUE,at=5/8,cex=1)
mtext("Prostate Cancer",side=3,line=0,outer=TRUE,at=7/8,cex=1)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=1)
dev.off()



breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", seq(1973,2001,4))
summary.breast <- apply(breast,1,summary.fxn)
rownames(summary.breast) <-
    c("gain in life expectancy","stage","mort, in situ","mort, loc", "mort, reg", "mort, dis", "mort, other")
colnames(summary.breast) <- seq(1973,2001,4)[-length(seq(1973,2001,4))]
summary.breast.data <- cbind(expand.grid(dimnames(summary.breast)),c(summary.breast))
names(summary.breast.data) <- c("variable","year","value")
summary.breast.data <- subset(summary.breast.data, variable != "gain in life expectancy")

dat1 <- subset(summary.breast.data, value  >=0)
dat2 <- subset(summary.breast.data, value  <0)
ggplot() +
    geom_bar(data=dat1, aes(x=year,y=value,fill=variable),stat="identity") +
    geom_bar(data=dat2, aes(x=year,y=value,fill=variable),stat="identity") +
    scale_fill_brewer(palette="Set1")


cervix <- results.fxn(mx.cervix, mx.cervix.cause, prop.cervix, "cervix", seq(1973,2001,4))
summary.cervix <- apply(cervix,1,summary.fxn2)
rownames(summary.cervix) <-
    c("gain in life expectancy","stage","mort, loc", "mort, reg", "mort, dis", "mort, other")
colnames(summary.cervix) <- seq(1973,2001,4)[-length(seq(1973,2001,4))]
summary.cervix.data <- cbind(expand.grid(dimnames(summary.cervix)),c(summary.cervix))
names(summary.cervix.data) <- c("variable","year","value")
summary.cervix.data <- subset(summary.cervix.data, variable != "gain in life expectancy")

dat1 <- subset(summary.cervix.data, value  >=0)
dat2 <- subset(summary.cervix.data, value  <0)
ggplot() +
    geom_bar(data=dat1, aes(x=year,y=value,fill=variable),stat="identity") +
    geom_bar(data=dat2, aes(x=year,y=value,fill=variable),stat="identity") +
    scale_fill_brewer(palette="Set1")


crc <- results.fxn(mx.crc, mx.crc.cause, prop.crc, "crc", seq(1973,2001,4))
summary.crc <- apply(crc,1,summary.fxn)
rownames(summary.crc) <-
    c("gain in life expectancy","stage","mort, in situ","mort, loc", "mort, reg", "mort, dis", "mort, other")
colnames(summary.crc) <- seq(1973,2001,4)[-length(seq(1973,2001,4))]
summary.crc.data <- cbind(expand.grid(dimnames(summary.crc)),c(summary.crc))
names(summary.crc.data) <- c("variable","year","value")
summary.crc.data <- subset(summary.crc.data, variable != "gain in life expectancy")

dat1 <- subset(summary.crc.data, value  >=0)
dat2 <- subset(summary.crc.data, value  <0)
ggplot() +
    geom_bar(data=dat1, aes(x=year,y=value,fill=variable),stat="identity") +
    geom_bar(data=dat2, aes(x=year,y=value,fill=variable),stat="identity") +
    scale_fill_brewer(palette="Set1")



prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1995,1998,2001))
summary.prostate <- apply(prostate,1,summary.fxn2)
rownames(summary.prostate) <-
    c("gain in life expectancy","stage","mort, in situ", "mort, loc-reg", "mort, dis", "mort, other")
colnames(summary.prostate) <- c(1995,1998,2001)[-3]
summary.prostate.data <- cbind(expand.grid(dimnames(summary.prostate)),c(summary.prostate))
names(summary.prostate.data) <- c("variable","year","value")
summary.prostate.data <- subset(summary.prostate.data, variable != "gain in life expectancy")

dat1 <- subset(summary.prostate.data, value  >=0)
dat2 <- subset(summary.prostate.data, value  <0)
ggplot() +
    geom_bar(data=dat1, aes(x=year,y=value,fill=variable),stat="identity") +
    geom_bar(data=dat2, aes(x=year,y=value,fill=variable),stat="identity") +
    scale_fill_brewer(palette="Set1")
