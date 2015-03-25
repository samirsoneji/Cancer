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


ymax <- max(summary.breast[[2]],sum(unlist(summary.breast[3:7])))
barplot(c(summary.breast[["stage"]],NA),las=1,axes=FALSE,ylim=c(0,ymax),border=FALSE,col="darkgrey")
barplot(matrix(c(rep(NA,5),unlist(summary.breast[3:7])),ncol=2,nrow=5),add=TRUE,axes=FALSE)
axis(2,at=seq(0,ceiling(ymax)),las=1)
axis(1,at=barplot(c(summary.breast[["stage"]],NA),plot=FALSE),paste(c("Stage","Mortality")),tick=FALSE)
