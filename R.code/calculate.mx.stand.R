rm(list=ls())

popl <- read.table("~/Desktop/Cancer/data/Population5.txt",skip=2,header=TRUE)
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

stand.fxn <- function(mx,popl,sex,year.ref) {
  if (sex=="male") p <- subset(popl,Year==year.ref)$Male
  if (sex=="female") p <- subset(popl,Year==year.ref)$Female
  if (sex=="both") p <- subset(popl,Year==year.ref)$Total
  p <- c(p[10:21],sum(p[22:24]))
  prop <- p/sum(p)
  mx.stand <- apply(
