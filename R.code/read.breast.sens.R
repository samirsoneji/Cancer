rm(list=ls())
setwd("~/Dropbox/Cancer/Value/")
library(plyr)
library(abind)

###############
#Breast Cancer#
###############
seer.breast <- read.fwf("data/SEER_data/SEER_1973_2012_TEXTDATA/incidence/yr1973_2012.seer9/BREAST.txt",widths=348)
age.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,39,42)))
coding.system <- as.numeric(apply(seer.breast, 1, function(x) substr(x,92,92)))
size.eod2 <- apply(seer.breast, 1, function(x) substr(x,86,87))
size.eod4 <- as.numeric(apply(seer.breast, 1, function(x) substr(x,88,89)))
size.eod13.clinical <- apply(seer.breast, 1, function(x) substr(x,73,73))
size.eod13.path.op <- apply(seer.breast, 1, function(x) substr(x,74,74))
size.eod10 <- as.numeric(apply(seer.breast, 1, function(x) substr(x,61,63)))
size.cs <- as.numeric(apply(seer.breast, 1, function(x) substr(x,96,98)))
stage <- as.numeric(apply(seer.breast, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.breast, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.breast, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.breast, 1, function(x) substr(x,255,259)))
data.breast <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,stage=stage,
                                coding.system=coding.system,size.eod2=size.eod2,size.eod4=size.eod4,
                                size.eod13.clinical=size.eod13.clinical,size.eod13.path.op=size.eod13.path.op,
                                size.eod10=size.eod10,size.cs=size.cs,vital.status=vital.status,surv.months=surv.months,cod=cod))
data.breast$surv.months <- as.numeric(as.character(data.breast$surv.months))
data.breast$age.dx <- as.numeric(as.character(data.breast$age.dx))
data.breast$size.eod4 <- as.numeric(as.character(data.breast$size.eod4))
data.breast$size.eod10 <- as.numeric(as.character(data.breast$size.eod10))
data.breast$cod <- as.numeric(as.character(data.breast$cod))
drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$age.dx < 40 | data.breast$age.dx==999) #drop in situ (4/8/15)
breast <- data.breast[-drop,]
breast$dead[breast$vital.status==1] <- 0
breast$dead[breast$vital.status==4] <- 1
breast$dead[breast$surv.months >= 120] <- 0
breast$surv.months <- ifelse(breast$surv.months<=120,breast$surv.months,120)
breast$age.dx.cat <- 5*floor(breast$age.dx/5)
breast$age.dx.cat[breast$age.dx.cat>=100] <- 100
breast$age.dx.cat2 <- as.factor(breast$age.dx.cat)
levels(breast$age.dx.cat2) <- c(rep("40-49",2),rep("50-59",2),rep("60-69",2),rep("70-79",2),rep("80-89",2),rep("90-99",2),rep("100+"))
levels(breast$stage) <- c("0. in situ","1. localized","2. regional","4. distant",NA)
breast$size[breast$size.eod13.path.op %in% c("1","2")] <- "<1cm"
breast$size[breast$size.eod13.path.op %in% c("3")] <- "1-2cm"
breast$size[breast$size.eod13.path.op %in% c("4")] <- "2-3cm"
breast$size[breast$size.eod13.path.op %in% c("5","6")] <- "3-5cm"
breast$size[breast$size.eod13.path.op %in% c("7","8")] <- "5+cm"
breast$size[breast$size.eod4 %in% 3:9] <- "<1cm"
breast$size[breast$size.eod4 %in% 10:19] <- "1-2cm"
breast$size[breast$size.eod4 %in% 20:29] <- "2-3cm"
breast$size[breast$size.eod4 %in% 30:49] <- "3-5cm"
breast$size[breast$size.eod4 %in% 50:97] <- "5+cm"
breast$size[breast$size.eod10 %in% 3:9] <- "<1cm"
breast$size[breast$size.eod10 %in% 10:19] <- "1-2cm"
breast$size[breast$size.eod10 %in% 20:29] <- "2-3cm"
breast$size[breast$size.eod10 %in% 30:49] <- "3-5cm"
breast$size[breast$size.eod10 %in% 50:990] <- "5+cm"
breast$size[breast$size.cs %in% c(1:9,991)] <- "<1cm"
breast$size[breast$size.cs %in% c(10:19,992)] <- "1-2cm"
breast$size[breast$size.cs %in% c(20:29,993)] <- "2-3cm"
breast$size[breast$size.cs %in% c(30:49,994:995)] <- "3-5cm"
breast$size[breast$size.cs %in% c(51:998)] <- "5+cm"
breast$cod2[breast$cod==26000] <- "breast"
breast$cod2[breast$cod>=20010 & breast$cod<=50300 & breast$cod!=26000] <- "other"
breast$cod3 <- "alive"
breast$cod3[breast$cod==26000] <- "breast"
breast$cod3[breast$cod>=20010 & breast$cod<=50300 & breast$cod!=26000] <- "other"

number.breast <- nrow(breast)

dead <- by(breast$dead, list(breast$age.dx, breast$year.dx, breast$size), sum)
dead.cause <- by(breast$dead, list(breast$age.dx, breast$year.dx, breast$size, breast$cod2), sum)
exposure <- table(breast$age.dx, breast$year.dx, breast$size)
mx.breast <- dead/exposure
mx.breast.cause <- aaply(dead.cause, 4, function(x) x/exposure)

dead.all.sizes <- apply(dead,c(1,2),sum,na.rm=TRUE)
dead.cause.all.sizes <- apply(dead.cause,c(1,2,4),sum,na.rm=TRUE)
exposure.all.sizes <- apply(exposure,c(1,2),sum,na.rm=TRUE)

mx.breast.all.sizes <- dead.all.sizes/exposure.all.sizes
mx.breast.all.sizes <- mx.breast.all.sizes[,as.character(1975:2002)]


