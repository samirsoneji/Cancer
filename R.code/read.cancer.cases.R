rm(list=ls())
setwd("~/Dropbox/Cancer/Value/")
library(plyr)
library(abind)

###############
#Breast Cancer#
###############
seer.breast <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/BREAST.txt",widths=348)
age.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,39,42)))
stage <- as.numeric(apply(seer.breast, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.breast, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.breast, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.breast, 1, function(x) substr(x,255,259)))
data.breast <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))
drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$age.dx < 40 | data.breast$age.dx==999)
breast <- data.breast[-drop,]
breast$dead[breast$vital.status==1] <- 0
breast$dead[breast$vital.status==4] <- 1
breast$dead[breast$surv.months >= 120] <- 0
breast$surv.months <- ifelse(breast$surv.months<=120,breast$surv.months,120)
breast$age.dx.cat <- 5*floor(breast$age.dx/5)
breast$age.dx.cat[breast$age.dx.cat>=100] <- 100
breast$stage[breast$stage==0] <- "0. in situ"
breast$stage[breast$stage==1] <- "1. localized"
breast$stage[breast$stage==2] <- "2. regional"
breast$stage[breast$stage==4] <- "4. distant"
breast$stage[breast$stage==4] <- "8. localized.regional"
breast$cod[breast$cod!=26000] <- "other"
breast$cod[breast$cod==26000] <- "breast"
dead <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
dead.cause <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage, breast$cod), sum)
exposure <- by(breast$surv.months/12, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
mx.breast <- dead/exposure
mx.breast.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.breast <- prop.table(as.table(table(breast$year.dx, breast$stage)),1)
save(mx.breast, mx.breast.cause, file="~/Cancer/data/mx.breast.Rdata")
save(prop.breast, file="~/Cancer/data/prop.breast.Rdata") 
print(paste("completed breast",date()))
 
###################
#Colorectal Cancer#
###################
seer.crc <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/COLRECT.txt",widths=348)
age.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.crc, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.crc, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.crc, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.crc, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.crc, 1, function(x) substr(x,255,259)))
data.crc <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,sex=sex,stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))
drop <- which(data.crc$surv.months==9999 | data.crc$stage==9 | data.crc$age.dx < 40 | data.crc$age.dx==999)
crc <- data.crc[-drop,]
crc$sex[crc$sex==1] <- "male"
crc$sex[crc$sex==2] <- "female"
crc$dead[crc$vital.status==1] <- 0
crc$dead[crc$vital.status==4] <- 1
crc$dead[crc$surv.months >= 120] <- 0
crc$surv.months <- ifelse(crc$surv.months<=120,crc$surv.months,120)
crc$age.dx.cat <- 5*floor(crc$age.dx/5)
crc$age.dx.cat[crc$age.dx.cat>=100] <- 100
crc$stage[crc$stage==0] <- "0. in situ"
crc$stage[crc$stage==1] <- "1. localized"
crc$stage[crc$stage==2] <- "2. regional"
crc$stage[crc$stage==4] <- "4. distant"
crc$cod[!(crc$cod %in% c(21040,21050))] <- "other"
crc$cod[crc$cod %in% c(21040,21050)] <- "crc"

crc.male <- subset(crc, sex=="male")
dead <- by(crc.male$dead, list(crc.male$age.dx.cat, crc.male$year.dx, crc.male$stage), sum)
dead.cause <- by(crc.male$dead, list(crc.male$age.dx.cat, crc.male$year.dx, crc.male$stage, crc.male$cod), sum)
exposure <- by(crc.male$surv.months/12, list(crc.male$age.dx.cat, crc.male$year.dx, crc.male$stage), sum)
mx.crc.male <- dead/exposure
mx.crc.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.crc.male <- apply(table(crc.male$year.dx,crc.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.crc.male, mx.crc.male.cause, file="~/Cancer/data/mx.crc.male.Rdata")
save(prop.crc.male, file="~/Cancer/data/prop.crc.male.Rdata") 
print(paste("completed crc.male",date()))
 
crc.female <- subset(crc, sex=="female")
dead <- by(crc.female$dead, list(crc.female$age.dx.cat, crc.female$year.dx, crc.female$stage), sum)
dead.cause <- by(crc.female$dead, list(crc.female$age.dx.cat, crc.female$year.dx, crc.female$stage, crc.female$cod), sum)
exposure <- by(crc.female$surv.months/12, list(crc.female$age.dx.cat, crc.female$year.dx, crc.female$stage), sum)
mx.crc.female <- dead/exposure
mx.crc.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.crc.female <- apply(table(crc.female$year.dx,crc.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.crc.female, mx.crc.female.cause, file="~/Cancer/data/mx.crc.female.Rdata")
save(prop.crc.female, file="~/Cancer/data/prop.crc.female.Rdata") 
print(paste("completed crc.female",date()))

#############
#Lung Cancer#
#############
seer.respir <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/RESPIR.txt",widths=348)
cancer <- as.numeric(apply(seer.respir, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.respir, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.respir, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.respir, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.respir, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.respir, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.respir, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.respir, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.respir, 1, function(x) substr(x,255,259)))
data.respir <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))
data.lung <- subset(data.respir, site.recode %in% c(22030))
drop <- which(data.lung$surv.months==9999 | data.lung$stage==9 | data.lung$age.dx < 40 | data.lung$age.dx==999)
lung <- data.lung[-drop,]
lung$sex[lung$sex==1] <- "male"
lung$sex[lung$sex==2] <- "female"
lung$dead[lung$vital.status==1] <- 0
lung$dead[lung$vital.status==4] <- 1
lung$dead[lung$surv.months >= 120] <- 0
lung$surv.months <- ifelse(lung$surv.months<=120,lung$surv.months,120)
lung$age.dx.cat <- 5*floor(lung$age.dx/5)
lung$age.dx.cat[lung$age.dx.cat>=100] <- 100
lung$stage[lung$stage==0] <- "0. in situ"
lung$stage[lung$stage==1] <- "1. localized"
lung$stage[lung$stage==2] <- "2. regional"
lung$stage[lung$stage==4] <- "4. distant"
lung$cod[lung$cod!=22030] <- "other"
lung$cod[lung$cod==22030] <- "lung"

lung.male <- subset(lung, sex=="male")
dead <- by(lung.male$dead, list(lung.male$age.dx.cat, lung.male$year.dx, lung.male$stage), sum)
dead.cause <- by(lung.male$dead, list(lung.male$age.dx.cat, lung.male$year.dx, lung.male$stage, lung.male$cod), sum)
exposure <- by(lung.male$surv.months/12, list(lung.male$age.dx.cat, lung.male$year.dx, lung.male$stage), sum)
mx.lung.male <- dead/exposure
mx.lung.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.lung.male <- apply(table(lung.male$year.dx,lung.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.lung.male, mx.lung.male.cause, file="~/Cancer/data/mx.lung.male.Rdata")
save(prop.lung.male, file="~/Cancer/data/prop.lung.male.Rdata") 
print(paste("completed lung.male",date()))
 
lung.female <- subset(lung, sex=="female")
dead <- by(lung.female$dead, list(lung.female$age.dx.cat, lung.female$year.dx, lung.female$stage), sum)
dead.cause <- by(lung.female$dead, list(lung.female$age.dx.cat, lung.female$year.dx, lung.female$stage, lung.female$cod), sum)
exposure <- by(lung.female$surv.months/12, list(lung.female$age.dx.cat, lung.female$year.dx, lung.female$stage), sum)
mx.lung.female <- dead/exposure
mx.lung.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.lung.female <- apply(table(lung.female$year.dx,lung.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.lung.female, mx.lung.female.cause, file="~/Cancer/data/mx.lung.female.Rdata")
save(prop.lung.female, file="~/Cancer/data/prop.lung.female.Rdata") 
print(paste("completed lung.female",date()))

#######################################################
#Digestive Organ Cancers: Stomach, Pancreas, Esophagus#
#######################################################
seer.digothr <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/DIGOTHR.txt",widths=348)
cancer <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.digothr, 1, function(x) substr(x,255,259)))
data.digothr <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.esophagus <- subset(data.digothr, site.recode %in% c(21010))
drop <- which(data.esophagus$surv.months==9999 | data.esophagus$stage==9 | data.esophagus$age.dx < 40 | data.esophagus$age.dx==999)
esophagus <- data.esophagus[-drop,]
esophagus$sex[esophagus$sex==1] <- "male"
esophagus$sex[esophagus$sex==2] <- "female"
esophagus$dead[esophagus$vital.status==1] <- 0
esophagus$dead[esophagus$vital.status==4] <- 1
esophagus$dead[esophagus$surv.months >= 120] <- 0
esophagus$surv.months <- ifelse(esophagus$surv.months<=120,esophagus$surv.months,120)
esophagus$age.dx.cat <- 5*floor(esophagus$age.dx/5)
esophagus$age.dx.cat[esophagus$age.dx.cat>=100] <- 100
esophagus$stage[esophagus$stage==0] <- "0. in situ"
esophagus$stage[esophagus$stage==1] <- "1. localized"
esophagus$stage[esophagus$stage==2] <- "2. regional"
esophagus$stage[esophagus$stage==4] <- "4. distant"
esophagus$stage[esophagus$stage==4] <- "8. localized.regional"
esophagus$cod[esophagus$cod!=21010] <- "other"
esophagus$cod[esophagus$cod==21010] <- "esophagus"

esophagus.male <- subset(esophagus, sex=="male")
dead <- by(esophagus.male$dead, list(esophagus.male$age.dx.cat, esophagus.male$year.dx, esophagus.male$stage), sum)
dead.cause <- by(esophagus.male$dead, list(esophagus.male$age.dx.cat, esophagus.male$year.dx, esophagus.male$stage, esophagus.male$cod), sum)
exposure <- by(esophagus.male$surv.months/12, list(esophagus.male$age.dx.cat, esophagus.male$year.dx, esophagus.male$stage), sum)
mx.esophagus.male <- dead/exposure
mx.esophagus.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.esophagus.male <- apply(table(esophagus.male$year.dx,esophagus.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.esophagus.male, mx.esophagus.male.cause, file="~/Cancer/data/mx.esophagus.male.Rdata")
save(prop.esophagus.male, file="~/Cancer/data/prop.esophagus.male.Rdata") 
print(paste("completed esophagus.male",date()))
 
esophagus.female <- subset(esophagus, sex=="female")
dead <- by(esophagus.female$dead, list(esophagus.female$age.dx.cat, esophagus.female$year.dx, esophagus.female$stage), sum)
dead.cause <- by(esophagus.female$dead, list(esophagus.female$age.dx.cat, esophagus.female$year.dx, esophagus.female$stage, esophagus.female$cod), sum)
exposure <- by(esophagus.female$surv.months/12, list(esophagus.female$age.dx.cat, esophagus.female$year.dx, esophagus.female$stage), sum)
mx.esophagus.female <- dead/exposure
mx.esophagus.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.esophagus.female <- apply(table(esophagus.female$year.dx,esophagus.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.esophagus.female, mx.esophagus.female.cause, file="~/Cancer/data/mx.esophagus.female.Rdata")
save(prop.esophagus.female, file="~/Cancer/data/prop.esophagus.female.Rdata") 
print(paste("completed esophagus.female",date()))
 
data.stomach <- subset(data.digothr, site.recode %in% c(21020))
drop <- which(data.stomach$surv.months==9999 | data.stomach$stage==9 | data.stomach$age.dx < 40 | data.stomach$age.dx==999)
stomach <- data.stomach[-drop,]
stomach$sex[stomach$sex==1] <- "male"
stomach$sex[stomach$sex==2] <- "female"
stomach$dead[stomach$vital.status==1] <- 0
stomach$dead[stomach$vital.status==4] <- 1
stomach$dead[stomach$surv.months >= 120] <- 0
stomach$surv.months <- ifelse(stomach$surv.months<=120,stomach$surv.months,120)
stomach$age.dx.cat <- 5*floor(stomach$age.dx/5)
stomach$age.dx.cat[stomach$age.dx.cat>=100] <- 100
stomach$stage[stomach$stage==0] <- "0. in situ"
stomach$stage[stomach$stage==1] <- "1. localized"
stomach$stage[stomach$stage==2] <- "2. regional"
stomach$stage[stomach$stage==4] <- "4. distant"
stomach$stage[stomach$stage==4] <- "8. localized.regional"
stomach$cod[stomach$cod!=21020] <- "other"
stomach$cod[stomach$cod==21020] <- "stomach"

stomach.male <- subset(stomach, sex=="male")
dead <- by(stomach.male$dead, list(stomach.male$age.dx.cat, stomach.male$year.dx, stomach.male$stage), sum)
dead.cause <- by(stomach.male$dead, list(stomach.male$age.dx.cat, stomach.male$year.dx, stomach.male$stage, stomach.male$cod), sum)
exposure <- by(stomach.male$surv.months/12, list(stomach.male$age.dx.cat, stomach.male$year.dx, stomach.male$stage), sum)
mx.stomach.male <- dead/exposure
mx.stomach.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.stomach.male <- apply(table(stomach.male$year.dx,stomach.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.stomach.male, mx.stomach.male.cause, file="~/Cancer/data/mx.stomach.male.Rdata")
save(prop.stomach.male, file="~/Cancer/data/prop.stomach.male.Rdata") 
print(paste("completed stomach.male",date()))
 
stomach.female <- subset(stomach, sex=="female")
dead <- by(stomach.female$dead, list(stomach.female$age.dx.cat, stomach.female$year.dx, stomach.female$stage), sum)
dead.cause <- by(stomach.female$dead, list(stomach.female$age.dx.cat, stomach.female$year.dx, stomach.female$stage, stomach.female$cod), sum)
exposure <- by(stomach.female$surv.months/12, list(stomach.female$age.dx.cat, stomach.female$year.dx, stomach.female$stage), sum)
mx.stomach.female <- dead/exposure
mx.stomach.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.stomach.female <- apply(table(stomach.female$year.dx,stomach.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.stomach.female, mx.stomach.female.cause, file="~/Cancer/data/mx.stomach.female.Rdata")
save(prop.stomach.female, file="~/Cancer/data/prop.stomach.female.Rdata") 
print(paste("completed stomach.female",date()))
 
data.pancreas <- subset(data.digothr, site.recode %in% c(21100))
drop <- which(data.pancreas$surv.months==9999 | data.pancreas$stage==9 | data.pancreas$age.dx < 40 | data.pancreas$age.dx==999)
pancreas <- data.pancreas[-drop,]
pancreas$sex[pancreas$sex==1] <- "male"
pancreas$sex[pancreas$sex==2] <- "female"
pancreas$dead[pancreas$vital.status==1] <- 0
pancreas$dead[pancreas$vital.status==4] <- 1
pancreas$dead[pancreas$surv.months >= 120] <- 0
pancreas$surv.months <- ifelse(pancreas$surv.months<=120,pancreas$surv.months,120)
pancreas$age.dx.cat <- 5*floor(pancreas$age.dx/5)
pancreas$age.dx.cat[pancreas$age.dx.cat>=100] <- 100
pancreas$stage[pancreas$stage==0] <- "0. in situ"
pancreas$stage[pancreas$stage==1] <- "1. localized"
pancreas$stage[pancreas$stage==2] <- "2. regional"
pancreas$stage[pancreas$stage==4] <- "4. distant"
pancreas$stage[pancreas$stage==4] <- "8. localized.regional"
pancreas$cod[pancreas$cod!=21100] <- "other"
pancreas$cod[pancreas$cod==21100] <- "pancreas"

pancreas.male <- subset(pancreas, sex=="male")
dead <- by(pancreas.male$dead, list(pancreas.male$age.dx.cat, pancreas.male$year.dx, pancreas.male$stage), sum)
dead.cause <- by(pancreas.male$dead, list(pancreas.male$age.dx.cat, pancreas.male$year.dx, pancreas.male$stage, pancreas.male$cod), sum)
exposure <- by(pancreas.male$surv.months/12, list(pancreas.male$age.dx.cat, pancreas.male$year.dx, pancreas.male$stage), sum)
mx.pancreas.male <- dead/exposure
mx.pancreas.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.pancreas.male <- apply(table(pancreas.male$year.dx,pancreas.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.pancreas.male, mx.pancreas.male.cause, file="~/Cancer/data/mx.pancreas.male.Rdata")
save(prop.pancreas.male, file="~/Cancer/data/prop.pancreas.male.Rdata") 
print(paste("completed pancreas.male",date()))
 
pancreas.female <- subset(pancreas, sex=="female")
dead <- by(pancreas.female$dead, list(pancreas.female$age.dx.cat, pancreas.female$year.dx, pancreas.female$stage), sum)
dead.cause <- by(pancreas.female$dead, list(pancreas.female$age.dx.cat, pancreas.female$year.dx, pancreas.female$stage, pancreas.female$cod), sum)
exposure <- by(pancreas.female$surv.months/12, list(pancreas.female$age.dx.cat, pancreas.female$year.dx, pancreas.female$stage), sum)
mx.pancreas.female <- dead/exposure
mx.pancreas.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.pancreas.female <- apply(table(pancreas.female$year.dx,pancreas.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.pancreas.female, mx.pancreas.female.cause, file="~/Cancer/data/mx.pancreas.female.Rdata")
save(prop.pancreas.female, file="~/Cancer/data/prop.pancreas.female.Rdata") 
print(paste("completed pancreas.female",date()))
 
###############################################
#Female Genital Cancers: Ovary, Uterus, Cervix#
###############################################
seer.femgen <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/FEMGEN.txt",widths=348)
cancer <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,255,259)))
data.femgen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.cervix <- subset(data.femgen, site.recode %in% c(27010))
drop <- which(data.cervix$surv.months==9999 | data.cervix$stage==9 | data.cervix$age.dx < 40 | data.cervix$age.dx==999)
cervix <- data.cervix[-drop,]
cervix$sex[cervix$sex==1] <- "male"
cervix$sex[cervix$sex==2] <- "female"
cervix$dead[cervix$vital.status==1] <- 0
cervix$dead[cervix$vital.status==4] <- 1
cervix$dead[cervix$surv.months >= 120] <- 0
cervix$surv.months <- ifelse(cervix$surv.months<=120,cervix$surv.months,120)
cervix$age.dx.cat <- 5*floor(cervix$age.dx/5)
cervix$age.dx.cat[cervix$age.dx.cat>=100] <- 100
cervix$stage[cervix$stage==0] <- "0. in situ"
cervix$stage[cervix$stage==1] <- "1. localized"
cervix$stage[cervix$stage==2] <- "2. regional"
cervix$stage[cervix$stage==4] <- "4. distant"
cervix$stage[cervix$stage==4] <- "8. localized.regional"
cervix$cod[cervix$cod!=27010] <- "other"
cervix$cod[cervix$cod==27010] <- "cervix"
dead <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage), sum)
dead.cause <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage, cervix$cod), sum)
exposure <- by(cervix$surv.months/12, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage), sum)
mx.cervix <- dead/exposure
mx.cervix.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.cervix <- prop.table(as.table(table(cervix$year.dx, cervix$stage)),1)
save(mx.cervix, mx.cervix.cause, file="~/Cancer/data/mx.cervix.Rdata")
save(prop.cervix, file="~/Cancer/data/prop.cervix.Rdata") 
print(paste("completed cervix",date()))
 
data.uterus <- subset(data.femgen, site.recode %in% c(27020,27030))
drop <- which(data.uterus$surv.months==9999 | data.uterus$stage==9 | data.uterus$age.dx < 40 | data.uterus$age.dx==999)
uterus <- data.uterus[-drop,]
uterus$sex[uterus$sex==1] <- "male"
uterus$sex[uterus$sex==2] <- "female"
uterus$dead[uterus$vital.status==1] <- 0
uterus$dead[uterus$vital.status==4] <- 1
uterus$dead[uterus$surv.months >= 120] <- 0
uterus$surv.months <- ifelse(uterus$surv.months<=120,uterus$surv.months,120)
uterus$age.dx.cat <- 5*floor(uterus$age.dx/5)
uterus$age.dx.cat[uterus$age.dx.cat>=100] <- 100
uterus$stage[uterus$stage==0] <- "0. in situ"
uterus$stage[uterus$stage==1] <- "1. localized"
uterus$stage[uterus$stage==2] <- "2. regional"
uterus$stage[uterus$stage==4] <- "4. distant"
uterus$stage[uterus$stage==4] <- "8. localized.regional"
uterus$cod[!(uterus$cod %in% c(27020,27030))] <- "other"
uterus$cod[uterus$cod %in% c(27020,27030)] <- "uterus"
dead <- by(uterus$dead, list(uterus$age.dx.cat, uterus$year.dx, uterus$stage), sum)
dead.cause <- by(uterus$dead, list(uterus$age.dx.cat, uterus$year.dx, uterus$stage, uterus$cod), sum)
exposure <- by(uterus$surv.months/12, list(uterus$age.dx.cat, uterus$year.dx, uterus$stage), sum)
mx.uterus <- dead/exposure
mx.uterus.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.uterus <- prop.table(as.table(table(uterus$year.dx, uterus$stage)),1)
save(mx.uterus, mx.uterus.cause, file="~/Cancer/data/mx.uterus.Rdata")
save(prop.uterus, file="~/Cancer/data/prop.uterus.Rdata") 
print(paste("completed uterus",date()))
 
data.ovary <- subset(data.femgen, site.recode %in% c(27040))
drop <- which(data.ovary$surv.months==9999 | data.ovary$stage==9 | data.ovary$age.dx < 40 | data.ovary$age.dx==999)
ovary <- data.ovary[-drop,]
ovary$sex[ovary$sex==1] <- "male"
ovary$sex[ovary$sex==2] <- "female"
ovary$dead[ovary$vital.status==1] <- 0
ovary$dead[ovary$vital.status==4] <- 1
ovary$dead[ovary$surv.months >= 120] <- 0
ovary$surv.months <- ifelse(ovary$surv.months<=120,ovary$surv.months,120)
ovary$age.dx.cat <- 5*floor(ovary$age.dx/5)
ovary$age.dx.cat[ovary$age.dx.cat>=100] <- 100
ovary$stage[ovary$stage==0] <- "0. in situ"
ovary$stage[ovary$stage==1] <- "1. localized"
ovary$stage[ovary$stage==2] <- "2. regional"
ovary$stage[ovary$stage==4] <- "4. distant"
ovary$stage[ovary$stage==4] <- "8. localized.regional"
ovary$cod[ovary$cod!=27040] <- "other"
ovary$cod[ovary$cod==27040] <- "ovary"
dead <- by(ovary$dead, list(ovary$age.dx.cat, ovary$year.dx, ovary$stage), sum)
dead.cause <- by(ovary$dead, list(ovary$age.dx.cat, ovary$year.dx, ovary$stage, ovary$cod), sum)
exposure <- by(ovary$surv.months/12, list(ovary$age.dx.cat, ovary$year.dx, ovary$stage), sum)
mx.ovary <- dead/exposure
mx.ovary.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.ovary <- prop.table(as.table(table(ovary$year.dx, ovary$stage)),1)
save(mx.ovary, mx.ovary.cause, file="~/Cancer/data/mx.ovary.Rdata")
save(prop.ovary, file="~/Cancer/data/prop.ovary.Rdata") 
print(paste("completed ovary",date()))
 
################################
#Male Genital Cancers: Prostate#
################################
seer.malegen <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/MALEGEN.txt",widths=348)
cancer <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,255,259)))
data.malegen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.prostate <- subset(data.malegen, site.recode %in% c(28010))
drop <- which(data.prostate$surv.months==9999 | data.prostate$stage==9 | data.prostate$age.dx < 40 | data.prostate$age.dx==999)
prostate <- data.prostate[-drop,]
prostate$sex[prostate$sex==1] <- "male"
prostate$sex[prostate$sex==2] <- "female"
prostate$dead[prostate$vital.status==1] <- 0
prostate$dead[prostate$vital.status==4] <- 1
prostate$dead[prostate$surv.months >= 120] <- 0
prostate$surv.months <- ifelse(prostate$surv.months<=120,prostate$surv.months,120)
prostate$age.dx.cat <- 5*floor(prostate$age.dx/5)
prostate$age.dx.cat[prostate$age.dx.cat>=100] <- 100
prostate$stage[prostate$stage==0] <- "0. in situ"
prostate$stage[prostate$stage==1] <- "1. localized"
prostate$stage[prostate$stage==2] <- "2. regional"
prostate$stage[prostate$stage==4] <- "4. distant"
prostate$stage[prostate$stage==8] <- "8. localized.regional"
prostate$cod[prostate$cod!=28010] <- "other"
prostate$cod[prostate$cod==28010] <- "prostate"
dead <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$stage), sum)
dead.cause <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$stage, prostate$cod), sum)
exposure <- by(prostate$surv.months/12, list(prostate$age.dx.cat, prostate$year.dx, prostate$stage), sum)
mx.prostate <- dead/exposure
mx.prostate.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.prostate <- prop.table(as.table(table(prostate$year.dx, prostate$stage)),1)
save(mx.prostate, mx.prostate.cause, file="~/Cancer/data/mx.prostate.Rdata")
save(prop.prostate, file="~/Cancer/data/prop.prostate.Rdata") 
print(paste("completed prostate",date()))
 
########################################
#Urinary Organ Cancers: Bladder, Kidney#
########################################
seer.urinary <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/URINARY.txt",widths=348)
cancer <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.urinary, 1, function(x) substr(x,255,259)))
data.urinary <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.bladder <- subset(data.urinary, site.recode %in% c(29010))
drop <- which(data.bladder$surv.months==9999 | data.bladder$stage==9 | data.bladder$age.dx < 40 | data.bladder$age.dx==999)
bladder <- data.bladder[-drop,]
bladder$sex[bladder$sex==1] <- "male"
bladder$sex[bladder$sex==2] <- "female"
bladder$dead[bladder$vital.status==1] <- 0
bladder$dead[bladder$vital.status==4] <- 1
bladder$dead[bladder$surv.months >= 120] <- 0
bladder$surv.months <- ifelse(bladder$surv.months<=120,bladder$surv.months,120)
bladder$age.dx.cat <- 5*floor(bladder$age.dx/5)
bladder$age.dx.cat[bladder$age.dx.cat>=100] <- 100
bladder$stage[bladder$stage==0] <- "0. in situ"
bladder$stage[bladder$stage==1] <- "1. localized"
bladder$stage[bladder$stage==2] <- "2. regional"
bladder$stage[bladder$stage==4] <- "4. distant"
bladder$stage[bladder$stage==4] <- "8. localized.regional"
bladder$cod[bladder$cod!=29010] <- "other"
bladder$cod[bladder$cod==29010] <- "bladder"

bladder.male <- subset(bladder, sex=="male")
dead <- by(bladder.male$dead, list(bladder.male$age.dx.cat, bladder.male$year.dx, bladder.male$stage), sum)
dead.cause <- by(bladder.male$dead, list(bladder.male$age.dx.cat, bladder.male$year.dx, bladder.male$stage, bladder.male$cod), sum)
exposure <- by(bladder.male$surv.months/12, list(bladder.male$age.dx.cat, bladder.male$year.dx, bladder.male$stage), sum)
mx.bladder.male <- dead/exposure
mx.bladder.male <- abind(mx.bladder.male,matrix(0,dim=dim(mx.bladder.male)[1:2]),along=3)

mx.bladder.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.bladder.male <- apply(table(bladder.male$year.dx,bladder.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.bladder.male, mx.bladder.male.cause, file="~/Cancer/data/mx.bladder.male.Rdata")
save(prop.bladder.male, file="~/Cancer/data/prop.bladder.male.Rdata") 
print(paste("completed bladder.male",date()))
 
bladder.female <- subset(bladder, sex=="female")
dead <- by(bladder.female$dead, list(bladder.female$age.dx.cat, bladder.female$year.dx, bladder.female$stage), sum)
dead.cause <- by(bladder.female$dead, list(bladder.female$age.dx.cat, bladder.female$year.dx, bladder.female$stage, bladder.female$cod), sum)
exposure <- by(bladder.female$surv.months/12, list(bladder.female$age.dx.cat, bladder.female$year.dx, bladder.female$stage), sum)
mx.bladder.female <- dead/exposure
mx.bladder.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.bladder.female <- apply(table(bladder.female$year.dx,bladder.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.bladder.female, mx.bladder.female.cause, file="~/Cancer/data/mx.bladder.female.Rdata")
save(prop.bladder.female, file="~/Cancer/data/prop.bladder.female.Rdata") 
print(paste("completed bladder.female",date()))

data.kidney <- subset(data.urinary, site.recode %in% c(29020))
drop <- which(data.kidney$surv.months==9999 | data.kidney$stage==9 | data.kidney$age.dx < 40 | data.kidney$age.dx==999)
kidney <- data.kidney[-drop,]
kidney$sex[kidney$sex==1] <- "male"
kidney$sex[kidney$sex==2] <- "female"
kidney$dead[kidney$vital.status==1] <- 0
kidney$dead[kidney$vital.status==4] <- 1
kidney$dead[kidney$surv.months >= 120] <- 0
kidney$surv.months <- ifelse(kidney$surv.months<=120,kidney$surv.months,120)
kidney$age.dx.cat <- 5*floor(kidney$age.dx/5)
kidney$age.dx.cat[kidney$age.dx.cat>=100] <- 100
kidney$stage[kidney$stage==0] <- "0. in situ"
kidney$stage[kidney$stage==1] <- "1. localized"
kidney$stage[kidney$stage==2] <- "2. regional"
kidney$stage[kidney$stage==4] <- "4. distant"
kidney$stage[kidney$stage==4] <- "8. localized.regional"
kidney$cod[kidney$cod!=29020] <- "other"
kidney$cod[kidney$cod==29020] <- "kidney"

kidney.male <- subset(kidney, sex=="male")
dead <- by(kidney.male$dead, list(kidney.male$age.dx.cat, kidney.male$year.dx, kidney.male$stage), sum)
dead.cause <- by(kidney.male$dead, list(kidney.male$age.dx.cat, kidney.male$year.dx, kidney.male$stage, kidney.male$cod), sum)
exposure <- by(kidney.male$surv.months/12, list(kidney.male$age.dx.cat, kidney.male$year.dx, kidney.male$stage), sum)
mx.kidney.male <- dead/exposure
mx.kidney.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.kidney.male <- apply(table(kidney.male$year.dx,kidney.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.kidney.male, mx.kidney.male.cause, file="~/Cancer/data/mx.kidney.male.Rdata")
save(prop.kidney.male, file="~/Cancer/data/prop.kidney.male.Rdata") 
print(paste("completed kidney.male",date()))
 
kidney.female <- subset(kidney, sex=="female")
dead <- by(kidney.female$dead, list(kidney.female$age.dx.cat, kidney.female$year.dx, kidney.female$stage), sum)
dead.cause <- by(kidney.female$dead, list(kidney.female$age.dx.cat, kidney.female$year.dx, kidney.female$stage, kidney.female$cod), sum)
exposure <- by(kidney.female$surv.months/12, list(kidney.female$age.dx.cat, kidney.female$year.dx, kidney.female$stage), sum)
mx.kidney.female <- dead/exposure
mx.kidney.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.kidney.female <- apply(table(kidney.female$year.dx,kidney.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.kidney.female, mx.kidney.female.cause, file="~/Cancer/data/mx.kidney.female.Rdata")
save(prop.kidney.female, file="~/Cancer/data/prop.kidney.female.Rdata") 
print(paste("completed kidney.female",date()))
 
########################################
#Other Organ Cancers: Melanoma, Head & Neck, Brain
########################################
seer.other <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/OTHER.txt",widths=348)
cancer <- as.numeric(apply(seer.other, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.other, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.other, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.other, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.other, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.other, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.other, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.other, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.other, 1, function(x) substr(x,255,259)))
data.other <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.melanoma <- subset(data.other, site.recode %in% c(25010))
drop <- which(data.melanoma$surv.months==9999 | data.melanoma$stage==9 | data.melanoma$age.dx < 40 | data.melanoma$age.dx==999)
melanoma <- data.melanoma[-drop,]
melanoma$sex[melanoma$sex==1] <- "male"
melanoma$sex[melanoma$sex==2] <- "female"
melanoma$dead[melanoma$vital.status==1] <- 0
melanoma$dead[melanoma$vital.status==4] <- 1
melanoma$dead[melanoma$surv.months >= 120] <- 0
melanoma$surv.months <- ifelse(melanoma$surv.months<=120,melanoma$surv.months,120)
melanoma$age.dx.cat <- 5*floor(melanoma$age.dx/5)
melanoma$age.dx.cat[melanoma$age.dx.cat>=100] <- 100
melanoma$stage[melanoma$stage==0] <- "0. in situ"
melanoma$stage[melanoma$stage==1] <- "1. localized"
melanoma$stage[melanoma$stage==2] <- "2. regional"
melanoma$stage[melanoma$stage==4] <- "4. distant"
melanoma$stage[melanoma$stage==4] <- "8. localized.regional"
melanoma$cod[melanoma$cod!=25010] <- "other"
melanoma$cod[melanoma$cod==25010] <- "melanoma"

melanoma.male <- subset(melanoma, sex=="male")
dead <- by(melanoma.male$dead, list(melanoma.male$age.dx.cat, melanoma.male$year.dx, melanoma.male$stage), sum)
dead.cause <- by(melanoma.male$dead, list(melanoma.male$age.dx.cat, melanoma.male$year.dx, melanoma.male$stage, melanoma.male$cod), sum)
exposure <- by(melanoma.male$surv.months/12, list(melanoma.male$age.dx.cat, melanoma.male$year.dx, melanoma.male$stage), sum)
mx.melanoma.male <- dead/exposure
mx.melanoma.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.melanoma.male <- apply(table(melanoma.male$year.dx,melanoma.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.melanoma.male, mx.melanoma.male.cause, file="~/Cancer/data/mx.melanoma.male.Rdata")
save(prop.melanoma.male, file="~/Cancer/data/prop.melanoma.male.Rdata") 
print(paste("completed melanoma.male",date()))
 
melanoma.female <- subset(melanoma, sex=="female")
dead <- by(melanoma.female$dead, list(melanoma.female$age.dx.cat, melanoma.female$year.dx, melanoma.female$stage), sum)
dead.cause <- by(melanoma.female$dead, list(melanoma.female$age.dx.cat, melanoma.female$year.dx, melanoma.female$stage, melanoma.female$cod), sum)
exposure <- by(melanoma.female$surv.months/12, list(melanoma.female$age.dx.cat, melanoma.female$year.dx, melanoma.female$stage), sum)
mx.melanoma.female <- dead/exposure
mx.melanoma.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.melanoma.female <- apply(table(melanoma.female$year.dx,melanoma.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.melanoma.female, mx.melanoma.female.cause, file="~/Cancer/data/mx.melanoma.female.Rdata")
save(prop.melanoma.female, file="~/Cancer/data/prop.melanoma.female.Rdata") 
print(paste("completed melanoma.female",date()))
 
data.headneck <- subset(data.other, site.recode %in% c(20100))
drop <- which(data.headneck$surv.months==9999 | data.headneck$stage==9 | data.headneck$age.dx < 40 | data.headneck$age.dx==999)
headneck <- data.headneck[-drop,]
headneck$sex[headneck$sex==1] <- "male"
headneck$sex[headneck$sex==2] <- "female"
headneck$dead[headneck$vital.status==1] <- 0
headneck$dead[headneck$vital.status==4] <- 1
headneck$dead[headneck$surv.months >= 120] <- 0
headneck$surv.months <- ifelse(headneck$surv.months<=120,headneck$surv.months,120)
headneck$age.dx.cat <- 5*floor(headneck$age.dx/5)
headneck$age.dx.cat[headneck$age.dx.cat>=100] <- 100
headneck$stage[headneck$stage==0] <- "0. in situ"
headneck$stage[headneck$stage==1] <- "1. localized"
headneck$stage[headneck$stage==2] <- "2. regional"
headneck$stage[headneck$stage==4] <- "4. distant"
headneck$stage[headneck$stage==4] <- "8. localized.regional"
headneck$cod[headneck$cod!=20100] <- "other"
headneck$cod[headneck$cod==20100] <- "headneck"

headneck.male <- subset(headneck, sex=="male")
dead <- by(headneck.male$dead, list(headneck.male$age.dx.cat, headneck.male$year.dx, headneck.male$stage), sum)
dead <- abind(dead, matrix(0,dim(dead)[2],dim(dead)[3]),along=1)
dimnames(dead)[[1]] <- seq(40,100,5)
dead.cause <- by(headneck.male$dead, list(headneck.male$age.dx.cat, headneck.male$year.dx, headneck.male$stage, headneck.male$cod), sum)
dead.cause <- abind(dead.cause, array(0,dim=c(dim(dead.cause)[2],dim(dead.cause)[3],dim(dead.cause)[4])),along=1)
dimnames(dead.cause)[[1]] <- seq(40,100,5)
exposure <- by(headneck.male$surv.months/12, list(headneck.male$age.dx.cat, headneck.male$year.dx, headneck.male$stage), sum)
exposure <- abind(exposure, matrix(0,dim(exposure)[2],dim(exposure)[3]),along=1)
dimnames(exposure)[[1]] <- seq(40,100,5)
mx.headneck.male <- dead/exposure
mx.headneck.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.headneck.male <- apply(table(headneck.male$year.dx,headneck.male$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.headneck.male, mx.headneck.male.cause, file="~/Cancer/data/mx.headneck.male.Rdata")
save(prop.headneck.male, file="~/Cancer/data/prop.headneck.male.Rdata") 
print(paste("completed headneck.male",date()))
 
headneck.female <- subset(headneck, sex=="female")
dead <- by(headneck.female$dead, list(headneck.female$age.dx.cat, headneck.female$year.dx, headneck.female$stage), sum)
dead <- abind(dead, matrix(0,dim(dead)[2],dim(dead)[3]),along=1)
dimnames(dead)[[1]] <- seq(40,100,5)
dead.cause <- by(headneck.female$dead, list(headneck.female$age.dx.cat, headneck.female$year.dx, headneck.female$stage, headneck.female$cod), sum)
dead.cause <- abind(dead.cause, array(0,dim=c(dim(dead.cause)[2],dim(dead.cause)[3],dim(dead.cause)[4])),along=1)
dimnames(dead.cause)[[1]] <- seq(40,100,5)
exposure <- by(headneck.female$surv.months/12, list(headneck.female$age.dx.cat, headneck.female$year.dx, headneck.female$stage), sum)
exposure <- abind(exposure, matrix(0,dim(exposure)[2],dim(exposure)[3]),along=1)
dimnames(exposure)[[1]] <- seq(40,100,5)
mx.headneck.female <- dead/exposure
mx.headneck.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.headneck.female <- apply(table(headneck.female$year.dx,headneck.female$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.headneck.female, mx.headneck.female.cause, file="~/Cancer/data/mx.headneck.female.Rdata")
save(prop.headneck.female, file="~/Cancer/data/prop.headneck.female.Rdata") 
print(paste("completed headneck.female",date()))
 
 
#Note (Dec 11, 2014): There is no stage for brain cancer.  All brain cancers are unstaged (==9).  We should drop brain cancer from the list of cancers we consider.
#data.brain <- subset(data.other, site.recode %in% c(31010))
#drop <- which(data.brain$surv.months==9999 | data.brain$stage==9 | data.brain$age.dx < 40 | data.brain$age.dx==999)
#brain <- data.brain[-drop,]
#brain$sex[brain$sex==1] <- "male"
#brain$sex[brain$sex==2] <- "female"
#brain$dead[brain$vital.status==1] <- 0
#brain$dead[brain$vital.status==4] <- 1
#brain$dead[brain$surv.months >= 120] <- 0
#brain$surv.months <- ifelse(brain$surv.months<=120,brain$surv.months,120)
#brain$age.dx.cat <- 5*floor(brain$age.dx/5)
#brain$age.dx.cat[brain$age.dx.cat>=100] <- 100
#brain$stage[brain$stage==0] <- "0. in situ"
#brain$stage[brain$stage==1] <- "1. localized"
#brain$stage[brain$stage==2] <- "2. regional"
#brain$stage[brain$stage==4] <- "4. distant"
#brain$stage[brain$stage==4] <- "8. localized.regional"
#brain$cod[brain$cod!=31010] <- "other"
#brain$cod[brain$cod==31010] <- "brain"
#dead <- by(brain$dead, list(brain$age.dx.cat, brain$year.dx, brain$sex, brain$stage), sum)
#dead.cause <- by(brain$dead, list(brain$age.dx.cat, brain$year.dx, brain$sex,brain$stage, brain$cod), sum)
#exposure <- by(brain$surv.months/12, list(brain$age.dx.cat, brain$year.dx, brain$sex, brain$stage), sum)
#mx.brain <- dead/exposure
#mx.brain.cause <- aaply(dead.cause, 5, function(x) x/exposure)
#prop.brain <- prop.table(as.table(table(brain$year.dx, brain$sex, brain$stage)),1)
#save(mx.brain, mx.brain.cause, file="~/Cancer/data/mx.brain.Rdata")
#save(prop.brain, file="~/Cancer/data/prop.brain.Rdata") 
#print(paste("completed brain",date()))
 
########################################
#Blood Cancers: Lymphoma, Leukemia
########################################
seer.lymyleuk <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/LYMYLEUK.txt",widths=348)
cancer <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,24,24)))
stage <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,236,236)))
stage.lymphoma <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,348,348))) #Ann Arbor staging, 1983+
vital.status <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,255,259)))
data.lymyleuk <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,stage.lymphoma=stage.lymphoma,vital.status=vital.status,surv.months=surv.months,cod=cod))

#Note (Dec 11, 2014): Lymphoma staging follows Ann Arbor Staging (variable 348-348 in SEER data)
data.lymphoma <- subset(data.lymyleuk, site.recode %in% c(33011,33012,33041,33042))
drop <- which(data.lymphoma$surv.months==9999 | data.lymphoma$stage.lymphoma %in% c(8,9) | data.lymphoma$age.dx < 40 | data.lymphoma$age.dx==999)
lymphoma <- data.lymphoma[-drop,]
lymphoma$sex[lymphoma$sex==1] <- "male"
lymphoma$sex[lymphoma$sex==2] <- "female"
lymphoma$dead[lymphoma$vital.status==1] <- 0
lymphoma$dead[lymphoma$vital.status==4] <- 1
lymphoma$dead[lymphoma$surv.months >= 120] <- 0
lymphoma$surv.months <- ifelse(lymphoma$surv.months<=120,lymphoma$surv.months,120)
lymphoma$age.dx.cat <- 5*floor(lymphoma$age.dx/5)
lymphoma$age.dx.cat[lymphoma$age.dx.cat>=100] <- 100
lymphoma$stage.lymphoma[lymphoma$stage.lymphoma==1] <- "1. stage I"
lymphoma$stage.lymphoma[lymphoma$stage.lymphoma==2] <- "2. stage II"
lymphoma$stage.lymphoma[lymphoma$stage.lymphoma==3] <- "3. stage III"
lymphoma$stage.lymphoma[lymphoma$stage.lymphoma==4] <- "4. stage IV"
lymphoma$cod[!(lymphoma$cod %in% c(33010,33040))] <- "other"
lymphoma$cod[lymphoma$cod %in% c(33010,33040)] <- "lymphoma"

lymphoma.male <- subset(lymphoma, sex=="male")
dead <- by(lymphoma.male$dead, list(lymphoma.male$age.dx.cat, lymphoma.male$year.dx, lymphoma.male$stage.lymphoma), sum)
dead.cause <- by(lymphoma.male$dead, list(lymphoma.male$age.dx.cat, lymphoma.male$year.dx, lymphoma.male$stage.lymphoma, lymphoma.male$cod), sum)
exposure <- by(lymphoma.male$surv.months/12, list(lymphoma.male$age.dx.cat, lymphoma.male$year.dx, lymphoma.male$stage.lymphoma), sum)
mx.lymphoma.male <- dead/exposure
mx.lymphoma.male.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.lymphoma.male <- apply(table(lymphoma.male$year.dx,lymphoma.male$stage.lymphoma),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.lymphoma.male, mx.lymphoma.male.cause, file="~/Cancer/data/mx.lymphoma.male.Rdata")
save(prop.lymphoma.male, file="~/Cancer/data/prop.lymphoma.male.Rdata") 
print(paste("completed lymphoma.male",date()))
 
lymphoma.female <- subset(lymphoma, sex=="female")
dead <- by(lymphoma.female$dead, list(lymphoma.female$age.dx.cat, lymphoma.female$year.dx, lymphoma.female$stage.lymphoma), sum)
dead.cause <- by(lymphoma.female$dead, list(lymphoma.female$age.dx.cat, lymphoma.female$year.dx, lymphoma.female$stage.lymphoma, lymphoma.female$cod), sum)
exposure <- by(lymphoma.female$surv.months/12, list(lymphoma.female$age.dx.cat, lymphoma.female$year.dx, lymphoma.female$stage.lymphoma), sum)
mx.lymphoma.female <- dead/exposure
mx.lymphoma.female.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.lymphoma.female <- apply(table(lymphoma.female$year.dx,lymphoma.female$stage.lymphoma),c(1),function(x) x/sum(x,na.rm=TRUE))
save(mx.lymphoma.female, mx.lymphoma.female.cause, file="~/Cancer/data/mx.lymphoma.female.Rdata")
save(prop.lymphoma.female, file="~/Cancer/data/prop.lymphoma.female.Rdata") 
print(paste("completed lymphoma.female",date()))

#Note (Dec 11, 2014): Leukemia stage seems to depend on type of leukemia.  Should we drop leukemia?
#data.leukemia <- subset(data.lymyleuk, site.recode %in% c(35011,35012,35013,35021,35031,35022,35023,35041,35043))
#drop <- which(data.leukemia$surv.months==9999 | data.leukemia$stage==9 | data.leukemia$age.dx < 40 | data.leukemia$age.dx==999)
#leukemia <- data.leukemia[-drop,]
#leukemia$sex[leukemia$sex==1] <- "male"
#leukemia$sex[leukemia$sex==2] <- "female"
#leukemia$dead[leukemia$vital.status==1] <- 0
#leukemia$dead[leukemia$vital.status==4] <- 1
#leukemia$dead[leukemia$surv.months >= 120] <- 0
#leukemia$surv.months <- ifelse(leukemia$surv.months<=120,leukemia$surv.months,120)
#leukemia$age.dx.cat <- 5*floor(leukemia$age.dx/5)
#leukemia$age.dx.cat[leukemia$age.dx.cat>=100] <- 100
#leukemia$stage[leukemia$stage==0] <- "0. in situ"
#leukemia$stage[leukemia$stage==1] <- "1. localized"
#leukemia$stage[leukemia$stage==2] <- "2. regional"
#leukemia$stage[leukemia$stage==4] <- "4. distant"
#leukemia$stage[leukemia$stage==4] <- "8. localized.regional"
#leukemia$cod[!(leukemia$cod %in% c(35011,35012,35013,35021,35031,35022,35023,35041,35043))] <- "other"
#leukemia$cod[leukemia$cod %in% c(35011,35012,35013,35021,35031,35022,35023,35041,35043)] <- "leukemia"
#dead <- by(leukemia$dead, list(leukemia$age.dx.cat, leukemia$year.dx, leukemia$sex), sum)
#dead.cause <- by(leukemia$dead, list(leukemia$age.dx.cat, leukemia$year.dx, leukemia$sex, leukemia$cod), sum)
#exposure <- by(leukemia$surv.months/12, list(leukemia$age.dx.cat, leukemia$year.dx, leukemia$sex), sum)
#mx.leukemia <- dead/exposure
#mx.leukemia.cause <- aaply(dead.cause, 4, function(x) x/exposure)
#prop.leukemia <- 1
#save(mx.leukemia, mx.leukemia.cause, file="~/Cancer/data/mx.leukemia.Rdata")
#save(prop.leukemia, file="~/Cancer/data/prop.leukemia.Rdata") 
#print(paste("completed leukemia",date()))
 
 
