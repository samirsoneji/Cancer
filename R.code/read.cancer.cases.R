setwd("~/Dropbox/Cancer/Value")

###############
#Breast Cancer#
###############
seer.breast <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/BREAST.txt",widths=348)
age.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,39,42)))
stage <- as.numeric(apply(seer.breast, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.breast, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.breast, 1, function(x) substr(x,301,304)))
data.breast <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,stage=stage,vital.status=vital.status,surv.months=surv.months))
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
year.list <- as.numeric(rownames(table(breast$year.dx)))
age.list <-  as.numeric(rownames(table(breast$age.dx.cat)))
dead <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
exposure <- by(breast$surv.months/12, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
mx.breast <- dead/exposure
prop.breast <- prop.table(as.table(table(breast$year.dx, breast$stage)),1)
save(mx.breast, file="~/Cancer/data/mx.breast.Rdata")
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
data.crc <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,sex=sex,stage=stage,vital.status=vital.status,surv.months=surv.months))
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
crc$stage[crc$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(crc$year.dx)))
age.list <-  as.numeric(rownames(table(crc$age.dx.cat)))
dead <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$sex, crc$stage), sum)
exposure <- by(crc$surv.months/12, list(crc$age.dx.cat, crc$year.dx, crc$sex, crc$stage), sum)
mx.crc <- dead/exposure
prop.crc <- apply(table(crc$year.dx, crc$sex, crc$stage),c(1,2),function(x) prop.table(x))
save(mx.crc, file="~/Cancer/data/mx.crc.Rdata")
save(prop.crc, file="~/Cancer/data/prop.crc.Rdata") 
print(paste("completed crc",date()))

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
data.respir <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))
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
lung$stage[lung$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(lung$year.dx)))
age.list <-  as.numeric(rownames(table(lung$age.dx.cat)))
dead <- by(lung$dead, list(lung$age.dx.cat, lung$year.dx, lung$sex, lung$stage), sum)
exposure <- by(lung$surv.months/12, list(lung$age.dx.cat, lung$year.dx, lung$sex, lung$stage), sum)
mx.lung <- dead/exposure
prop.lung <- apply(table(lung$year.dx, lung$sex, lung$stage),c(1,2),function(x) prop.table(x))
save(mx.lung, file="~/Cancer/data/mx.lung.Rdata")
save(prop.lung, file="~/Cancer/data/prop.lung.Rdata") 
print(paste("completed lung",date()))

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
data.digothr <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

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
year.list <- as.numeric(rownames(table(esophagus$year.dx)))
age.list <-  as.numeric(rownames(table(esophagus$age.dx.cat)))
dead <- by(esophagus$dead, list(esophagus$age.dx.cat, esophagus$year.dx, esophagus$sex, esophagus$stage), sum)
exposure <- by(esophagus$surv.months/12, list(esophagus$age.dx.cat, esophagus$year.dx, esophagus$sex, esophagus$stage), sum)
mx.esophagus <- dead/exposure
prop.esophagus <- apply(table(esophagus$year.dx, esophagus$sex, esophagus$stage),c(1,2),function(x) prop.table(x))
save(mx.esophagus, file="~/Cancer/data/mx.esophagus.Rdata")
save(prop.esophagus, file="~/Cancer/data/prop.esophagus.Rdata") 
print(paste("completed esophagus",date()))

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
year.list <- as.numeric(rownames(table(stomach$year.dx)))
age.list <-  as.numeric(rownames(table(stomach$age.dx.cat)))
dead <- by(stomach$dead, list(stomach$age.dx.cat, stomach$year.dx, stomach$sex, stomach$stage), sum)
exposure <- by(stomach$surv.months/12, list(stomach$age.dx.cat, stomach$year.dx, stomach$sex, stomach$stage), sum)
mx.stomach <- dead/exposure
prop.stomach <- apply(table(stomach$year.dx, stomach$sex, stomach$stage),c(1,2),function(x) prop.table(x))
save(mx.stomach, file="~/Cancer/data/mx.stomach.Rdata")
save(prop.stomach, file="~/Cancer/data/prop.stomach.Rdata") 
print(paste("completed stomach",date()))

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
year.list <- as.numeric(rownames(table(pancreas$year.dx)))
age.list <-  as.numeric(rownames(table(pancreas$age.dx.cat)))
dead <- by(pancreas$dead, list(pancreas$age.dx.cat, pancreas$year.dx, pancreas$sex, pancreas$stage), sum)
exposure <- by(pancreas$surv.months/12, list(pancreas$age.dx.cat, pancreas$year.dx, pancreas$sex, pancreas$stage), sum)
mx.pancreas <- dead/exposure
prop.pancreas <- apply(table(pancreas$year.dx, pancreas$sex, pancreas$stage),c(1,2),function(x) prop.table(x))
save(mx.pancreas, file="~/Cancer/data/mx.pancreas.Rdata")
save(prop.pancreas, file="~/Cancer/data/prop.pancreas.Rdata") 
print(paste("completed pancreas",date()))

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
data.femgen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

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
year.list <- as.numeric(rownames(table(cervix$year.dx)))
age.list <-  as.numeric(rownames(table(cervix$age.dx.cat)))
dead <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$sex, cervix$stage), sum)
exposure <- by(cervix$surv.months/12, list(cervix$age.dx.cat, cervix$year.dx, cervix$sex, cervix$stage), sum)
mx.cervix <- dead/exposure
prop.cervix <- apply(table(cervix$year.dx, cervix$sex, cervix$stage),c(1,2),function(x) prop.table(x))
mx.cervix <- mx.cervix[,,"female",]
prop.cervix <- prop.cervix[,,"female"]
save(mx.cervix, file="~/Cancer/data/mx.cervix.Rdata")
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
year.list <- as.numeric(rownames(table(uterus$year.dx)))
age.list <-  as.numeric(rownames(table(uterus$age.dx.cat)))
dead <- by(uterus$dead, list(uterus$age.dx.cat, uterus$year.dx, uterus$sex, uterus$stage), sum)
exposure <- by(uterus$surv.months/12, list(uterus$age.dx.cat, uterus$year.dx, uterus$sex, uterus$stage), sum)
mx.uterus <- dead/exposure
prop.uterus <- apply(table(uterus$year.dx, uterus$sex, uterus$stage),c(1,2),function(x) prop.table(x))
mx.uterus <- mx.uterus[,,"female",]
prop.uterus <- prop.uterus[,,"female"]
save(mx.uterus, file="~/Cancer/data/mx.uterus.Rdata")
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
year.list <- as.numeric(rownames(table(ovary$year.dx)))
age.list <-  as.numeric(rownames(table(ovary$age.dx.cat)))
dead <- by(ovary$dead, list(ovary$age.dx.cat, ovary$year.dx, ovary$sex, ovary$stage), sum)
exposure <- by(ovary$surv.months/12, list(ovary$age.dx.cat, ovary$year.dx, ovary$sex, ovary$stage), sum)
mx.ovary <- dead/exposure
prop.ovary <- apply(table(ovary$year.dx, ovary$sex, ovary$stage),c(1,2),function(x) prop.table(x))
mx.ovary <- mx.ovary[,,"female",]
prop.ovary <- prop.ovary[,,"female"]
save(mx.ovary, file="~/Cancer/data/mx.ovary.Rdata")
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
data.malegen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

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
prostate$stage[prostate$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(prostate$year.dx)))
age.list <-  as.numeric(rownames(table(prostate$age.dx.cat)))
dead <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$sex, prostate$stage), sum)
exposure <- by(prostate$surv.months/12, list(prostate$age.dx.cat, prostate$year.dx, prostate$sex, prostate$stage), sum)
mx.prostate <- dead/exposure
prop.prostate <- apply(table(prostate$year.dx, prostate$sex, prostate$stage),c(1,2),function(x) prop.table(x))
mx.prostate <- mx.prostate[,,"male",]
prop.prostate <- prop.prostate[,,"male"]
save(mx.prostate, file="~/Cancer/data/mx.prostate.Rdata")
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
data.urinary <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

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
year.list <- as.numeric(rownames(table(bladder$year.dx)))
age.list <-  as.numeric(rownames(table(bladder$age.dx.cat)))
dead <- by(bladder$dead, list(bladder$age.dx.cat, bladder$year.dx, bladder$sex, bladder$stage), sum)
exposure <- by(bladder$surv.months/12, list(bladder$age.dx.cat, bladder$year.dx, bladder$sex, bladder$stage), sum)
mx.bladder <- dead/exposure
prop.bladder <- apply(table(bladder$year.dx, bladder$sex, bladder$stage),c(1,2),function(x) prop.table(x))
save(mx.bladder, file="~/Cancer/data/mx.bladder.Rdata")
save(prop.bladder, file="~/Cancer/data/prop.bladder.Rdata") 
print(paste("completed bladder",date()))

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
year.list <- as.numeric(rownames(table(kidney$year.dx)))
age.list <-  as.numeric(rownames(table(kidney$age.dx.cat)))
dead <- by(kidney$dead, list(kidney$age.dx.cat, kidney$year.dx, kidney$sex, kidney$stage), sum)
exposure <- by(kidney$surv.months/12, list(kidney$age.dx.cat, kidney$year.dx, kidney$sex, kidney$stage), sum)
mx.kidney <- dead/exposure
prop.kidney <- apply(table(kidney$year.dx, kidney$sex, kidney$stage),c(1,2),function(x) prop.table(x))
save(mx.kidney, file="~/Cancer/data/mx.kidney.Rdata")
save(prop.kidney, file="~/Cancer/data/prop.kidney.Rdata") 
print(paste("completed kidney",date()))

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
data.other <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

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
year.list <- as.numeric(rownames(table(melanoma$year.dx)))
age.list <-  as.numeric(rownames(table(melanoma$age.dx.cat)))
dead <- by(melanoma$dead, list(melanoma$age.dx.cat, melanoma$year.dx, melanoma$sex, melanoma$stage), sum)
exposure <- by(melanoma$surv.months/12, list(melanoma$age.dx.cat, melanoma$year.dx, melanoma$sex, melanoma$stage), sum)
mx.melanoma <- dead/exposure
prop.melanoma <- apply(table(melanoma$year.dx, melanoma$sex, melanoma$stage),c(1,2),function(x) prop.table(x))
save(mx.melanoma, file="~/Cancer/data/mx.melanoma.Rdata")
save(prop.melanoma, file="~/Cancer/data/prop.melanoma.Rdata") 
print(paste("completed melanoma",date()))

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
year.list <- as.numeric(rownames(table(headneck$year.dx)))
age.list <-  as.numeric(rownames(table(headneck$age.dx.cat)))
dead <- by(headneck$dead, list(headneck$age.dx.cat, headneck$year.dx, headneck$sex, headneck$stage), sum)
exposure <- by(headneck$surv.months/12, list(headneck$age.dx.cat, headneck$year.dx, headneck$sex, headneck$stage), sum)
mx.headneck <- dead/exposure
prop.headneck <- apply(table(headneck$year.dx, headneck$sex, headneck$stage),c(1,2),function(x) prop.table(x))
save(mx.headneck, file="~/Cancer/data/mx.headneck.Rdata")
save(prop.headneck, file="~/Cancer/data/prop.headneck.Rdata")
print(paste("completed headneck",date()))

data.brain <- subset(data.other, site.recode %in% c(31010))
drop <- which(data.brain$surv.months==9999 | data.brain$stage==9 | data.brain$age.dx < 40 | data.brain$age.dx==999)
brain <- data.brain[-drop,]
brain$sex[brain$sex==1] <- "male"
brain$sex[brain$sex==2] <- "female"
brain$dead[brain$vital.status==1] <- 0
brain$dead[brain$vital.status==4] <- 1
brain$dead[brain$surv.months >= 120] <- 0
brain$surv.months <- ifelse(brain$surv.months<=120,brain$surv.months,120)
brain$age.dx.cat <- 5*floor(brain$age.dx/5)
brain$age.dx.cat[brain$age.dx.cat>=100] <- 100
brain$stage[brain$stage==0] <- "0. in situ"
brain$stage[brain$stage==1] <- "1. localized"
brain$stage[brain$stage==2] <- "2. regional"
brain$stage[brain$stage==4] <- "4. distant"
brain$stage[brain$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(brain$year.dx)))
age.list <-  as.numeric(rownames(table(brain$age.dx.cat)))
dead <- by(brain$dead, list(brain$age.dx.cat, brain$year.dx, brain$sex, brain$stage), sum)
exposure <- by(brain$surv.months/12, list(brain$age.dx.cat, brain$year.dx, brain$sex, brain$stage), sum)
mx.brain <- dead/exposure
prop.brain <- apply(table(brain$year.dx, brain$sex, brain$stage),c(1,2),function(x) prop.table(x))
save(mx.brain, file="~/Cancer/data/mx.brain.Rdata")
save(prop.brain, file="~/Cancer/data/prop.brain.Rdata") 
print(paste("completed brain",date()))

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
vital.status <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.lymyleuk, 1, function(x) substr(x,301,304)))
data.lymyleuk <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,vital.status=vital.status,surv.months=surv.months))

data.lymphoma <- subset(data.lymyleuk, site.recode %in% c(33011,33012,33041,33042))
drop <- which(data.lymphoma$surv.months==9999 | data.lymphoma$stage==9 | data.lymphoma$age.dx < 40 | data.lymphoma$age.dx==999)
lymphoma <- data.lymphoma[-drop,]
lymphoma$sex[lymphoma$sex==1] <- "male"
lymphoma$sex[lymphoma$sex==2] <- "female"
lymphoma$dead[lymphoma$vital.status==1] <- 0
lymphoma$dead[lymphoma$vital.status==4] <- 1
lymphoma$dead[lymphoma$surv.months >= 120] <- 0
lymphoma$surv.months <- ifelse(lymphoma$surv.months<=120,lymphoma$surv.months,120)
lymphoma$age.dx.cat <- 5*floor(lymphoma$age.dx/5)
lymphoma$age.dx.cat[lymphoma$age.dx.cat>=100] <- 100
lymphoma$stage[lymphoma$stage==0] <- "0. in situ"
lymphoma$stage[lymphoma$stage==1] <- "1. localized"
lymphoma$stage[lymphoma$stage==2] <- "2. regional"
lymphoma$stage[lymphoma$stage==4] <- "4. distant"
lymphoma$stage[lymphoma$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(lymphoma$year.dx)))
age.list <-  as.numeric(rownames(table(lymphoma$age.dx.cat)))
dead <- by(lymphoma$dead, list(lymphoma$age.dx.cat, lymphoma$year.dx, lymphoma$sex, lymphoma$stage), sum)
exposure <- by(lymphoma$surv.months/12, list(lymphoma$age.dx.cat, lymphoma$year.dx, lymphoma$sex, lymphoma$stage), sum)
mx.lymphoma <- dead/exposure
prop.lymphoma <- apply(table(lymphoma$year.dx, lymphoma$sex, lymphoma$stage),c(1,2),function(x) prop.table(x))
save(mx.lymphoma, file="~/Cancer/data/mx.lymphoma.Rdata")
save(prop.lymphoma, file="~/Cancer/data/prop.lymphoma.Rdata") 
print(paste("completed lymphoma",date()))

data.leukemia <- subset(data.lymyleuk, site.recode %in% c(35011,35012,35013,35021,35031,35022,35023,35041,35043))
drop <- which(data.leukemia$surv.months==9999 | data.leukemia$stage==9 | data.leukemia$age.dx < 40 | data.leukemia$age.dx==999)
leukemia <- data.leukemia[-drop,]
leukemia$sex[leukemia$sex==1] <- "male"
leukemia$sex[leukemia$sex==2] <- "female"
leukemia$dead[leukemia$vital.status==1] <- 0
leukemia$dead[leukemia$vital.status==4] <- 1
leukemia$dead[leukemia$surv.months >= 120] <- 0
leukemia$surv.months <- ifelse(leukemia$surv.months<=120,leukemia$surv.months,120)
leukemia$age.dx.cat <- 5*floor(leukemia$age.dx/5)
leukemia$age.dx.cat[leukemia$age.dx.cat>=100] <- 100
leukemia$stage[leukemia$stage==0] <- "0. in situ"
leukemia$stage[leukemia$stage==1] <- "1. localized"
leukemia$stage[leukemia$stage==2] <- "2. regional"
leukemia$stage[leukemia$stage==4] <- "4. distant"
leukemia$stage[leukemia$stage==4] <- "8. localized.regional"
year.list <- as.numeric(rownames(table(leukemia$year.dx)))
age.list <-  as.numeric(rownames(table(leukemia$age.dx.cat)))
dead <- by(leukemia$dead, list(leukemia$age.dx.cat, leukemia$year.dx, leukemia$sex, leukemia$stage), sum)
exposure <- by(leukemia$surv.months/12, list(leukemia$age.dx.cat, leukemia$year.dx, leukemia$sex, leukemia$stage), sum)
mx.leukemia <- dead/exposure
prop.leukemia <- apply(table(leukemia$year.dx, leukemia$sex, leukemia$stage),c(1,2),function(x) prop.table(x))
save(mx.leukemia, file="~/Cancer/data/mx.leukemia.Rdata")
save(prop.leukemia, file="~/Cancer/data/prop.leukemia.Rdata") 
print(paste("completed leukemia",date()))
