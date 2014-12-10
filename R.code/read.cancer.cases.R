setwd("~/Dropbox/Cancer/Value")

###############
#Breast Cancer#
###############
#seer.breast <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/BREAST.txt",widths=348)
#age.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,25,27)))
#year.dx <- as.numeric(apply(seer.breast, 1, function(x) substr(x,39,42)))
#stage <- as.numeric(apply(seer.breast, 1, function(x) substr(x,236,236)))
#vital.status <- as.numeric(apply(seer.breast, 1, function(x) substr(x,265,265)))
#surv.months <- as.numeric(apply(seer.breast, 1, function(x) substr(x,301,304)))
#data.breast <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,stage=stage,vital.status=vital.status,surv.months=surv.months))
#drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$age.dx < 40 | data.breast$age.dx==999)
#breast <- data.breast[-drop,]
#breast$dead[breast$vital.status==1] <- 0
#breast$dead[breast$vital.status==4] <- 1
#breast$dead[breast$surv.months >= 120] <- 0
#breast$surv.months <- ifelse(breast$surv.months<=120,breast$surv.months,120)
#breast$age.dx.cat <- 5*floor(breast$age.dx/5)
#breast$age.dx.cat[breast$age.dx.cat>=100] <- 100
#breast$stage[breast$stage==0] <- "0. in situ"
#breast$stage[breast$stage==1] <- "1. localized"
#breast$stage[breast$stage==2] <- "2. regional"
#breast$stage[breast$stage==4] <- "4. distant"
#breast$stage[breast$stage==4] <- "8. localized.regional"
#year.list <- as.numeric(rownames(table(breast$year.dx)))
#age.list <-  as.numeric(rownames(table(breast$age.dx.cat)))
#dead <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
#exposure <- by(breast$surv.months/12, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
#mx.breast <- dead/exposure
#prop.breast <- prop.table(as.table(table(breast$year.dx, breast$stage)),1)
#save(mx.breast, file="~/Cancer/data/mx.breast.Rdata")
#save(prop.breast, file="~/Cancer/data/prop.breast.Rdata") 

###################
#Colorectal Cancer#
###################
#seer.crc <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/COLRECT.txt",widths=348)
#age.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,25,27)))
#year.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,39,42)))
#sex <- as.numeric(apply(seer.crc, 1, function(x) substr(x,24,24)))
#stage <- as.numeric(apply(seer.crc, 1, function(x) substr(x,236,236)))
#vital.status <- as.numeric(apply(seer.crc, 1, function(x) substr(x,265,265)))
#surv.months <- as.numeric(apply(seer.crc, 1, function(x) substr(x,301,304)))
#data.crc <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,sex=sex,stage=stage,vital.status=vital.status,surv.months=surv.months))
#drop <- which(data.crc$surv.months==9999 | data.crc$stage==9 | data.crc$age.dx < 40 | data.crc$age.dx==999)
#crc <- data.crc[-drop,]
#crc$sex[crc$sex==1] <- "male"
#crc$sex[crc$sex==2] <- "female"
#crc$dead[crc$vital.status==1] <- 0
#crc$dead[crc$vital.status==4] <- 1
#crc$dead[crc$surv.months >= 120] <- 0
#crc$surv.months <- ifelse(crc$surv.months<=120,crc$surv.months,120)
#crc$age.dx.cat <- 5*floor(crc$age.dx/5)
#crc$age.dx.cat[crc$age.dx.cat>=100] <- 100
#crc$stage[crc$stage==0] <- "0. in situ"
#crc$stage[crc$stage==1] <- "1. localized"
#crc$stage[crc$stage==2] <- "2. regional"
#crc$stage[crc$stage==4] <- "4. distant"
#crc$stage[crc$stage==4] <- "8. localized.regional"
#year.list <- as.numeric(rownames(table(crc$year.dx)))
#age.list <-  as.numeric(rownames(table(crc$age.dx.cat)))
#dead <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$sex, crc$stage), sum)
#exposure <- by(crc$surv.months/12, list(crc$age.dx.cat, crc$year.dx, crc$sex, crc$stage), sum)
#mx.crc <- dead/exposure
#prop.crc <- apply(table(crc$year.dx, crc$sex, crc$stage),c(1,2),function(x) prop.table(x))
#save(mx.crc, file="~/Cancer/data/mx.crc.Rdata")
#save(prop.crc, file="~/Cancer/data/prop.crc.Rdata") 

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
lung <- c(340:349)
data.lung <- subset(data.respir, cancer %in% lung)
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
