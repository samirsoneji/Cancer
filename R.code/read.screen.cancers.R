rm(list=ls())
setwd("~/Dropbox/Cancer/Value/")
library(plyr)
library(abind)

## ###############
## #Breast Cancer#
## ###############
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
drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$stage==0 | data.breast$age.dx < 40 | data.breast$age.dx==999) #drop in situ (4/8/15)
breast <- data.breast[-drop,]
breast$dead[breast$vital.status==1] <- 0
breast$dead[breast$vital.status==4] <- 1
breast$dead[breast$surv.months >= 120] <- 0
breast$surv.months <- ifelse(breast$surv.months<=120,breast$surv.months,120)
breast$age.dx.cat <- 5*floor(breast$age.dx/5)
breast$age.dx.cat[breast$age.dx.cat>=100] <- 100
levels(breast$stage) <- c(NA,"1. localized","2. regional","4. distant",NA)
breast$size[breast$size.eod13.path.op %in% c("1","2")] <- "<1cm"
breast$size[breast$size.eod13.path.op %in% c("3")] <- "1-2cm"
breast$size[breast$size.eod13.path.op %in% c("4","5","6")] <- "2-5cm"
breast$size[breast$size.eod13.path.op %in% c("7","8")] <- "5+cm"
breast$size[breast$size.eod4 %in% 3:9] <- "<1cm"
breast$size[breast$size.eod4 %in% 10:19] <- "1-2cm"
breast$size[breast$size.eod4 %in% 20:49] <- "2-5cm"
breast$size[breast$size.eod4 %in% 50:97] <- "5+cm"
breast$size[breast$size.eod10 %in% 3:9] <- "<1cm"
breast$size[breast$size.eod10 %in% 10:19] <- "1-2cm"
breast$size[breast$size.eod10 %in% 20:49] <- "2-5cm"
breast$size[breast$size.eod10 %in% 50:990] <- "5+cm"
breast$size[breast$size.cs %in% c(1:9,991)] <- "<1cm"
breast$size[breast$size.cs %in% c(10:19,992)] <- "1-2cm"
breast$size[breast$size.cs %in% c(21:49,993:995)] <- "2-5cm"
breast$size[breast$size.cs %in% c(51:998)] <- "5+cm"
breast$cod2[breast$cod==26000] <- "breast"
breast$cod2[breast$cod>=20010 & breast$cod<=50300 & breast$cod!=26000] <- "other"
number.breast <- nrow(breast)

dead <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
dead.cause <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$stage, breast$cod2), sum)
exposure <- by(breast$surv.months/12, list(breast$age.dx.cat, breast$year.dx, breast$stage), sum)
mx.breast <- dead/exposure
mx.breast.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.breast <- prop.table(as.table(table(breast$year.dx, breast$stage)),1)
mx.breast.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.breast <- prop.table(table(breast$age.dx.cat[breast$year==1987]))
save(stand.breast, number.breast, mx.breast, mx.breast.cause, mx.breast.overall, file="~/Desktop/Cancer/data/mx.breast.Rdata")
save(prop.breast, file="~/Desktop/Cancer/data/prop.breast.Rdata") 
print(paste("completed breast",date()))

dead <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$size), sum)
dead.cause <- by(breast$dead, list(breast$age.dx.cat, breast$year.dx, breast$size, breast$cod2), sum)
exposure <- by(breast$surv.months/12, list(breast$age.dx.cat, breast$year.dx, breast$size), sum)
mx.breast <- dead/exposure
mx.breast.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.breast <- prop.table(as.table(table(breast$year.dx, breast$size)),1)
mx.breast.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.breast <- prop.table(table(breast$age.dx.cat[breast$year==1987]))
save(stand.breast, number.breast, mx.breast, mx.breast.cause, mx.breast.overall, prop.breast, file="~/Desktop/Cancer/data/mx.breast.size.Rdata")

## ###################
## #Colorectal Cancer#
## ###################
seer.crc <- read.fwf("data/SEER_data/SEER_1973_2012_TEXTDATA/incidence/yr1973_2012.seer9/COLRECT.txt",widths=348)
age.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.crc, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.crc, 1, function(x) substr(x,24,24)))
coding.system <- as.numeric(apply(seer.crc, 1, function(x) substr(x,92,92)))
size.eod2 <- apply(seer.crc, 1, function(x) substr(x,86,87))
size.eod4 <- as.numeric(apply(seer.crc, 1, function(x) substr(x,88,89)))
size.eod13.clinical <- apply(seer.crc, 1, function(x) substr(x,73,73))
size.eod13.path.op <- apply(seer.crc, 1, function(x) substr(x,74,74))
size.eod10 <- as.numeric(apply(seer.crc, 1, function(x) substr(x,61,63)))
size.cs <- as.numeric(apply(seer.crc, 1, function(x) substr(x,96,98)))
stage <- as.numeric(apply(seer.crc, 1, function(x) substr(x,236,236)))
ajcc3 <- as.numeric(apply(seer.crc, 1, function(x) substr(x,237,238)))
vital.status <- as.numeric(apply(seer.crc, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.crc, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.crc, 1, function(x) substr(x,255,259)))
data.crc <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,sex=sex,stage=stage,
                             coding.system=coding.system,size.eod2=size.eod2,size.eod4=size.eod4,
                                size.eod13.clinical=size.eod13.clinical,size.eod13.path.op=size.eod13.path.op,
                                size.eod10=size.eod10,size.cs=size.cs,ajcc3=ajcc3,
                             vital.status=vital.status,surv.months=surv.months,cod=cod))
data.crc$surv.months <- as.numeric(as.character(data.crc$surv.months))
data.crc$age.dx <- as.numeric(as.character(data.crc$age.dx))
data.crc$size.eod4 <- as.numeric(as.character(data.crc$size.eod4))
data.crc$size.eod10 <- as.numeric(as.character(data.crc$size.eod10))
data.crc$cod <- as.numeric(as.character(data.crc$cod))
data.crc$sex <- as.numeric(as.character(data.crc$sex))
data.crc$vital.status <- as.numeric(as.character(data.crc$vital.status))
drop <- which(data.crc$surv.months==9999 | data.crc$stage==9 | data.crc$stage==0 | data.crc$age.dx < 40 | data.crc$age.dx==999)
crc <- data.crc[-drop,]
crc$sex[crc$sex==1] <- "male"
crc$sex[crc$sex==2] <- "female"
crc$dead[crc$vital.status==1] <- 0
crc$dead[crc$vital.status==4] <- 1
crc$dead[crc$surv.months >= 120] <- 0
crc$surv.months <- ifelse(crc$surv.months<=120,crc$surv.months,120)
crc$age.dx.cat <- 5*floor(crc$age.dx/5)
crc$age.dx.cat[crc$age.dx.cat>=100] <- 100
levels(crc$stage) <- c(NA,"1. localized","2. regional","4. distant",NA)
levels(crc$ajcc3) <- c(NA,"I","II","III","IV",NA,NA,NA)
crc$size[crc$size.eod13.path.op %in% c("1","2")] <- "<1cm"
crc$size[crc$size.eod13.path.op %in% c("3")] <- "1-2cm"
crc$size[crc$size.eod13.path.op %in% c("4","5","6")] <- "2-5cm"
crc$size[crc$size.eod13.path.op %in% c("7","8")] <- "5+cm"
crc$size[crc$size.eod4 %in% 3:9] <- "<1cm"
crc$size[crc$size.eod4 %in% 10:19] <- "1-2cm"
crc$size[crc$size.eod4 %in% 20:49] <- "2-5cm"
crc$size[crc$size.eod4 %in% 50:97] <- "5+cm"
crc$size[crc$size.eod10 %in% 3:9] <- "<1cm"
crc$size[crc$size.eod10 %in% 10:19] <- "1-2cm"
crc$size[crc$size.eod10 %in% 20:49] <- "2-5cm"
crc$size[crc$size.eod10 %in% 50:990] <- "5+cm"
crc$size[crc$size.cs %in% c(1:9,991)] <- "<1cm"
crc$size[crc$size.cs %in% c(10:19,992)] <- "1-2cm"
crc$size[crc$size.cs %in% c(21:49,993:995)] <- "2-5cm"
crc$size[crc$size.cs %in% c(51:998)] <- "5+cm"
crc$cod2[crc$cod%in% c(21040,21050)] <- "crc"
crc$cod2[crc$cod>=20010 & crc$cod<=50300 & crc$cod!=21040 & crc$cod!=21050] <- "other"
number.crc <- nrow(crc)

dead <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$stage), sum)
dead.cause <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$stage, crc$cod2), sum)
exposure <- by(crc$surv.months/12, list(crc$age.dx.cat, crc$year.dx, crc$stage), sum)
mx.crc <- dead/exposure
mx.crc.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.crc <- apply(table(crc$year.dx,crc$stage),c(1),function(x) x/sum(x,na.rm=TRUE))
stand.crc <- prop.table(table(crc$age.dx.cat[crc$year==1987]))
save(stand.crc, mx.crc, mx.crc.cause, file="~/Desktop/Cancer/data/mx.crc.Rdata")
save(prop.crc, file="~/Desktop/Cancer/data/prop.crc.Rdata") 
print(paste("completed crc",date()))

dead <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$ajcc3), sum)
dead.cause <- by(crc$dead, list(crc$age.dx.cat, crc$year.dx, crc$ajcc3, crc$cod2), sum)
exposure <- by(crc$surv.months/12, list(crc$age.dx.cat, crc$year.dx, crc$ajcc3), sum)
mx.crc <- dead/exposure
mx.crc.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.crc <- prop.table(as.table(table(crc$year.dx, crc$ajcc3)),1)
mx.crc.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.crc <- prop.table(table(crc$age.dx.cat[crc$year==1987]))
save(stand.crc, number.crc, mx.crc, mx.crc.cause, mx.crc.overall, prop.crc, file="~/Desktop/Cancer/data/mx.crc.ajcc3.Rdata")
 
###############################################
#Female Genital Cancers: Ovary, Uterus, Cervix#
###############################################
seer.femgen <- read.fwf("data/SEER_data/SEER_1973_2012_TEXTDATA/incidence/yr1973_2012.seer9/FEMGEN.txt",widths=348)
cancer <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,24,24)))
coding.system <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,92,92)))
size.eod2 <- apply(seer.femgen, 1, function(x) substr(x,86,87))
size.eod4 <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,88,89)))
size.eod13 <- apply(seer.femgen, 1, function(x) substr(x,73,74))
size.eod10 <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,61,63)))
size.cs <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,96,98)))
stage <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.femgen, 1, function(x) substr(x,255,259)))
data.femgen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage,
                                 coding.system=coding.system,size.eod2=size.eod2,size.eod4=size.eod4,
                                size.eod13.=size.eod13,
                                size.eod10=size.eod10,size.cs=size.cs,
                                vital.status=vital.status,surv.months=surv.months,cod=cod))

#Note: CIN III and carcinoma of the cervix in situ are no longer reportable to NPCR or CoC and are not reportable for SEER starting with cases diagnosed after 1/1/1996
#Source: http://seer.cancer.gov/registrars/data-collection.html
data.cervix <- subset(data.femgen, site.recode %in% c(27010))
data.cervix$surv.months <- as.numeric(as.character(data.cervix$surv.months))
data.cervix$age.dx <- as.numeric(as.character(data.cervix$age.dx))
data.cervix$size.eod4 <- as.numeric(as.character(data.cervix$size.eod4))
data.cervix$size.eod10 <- as.numeric(as.character(data.cervix$size.eod10))
data.cervix$size.cs <- as.numeric(as.character(data.cervix$size.cs))
data.cervix$cod <- as.numeric(as.character(data.cervix$cod))
drop <- which(data.cervix$surv.months==9999 | data.cervix$stage==9 | data.cervix$age.dx < 40 | data.cervix$age.dx==999 | data.cervix$stage==0)
cervix <- data.cervix[-drop,]
cervix$dead[cervix$vital.status==1] <- 0
cervix$dead[cervix$vital.status==4] <- 1
cervix$dead[cervix$surv.months >= 120] <- 0
cervix$surv.months <- ifelse(cervix$surv.months<=120,cervix$surv.months,120)
cervix$age.dx.cat <- 5*floor(cervix$age.dx/5)
cervix$age.dx.cat[cervix$age.dx.cat>=100] <- 100
levels(cervix$stage) <- c(NA,"1. localized","2. regional","4. distant",NA)
## cervix$size[cervix$size.eod13 %in% c("01","02","03","04","05","06")] <- "<7mm"
## cervix$size[cervix$size.eod13 %in% c("07","08","09",as.character(10:39))] <- "7mm-4cm"
## cervix$size[cervix$size.eod13 %in% as.character(40:98)] <- "4+cm"
## cervix$size[cervix$size.eod4>=2 & cervix$size.eod4<=6] <- "<7mm"
## cervix$size[cervix$size.eod4>=7 & cervix$size.eod4<=39] <- "7mm-4cm"
## cervix$size[cervix$size.eod4>=40 & cervix$size.eod4<=97] <- "4+cm"
## cervix$size[cervix$size.eod10>=2 & cervix$size.eod10<=6] <- "<7mm"
## cervix$size[cervix$size.eod10>=7 & cervix$size.eod10<=39] <- "7mm-4cm"
## cervix$size[cervix$size.eod10>=40 & cervix$size.eod10<=990] <- "4+cm"
## cervix$size[cervix$size.cs %in% c(1:6,991)] <- "<7mm"
## cervix$size[cervix$size.cs %in% c(7:39,992:994)] <- "7mm-4cm"
## cervix$size[cervix$size.cs %in% c(40:989,995)] <- "4+cm"
cervix$size[cervix$size.eod13 %in% c("01","02","03","04","05","06","07","08","09",as.character(10:39))] <- "<4cm"
cervix$size[cervix$size.eod13 %in% as.character(40:98)] <- "4+cm"
cervix$size[cervix$size.eod4>=2 & cervix$size.eod4<=39] <- "<4cm"
cervix$size[cervix$size.eod4>=40 & cervix$size.eod4<=97] <- "4+cm"
cervix$size[cervix$size.eod10>=2 & cervix$size.eod10<=39] <- "<4cm"
cervix$size[cervix$size.eod10>=40 & cervix$size.eod10<=990] <- "4+cm"
cervix$size[cervix$size.cs %in% c(1:39,991,992,993,994)] <- "<4cm"
cervix$size[cervix$size.cs %in% c(40:989,995)] <- "4+cm"
cervix$cod2[cervix$cod==27010] <- "cervix"
cervix$cod2[cervix$cod>=20010 & cervix$cod<=50300 & cervix$cod!=27010] <- "other"
number.cervix <- nrow(cervix)

dead <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage), sum)
dead.cause <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage, cervix$cod2), sum)
exposure <- by(cervix$surv.months/12, list(cervix$age.dx.cat, cervix$year.dx, cervix$stage), sum)
mx.cervix <- dead/exposure
mx.cervix.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.cervix <- prop.table(as.table(table(cervix$year.dx, cervix$stage)),1)
mx.cervix.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.cervix <- prop.table(table(cervix$age.dx.cat[cervix$year==1987]))
save(stand.cervix, number.cervix, mx.cervix, mx.cervix.cause, mx.cervix.overall, file="~/Desktop/Cancer/data/mx.cervix.Rdata")
save(prop.cervix, file="~/Desktop/Cancer/data/prop.cervix.Rdata") 
print(paste("completed cervix",date()))
 
dead <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$size), sum)
dead.cause <- by(cervix$dead, list(cervix$age.dx.cat, cervix$year.dx, cervix$size, cervix$cod2), sum)
exposure <- by(cervix$surv.months/12, list(cervix$age.dx.cat, cervix$year.dx, cervix$size), sum)
mx.cervix <- dead/exposure
mx.cervix.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.cervix <- prop.table(as.table(table(cervix$year.dx, cervix$size)),1)
mx.cervix.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.cervix <- prop.table(table(cervix$age.dx.cat[cervix$year==1987]))
save(stand.cervix, number.cervix, mx.cervix, mx.cervix.cause, mx.cervix.overall, prop.cervix, file="~/Desktop/Cancer/data/mx.cervix.size.Rdata")
 
################################
#Male Genital Cancers: Prostate#
################################
seer.malegen <- read.fwf("data/SEER_data/SEER_1973_2012_TEXTDATA/incidence/yr1973_2012.seer9/MALEGEN.txt",widths=348)
cancer <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,44,46))) #full position is [43,46]; removed position 43, which is "C" for cancer
site.recode <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,199,203)))
age.dx <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,24,24)))
grade <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,58,58)))
coding.system <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,92,92)))
size.eod2 <- apply(seer.malegen, 1, function(x) substr(x,86,87))
size.eod4 <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,88,89)))
size.eod13.clinical <- apply(seer.malegen, 1, function(x) substr(x,73,73))
size.eod13.path.op <- apply(seer.malegen, 1, function(x) substr(x,74,74))
size.eod10 <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,61,63)))
size.cs <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,96,98)))
ajcc  <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,134,135)))
stage <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,236,236)))
ajcc3 <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,237,238)))
ajcc3.mod <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,239,240)))
eod13 <- apply(seer.malegen, 1, function(x) substr(x,73,85))
eod13.distant <- apply(seer.malegen, 1, function(x) substr(x,85,85))
eod2 <- apply(seer.malegen, 1, function(x) substr(x,86,87))
eod4 <- apply(seer.malegen, 1, function(x) substr(x,88,91))
eod4.extension <- apply(seer.malegen, 1, function(x) substr(x,90,90))
coding.system <- apply(seer.malegen, 1, function(x) substr(x,92,92))
vital.status <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer.malegen, 1, function(x) substr(x,255,259)))
data.malegen <- data.frame(cbind(cancer=cancer,site.recode=site.recode,age.dx=age.dx,year.dx=year.dx,sex=sex,
                                stage=stage, coding.system=coding.system,size.eod2=size.eod2,size.eod4=size.eod4,
                                size.eod13.clinical=size.eod13.clinical,size.eod13.path.op=size.eod13.path.op,
                                size.eod10=size.eod10,size.cs=size.cs,ajcc=ajcc,grade=grade,
                                 ajcc3=ajcc3,ajcc3.mod=ajcc3.mod,eod13=eod13,eod13.distant=eod13.distant,eod2=eod2,eod4=eod4,
                                 eod4.extension=eod4.extension,
                                 coding.system=coding.system,
                                 vital.status=vital.status,surv.months=surv.months,cod=cod))
data.malegen$site.recode <- as.numeric(as.character(data.malegen$site.recode))
data.prostate <- subset(data.malegen, site.recode %in% c(28010))
data.prostate$surv.months <- as.numeric(as.character(data.prostate$surv.months))
data.prostate$age.dx <- as.numeric(as.character(data.prostate$age.dx))
data.prostate$size.eod4 <- as.numeric(as.character(data.prostate$size.eod4))
data.prostate$size.eod10 <- as.numeric(as.character(data.prostate$size.eod10))
data.prostate$cod <- as.numeric(as.character(data.prostate$cod))
drop <- which(data.prostate$surv.months==9999 | data.prostate$stage==9 |
              data.prostate$age.dx %in% as.character(0:39) | data.prostate$age.dx==999)
prostate <- data.prostate[-drop,]
levels(prostate$sex) <- "male"
prostate$dead[prostate$vital.status=="1"] <- 0
prostate$dead[prostate$vital.status=="4"] <- 1
prostate$dead[as.numeric(as.character(prostate$surv.months)) >= 120] <- 0
prostate$surv.months <- ifelse(as.numeric(as.character(prostate$surv.months))<=120,
                               as.numeric(as.character(prostate$surv.months)),120)
prostate$age.dx.cat <- 5*floor(as.numeric(as.character(prostate$age.dx))/5)
prostate$age.dx.cat[as.numeric(as.character(prostate$age.dx.cat))>=100] <- 100
levels(prostate$stage) <- c("0. in situ","1. localized","2. regional","4. distant","8. localized.regional",NA)
prostate$ajcc.stage <- prostate$ajcc3.mod
levels(prostate$ajcc.stage) <- c(NA,"I","II","III","IV",NA,NA)
prostate$cod2[prostate$cod==28010] <- "prostate"
prostate$cod2[prostate$cod>=20010 & prostate$cod<=50300 & prostate$cod!=28010] <- "other"
prostate$stage <- as.character(prostate$stage)
prostate$stage[which(prostate$year.dx %in% 1973:1977 & prostate$coding.system %in% c(0,1))] <- "8. localized.regional"
prostate$stage[which(prostate$year.dx %in% 1973:1977 & prostate$coding.system %in% c(0,1) & prostate$eod2 %in% c("&1","&2","&3","&6","&7","&8"))] <- "4. distant"
prostate$stage[which(prostate$year.dx %in% 1973:1982 & prostate$coding.system %in% c(2))] <- "8. localized.regional"
prostate$stage[which(prostate$year.dx %in% 1973:1982 & prostate$coding.system %in% c(2) & prostate$eod13.distant %in% c(as.character(1:9),"&"))] <- "4. distant"
number.prostate <- nrow(prostate)
prostate$stage[which(prostate$year.dx %in% 1983:1987 & prostate$coding.system %in% c(3))] <- "8. localized.regional"
prostate$stage[which(prostate$year.dx %in% 1983:1987 & prostate$coding.system %in% c(3) & prostate$eod4.extension %in% as.character(7:8))] <- "4. distant"
prostate$stage[which(prostate$year.dx %in% 1988:2003)] <- "8. localized.regional"
prostate$stage[which(prostate$year.dx %in% 1988:2003 & prostate$ajcc3 %in% c(0))] <- "0. in situ"
prostate$stage[which(prostate$year.dx %in% 1988:2003 & prostate$ajcc3 %in% c(40,41,42,49))] <- "4. distant"
prostate$size[prostate$size.eod13.clinical %in% c("1","2") | prostate$size.eod13.path.op %in% c("1","2")] <- "<1cm"
prostate$size[prostate$size.eod13.clinical %in% c("3") | prostate$size.eod13.path.op %in% c("3")] <- "1-2cm"
prostate$size[prostate$size.eod13.clinical %in% c("4","5","6") | prostate$size.eod13.path.op %in% c("4","5","6")] <- "2-5cm"
prostate$size[prostate$size.eod13.clinical %in% c("7","8") | prostate$size.eod13.path.op %in% c("7","8")] <- "5+cm"
prostate$size[prostate$size.eod4 %in% 3:9] <- "<1cm"
prostate$size[prostate$size.eod4 %in% 10:19] <- "1-2cm"
prostate$size[prostate$size.eod4 %in% 20:49] <- "2-5cm"
prostate$size[prostate$size.eod4 %in% 50:97] <- "5+cm"
prostate$size[prostate$size.eod10 %in% 3:9] <- "<1cm"
prostate$size[prostate$size.eod10 %in% 10:19] <- "1-2cm"
prostate$size[prostate$size.eod10 %in% 20:49] <- "2-5cm"
prostate$size[prostate$size.eod10 %in% 50:990] <- "5+cm"
prostate$size[prostate$size.cs %in% c(1:9,991)] <- "<1cm"
prostate$size[prostate$size.cs %in% c(10:19,992)] <- "1-2cm"
prostate$size[prostate$size.cs %in% c(21:49,993:995)] <- "2-5cm"
prostate$size[prostate$size.cs %in% c(51:998)] <- "5+cm"
levels(prostate$grade) <- 
prostate <- prostate[-which(prostate$stage=="0. in situ"),] #remove in situ prostate cancer

dead <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$ajcc.stage), sum)
dead.cause <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$ajcc.stage, prostate$cod2), sum)
exposure <- by(prostate$surv.months/12, list(prostate$age.dx.cat, prostate$year.dx, prostate$ajcc.stage), sum)
mx.prostate <- dead/exposure
mx.prostate.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.prostate <- prop.table(as.table(table(prostate$year.dx, prostate$ajcc.stage)),1)
mx.prostate.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.prostate <- prop.table(table(prostate$age.dx.cat[prostate$year==1998]))
save(stand.prostate, number.prostate, mx.prostate, mx.prostate.cause, mx.prostate.overall, file="~/Desktop/Cancer/data/mx.prostate.Rdata")
save(prop.prostate, file="~/Desktop/Cancer/data/prop.prostate.Rdata") 
print(paste("completed prostate",date()))


