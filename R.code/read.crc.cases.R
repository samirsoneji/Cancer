rm(list=ls())
setwd("~/Dropbox/Cancer/Value/")
library(plyr)
library(abind)

###################
#Colorectal Cancer#
###################
seer9.crc <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/COLRECT.txt",widths=348)
registry <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,9,18)))
age.dx <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,24,24)))
treatment <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,159,160)))
stage <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seer9.crc, 1, function(x) substr(x,255,259)))
seer9.crc.data <- data.frame(cbind(registry=registry,age.dx=age.dx,year.dx=year.dx,sex=sex,treatment=treatment,stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

seersj.crc <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1992_2011.sj_la_rg_ak/COLRECT.txt",widths=348)
registry <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,9,18)))
age.dx <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,24,24)))
treatment <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,159,160)))
stage <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seersj.crc, 1, function(x) substr(x,255,259)))
seersj.crc.data <- data.frame(cbind(registry=registry,age.dx=age.dx,year.dx=year.dx,sex=sex,treatment=treatment,stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

seerca.crc <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr2000_2011.ca_ky_lo_nj_ga/COLRECT.txt",widths=348)
registry <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,9,18)))
age.dx <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,39,42)))
sex <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,24,24)))
treatment <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,159,160)))
stage <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,301,304)))
cod <- as.numeric(apply(seerca.crc, 1, function(x) substr(x,255,259)))
seerca.crc.data <- data.frame(cbind(registry=registry,age.dx=age.dx,year.dx=year.dx,sex=sex,treatment=treatment,stage=stage,vital.status=vital.status,surv.months=surv.months,cod=cod))

data.crc <- rbind(seer9.crc.data,seersj.crc.data,seerca.crc.data)

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

crc$registry <- as.factor(crc$registry)
levels(crc$registry) <- c("San Francisco-Oakland",
                          "Connecticut",
                          "Detroit",
                          "Hawaii",
                          "Iowa",
                          "New Mexico",
                          "Seattle",
                          "Utah",
                          "Atlanta",
                          "Alaska Natives",
                          "San Jose-Monterey",
                          "Los Angeles",
                          "Rural Georgia",
                          "Greater California",
                          "Kentucky",
                          "Louisiana",
                          "New Jersey",
                          "Greater Georgia")
crc$stage <- as.factor(crc$stage)
levels(crc$stage) <- c("in situ","localized","regional","distant")



save(crc,file="~/Dropbox/Cancer/data/crc.incidence.Rdata")


###population data###
popl9 <-  read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/populations/yr1992_2011.seer9.plus.sj_la_rg_ak/19agegroups.txt",
                   widths=c(4,2,2,3,2,1,1,1,2,11))
names(popl9) <- c("year","state","statefips","countyfips","registry","race","origin","sex","age","population")
popl1118 <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/populations/yr2000_2011.ca_ky_lo_nj_ga/19agegroups.txt",
                   widths=c(4,2,2,3,2,1,1,1,2,11))
names(popl1118) <- c("year","state","statefips","countyfips","registry","race","origin","sex","age","population")

