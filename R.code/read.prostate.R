rm(list=ls())
setwd("~/Dropbox/Cancer/Value/")
library(plyr)
library(abind)

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
#levels(prostate$grade) <- c("I","II","III",NA,NA)
levels(prostate$grade) <- c("I_II","I_II","III",NA,NA)
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

dead <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$grade), sum)
dead.cause <- by(prostate$dead, list(prostate$age.dx.cat, prostate$year.dx, prostate$grade, prostate$cod2), sum)
exposure <- by(prostate$surv.months/12, list(prostate$age.dx.cat, prostate$year.dx, prostate$grade), sum)
mx.prostate <- dead/exposure
mx.prostate.cause <- aaply(dead.cause, 4, function(x) x/exposure)
prop.prostate <- prop.table(as.table(table(prostate$year.dx, prostate$grade)),1)
mx.prostate.overall <- apply(dead,c(1,2),sum,na.rm=TRUE)/apply(exposure,c(1,2),sum,na.rm=TRUE)
stand.prostate <- prop.table(table(prostate$age.dx.cat[prostate$year==1998]))
prop.died <- apply(table(prostate$year.dx,prostate$grade,prostate$dead),c(1,2),function(x) x[["1"]]/sum(x))
prop.died.prostate.prostate <- apply(table(prostate$year.dx,prostate$grade,prostate$cod2),c(1,2),function(x) x[["prostate"]]/sum(x))
save(stand.prostate, number.prostate, mx.prostate, mx.prostate.cause, mx.prostate.overall, prop.prostate, prop.died.prostate.prostate, file="~/Desktop/Cancer/data/mx.prostate.grade.Rdata")
print(paste("completed prostate",date()))


#proportion dead within 10 years
color <- brewer.pal(7,"YlGnBu")[c(3,5,7)]
par(mfrow=c(1,2))
matplot(1975:2002,100*apply(table(prostate$year.dx,prostate$grade,prostate$dead),c(1,2),function(x) x[["1"]]/sum(x))[as.character(1975:2002),],type="l",lty=1,ylab="% dead within 10 years",xlab="year",bty="l",las=1,ylim=c(0,100),col=color,lwd=2)
grid(lty=1,col="lightgrey")

matplot(1975:2002,100*apply(table(prostate$year.dx,prostate$grade,prostate$cod2),c(1,2),function(x) x[["prostate"]]/sum(x))[as.character(1975:2002),],type="l",lty=1,ylab="% dead of prostate cancer within 10 years",xlab="year",bty="l",las=1,ylim=c(0,100),col=color,lwd=2)
grid(lty=1,col="lightgrey")
