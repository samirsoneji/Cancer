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
levels(breast$stage) <- c("0. in situ","1. localized","2. regional","4. distant",NA)
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
breast$cod3 <- "alive"
breast$cod3[breast$cod==26000] <- "breast"
breast$cod3[breast$cod>=20010 & breast$cod<=50300 & breast$cod!=26000] <- "other"

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

popl <- read.fwf("~/Dropbox/Cancer/Value/data/SEER_data/SEER_1973_2012_TEXTDATA/populations/white_black_other/yr1973_2012.seer9/singleages.txt",
                 widths=c(4,2,2,3,2,1,1,1,2,11))
names(popl) <- c("year","state","statefips","countyfips","registry","race","origin","sex","age","popl")
popl$popl <- popl$popl/10

popl.array <- by(popl$popl,list(popl$year,popl$age), sum)[,as.character(40:85)]

size.counts <- table(breast$year.dx,breast$age.dx, breast$size)
size.counts <- apply(size.counts, c(1,3), function(x) c(x[1:45],sum(x[46:69])))
dimnames(size.counts)[[1]] <- 40:85
size.counts <- aperm(size.counts,c(2,1,3))

size.rate <- array(NA,dim=dim(size.counts),dimnames=dimnames(size.counts))
size.rate[,,"<1cm"] <- size.counts[,,"<1cm"]/popl.array
size.rate[,,"1-2cm"] <- size.counts[,,"1-2cm"]/popl.array
size.rate[,,"2-5cm"] <- size.counts[,,"2-5cm"]/popl.array
size.rate[,,"5+cm"] <- size.counts[,,"5+cm"]/popl.array

standard <- popl.array["2000",]/sum(popl.array["2000",])
stand.size.rate <- apply(size.rate, c(1,3), function(x) x%*%standard)

prop.died.breast.all <- apply(table(breast$year.dx,breast$size,breast$dead),c(1,2),function(x) x[["1"]]/sum(x))
prop.died.breast.breast <- apply(table(breast$year.dx,breast$size,breast$cod3),c(1,2),function(x) x[["breast"]]/sum(x))

save(stand.breast, stand.size.rate, number.breast, mx.breast, mx.breast.cause, mx.breast.overall, prop.breast, prop.died.breast.all, prop.died.breast.breast, file="~/Desktop/Cancer/data/mx.breast.size.Rdata")

save(breast, file="~/Desktop/Cancer/data/mx.breast.Rdata")
#proportion dead within 10 years
color <- brewer.pal(9,"YlGnBu")[c(3,5,7,9)]
matplot(1975:2002,100*apply(table(breast$year.dx,breast$size,breast$dead),c(1,2),function(x) x[["1"]]/sum(x))[as.character(1975:2002),],type="l",lty=1,ylab="% dead within 10 years",xlab="year",bty="l",las=1,ylim=c(0,100),col=color,lwd=2)
grid(lty=1,col="lightgrey")

matplot(1975:2002,100*apply(table(breast$year.dx,breast$size,breast$cod3),c(1,2),function(x) x[["breast"]]/sum(x))[as.character(1975:2002),],type="l",lty=1,ylab="% dead of breast cancer within 10 years",xlab="year",bty="l",las=1,ylim=c(0,100),col=color,lwd=2)
grid(lty=1,col="lightgrey")
