setwd("~/Dropbox/Cancer/Value")
widths <- read.csv("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/var.names.widths.csv",header=FALSE)
names(widths) <- c("name","width")
breast <- read.fwf("data/SEER_data/SEER_1973_2011_TEXTDATA/incidence/yr1973_2011.seer9/BREAST.txt",widths=348)

age.dx <- as.numeric(apply(breast, 1, function(x) substr(x,25,27)))
year.dx <- as.numeric(apply(breast, 1, function(x) substr(x,39,42)))
race <- as.numeric(apply(breast, 1, function(x) substr(x,233,233)))
stage <- as.numeric(apply(breast, 1, function(x) substr(x,236,236)))
vital.status <- as.numeric(apply(breast, 1, function(x) substr(x,265,265)))
surv.months <- as.numeric(apply(breast, 1, function(x) substr(x,301,304)))

data.breast <- data.frame(cbind(age.dx=age.dx,year.dx=year.dx,race=race,stage=stage,vital.status=vital.status,surv.months=surv.months))
drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$age.dx < 40)
breast <- breast[-drop,]
breast$race[breast$race==1] <- "white"
breast$race[breast$race==2] <- "black"
breast$race[breast$race>2] <- "other"
breast$dead[breast$vital.status==1] <- 0
breast$dead[breast$vital.status==4] <- 1
breast$dead[breast$surv.months >= 120] <- 0
breast$surv.months <- ifelse(breast$surv.months<=120,breast$surv.months,120)
breast$age.dx.cat <- 5*floor(breast$age.dx/5)
breast$stage[breast$stage==0] <- "0. in situ"
breast$stage[breast$stage==1] <- "1. localized"
breast$stage[breast$stage==2] <- "2. regional"
breast$stage[breast$stage==4] <- "4. distant"
breast$stage[breast$stage==4] <- "8. localized.regional"

year.list <- as.numeric(rownames(table(breast$year.dx)))
age.list <-  as.numeric(rownames(table(breast$age.dx.cat)))

dead <- by(breast$dead, list(breast$age.dx, breast$year.dx, breast$stage), sum)
exposure <- by(breast$surv.months/12, list(breast$age.dx, breast$year.dx, breast$stage), sum)
mx <- dead/exposure

prop <- prop.table(as.table(table(breast$year.dx, breast$stage)),1)

save(mx, file="data/SEER_data/mx.breast.Rdata")
save(prop, file="data/SEER_data/prop.breast.Rdata")


