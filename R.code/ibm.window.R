rm(list=ls())
library(plyr)
load("~/Desktop/Cancer/data/breast.Rdata")

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.sens.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.sens.fxn.R")
source("~/Desktop/Cancer/R.code/results.sens.fxn.R")

seer.breast <- read.fwf("~/Dropbox/Cancer/Value/data/SEER_data/SEER_1973_2012_TEXTDATA/incidence/yr1973_2012.seer9/BREAST.txt",widths=348)
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
data.breast$age.dx.cat <- 5*floor(data.breast$age.dx/5)
data.breast$age.dx.cat[data.breast$age.dx.cat>=100] <- 100
data.breast$size[data.breast$size.eod13.path.op %in% c("1","2")] <- "<1cm"
data.breast$size[data.breast$size.eod13.path.op %in% c("3")] <- "1-2cm"
data.breast$size[data.breast$size.eod13.path.op %in% c("4")] <- "2-3cm"
data.breast$size[data.breast$size.eod13.path.op %in% c("5","6")] <- "3-5cm"
data.breast$size[data.breast$size.eod13.path.op %in% c("7","8")] <- "5+cm"
data.breast$size[data.breast$size.eod4 %in% 3:9] <- "<1cm"
data.breast$size[data.breast$size.eod4 %in% 10:19] <- "1-2cm"
data.breast$size[data.breast$size.eod4 %in% 20:29] <- "2-3cm"
data.breast$size[data.breast$size.eod4 %in% 30:49] <- "3-5cm"
data.breast$size[data.breast$size.eod4 %in% 50:97] <- "5+cm"
data.breast$size[data.breast$size.eod10 %in% 3:9] <- "<1cm"
data.breast$size[data.breast$size.eod10 %in% 10:19] <- "1-2cm"
data.breast$size[data.breast$size.eod10 %in% 20:29] <- "2-3cm"
data.breast$size[data.breast$size.eod10 %in% 30:49] <- "3-5cm"
data.breast$size[data.breast$size.eod10 %in% 50:990] <- "5+cm"
data.breast$size[data.breast$size.cs %in% c(1:9,991)] <- "<1cm"
data.breast$size[data.breast$size.cs %in% c(10:19,992)] <- "1-2cm"
data.breast$size[data.breast$size.cs %in% c(20:29,993)] <- "2-3cm"
data.breast$size[data.breast$size.cs %in% c(30:49,994:995)] <- "3-5cm"
data.breast$size[data.breast$size.cs %in% c(51:998)] <- "5+cm"
data.breast$cod2[data.breast$cod==26000] <- "data.breast"
data.breast$cod2[data.breast$cod>=20010 & data.breast$cod<=50300 & data.breast$cod!=26000] <- "other"
data.breast$cod3 <- "alive"
data.breast$cod3[data.breast$cod==26000] <- "data.breast"
data.breast$cod3[data.breast$cod>=20010 & data.breast$cod<=50300 & data.breast$cod!=26000] <- "other"
drop <- which(data.breast$surv.months==9999 | data.breast$stage==9 | data.breast$age.dx < 40 | data.breast$age.dx==999) #drop in situ (4/8/15)
breast <- data.breast[-drop,]

readjust.fxn <- function(x,categories,other.categories) {
  adjusted.categories <- sum(x[categories])
  ratio.other.categories <- x[other.categories]/sum(x[other.categories])
  adjusted.other.categories <- (1-adjusted.categories) * ratio.other.categories
  all <- c(x[categories],adjusted.other.categories)
  return(all)
}

odx.fxn <- function(scalar, scalar2, categories, categories2, prop, mx, mx.cause,cancer,year.list) {
  categories.all <- c(categories,categories2)
  other.categories <- dimnames(prop)[[2]][-which(categories.all %in% dimnames(prop)[[2]])]
  prop[,categories] <- (1-scalar) * prop[,categories]
  prop[,categories2] <- (1-scalar2) * prop[,categories2]
  prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
  for (i in 1:nrow(prop.adj))
    prop.adj[i,] <- readjust.fxn(prop[i,],categories.all,other.categories)
  dimnames(prop.adj) <- dimnames(prop)
  mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
  mx[,,categories2] <- (1-scalar2)^-1 * mx[,,categories2]
  mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
  mx.cause[,,,categories2] <- (1-scalar2)^-1 * mx.cause[,,,categories2]
  res <- results.fxn(mx,mx.cause,prop.adj,cancer,year.list)
  return(res)
}
summary.fxn <- function(x)
    c(x[3],sum(unlist(x[4:8])),sum(unlist(x[seq(14,22,2)])),sum(unlist(x[seq(15,23,2)])))

ibm.fxn <- function(datos, w, year.list) {
    datos$surv.months <- ifelse(datos$surv.months<=w*12,datos$surv.months,w*12)
    datos$dead[datos$vital.status==1] <- 0
    datos$dead[datos$vital.status==4] <- 1
    datos$dead[datos$surv.months >= w*12] <- 0
    dead <- by(datos$dead, list(datos$age.dx.cat, datos$year.dx, datos$size), sum)
    dead.cause <- by(datos$dead, list(datos$age.dx.cat, datos$year.dx, datos$size, datos$cod2), sum)
    exposure <- by(datos$surv.months/12, list(datos$age.dx.cat, datos$year.dx, datos$size), sum)
    mx.breast <- dead/exposure
    mx.breast.cause <- aaply(dead.cause, 4, function(x) x/exposure)
    prop.breast <- prop.table(as.table(table(datos$year.dx, datos$size)),1)
    decomp <- summary.fxn(odx.fxn(0.10,0.10,"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",year.list))
    return(list(mx.breast=mx.breast,mx.breast.cause=mx.breast.cause,prop.breast=prop.breast,decomp=decomp))}

decomp8 <- ibm.fxn(breast,8,c(1975,2000))
decomp9 <- ibm.fxn(breast,9,c(1975,2000))
decomp10 <- ibm.fxn(breast,10,c(1975,2000))
decomp11 <- ibm.fxn(breast,11,c(1975,2000))
decomp12 <- ibm.fxn(breast,12,c(1975,2000))

w.list <- 8:12
decomp.list <- list()
for (w in 1:length(w.list))
    decomp.list[[w]] <- ibm.fxn(breast,w.list[w],c(1975,2000))

ibm.table <- as.data.frame(matrix(NA,nrow=length(w.list),ncol=6))
ibm.table[,1] <- w.list
ibm.table[,2] <- "1975-2002"
ibm.table[,3] <- round(unlist(lapply(decomp.list,function(x) x$decomp[[1]])),2)
ibm.table[,4] <- paste(round(unlist(lapply(decomp.list,function(x) x$decomp[[2]])),2)," (",round(100*unlist(lapply(decomp.list,function(x) x$decomp[[2]]))/unlist(lapply(decomp.list,function(x) x$decomp[[1]]))),"%)",sep="")
ibm.table[,5] <- paste(round(unlist(lapply(decomp.list,function(x) x$decomp[[3]])),2)," (",round(100*unlist(lapply(decomp.list,function(x) x$decomp[[3]]))/unlist(lapply(decomp.list,function(x) x$decomp[[1]]))),"%)",sep="")
ibm.table[,6] <- paste(round(unlist(lapply(decomp.list,function(x) x$decomp[[4]])),2)," (",round(100*unlist(lapply(decomp.list,function(x) x$decomp[[4]]))/unlist(lapply(decomp.list,function(x) x$decomp[[1]]))),"%)",sep="")
colnames(ibm.table) <- c("Window (Yrs)","Study Period","Overall Gain in Life Expectancy (Yrs)","Earlier Detection (Yrs)","Advancements in Breast Cancer Treatment (Yrs)","Advancements in Treatment of Other Diseases (Yrs)")
write.csv(ibm.table,file="~/Desktop/Cancer/text/ibm.table.csv")

library(xtable)
ibm.tablex <- xtable(ibm.table)
file.name <- "~/Desktop/Cancer/text/ibm.table.tex"
sink(file.name)
print(ibm.tablex,include.rownames=FALSE,floating=FALSE,floating.environment="table",sanitize.colnames.function=function(x) {x})
sink()
