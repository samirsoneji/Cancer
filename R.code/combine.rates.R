load("~/Cancer/data/mx.breast.Rdata")
load("~/Cancer/data/prop.breast.Rdata") 

load("~/Cancer/data/mx.crc.Rdata")
load("~/Cancer/data/prop.crc.Rdata") 

load("~/Cancer/data/mx.lung.Rdata")
load("~/Cancer/data/prop.lung.Rdata") 

load("~/Cancer/data/mx.esophagus.Rdata")
load("~/Cancer/data/prop.esophagus.Rdata") 

load("~/Cancer/data/mx.stomach.Rdata")
load("~/Cancer/data/prop.stomach.Rdata") 

load("~/Cancer/data/mx.pancreas.Rdata")
load("~/Cancer/data/prop.pancreas.Rdata") 

load("~/Cancer/data/mx.cervix.Rdata")
load("~/Cancer/data/prop.cervix.Rdata") 

load("~/Cancer/data/mx.uterus.Rdata")
load("~/Cancer/data/prop.uterus.Rdata") 

load("~/Cancer/data/mx.ovary.Rdata")
load("~/Cancer/data/prop.ovary.Rdata") 

load("~/Cancer/data/mx.prostate.Rdata")
load("~/Cancer/data/prop.prostate.Rdata") 

load("~/Cancer/data/mx.bladder.Rdata")
load("~/Cancer/data/prop.bladder.Rdata") 

load("~/Cancer/data/mx.kidney.Rdata")
load("~/Cancer/data/prop.kidney.Rdata") 

load("~/Cancer/data/mx.melanoma.Rdata")
load("~/Cancer/data/prop.melanoma.Rdata") 

load("~/Cancer/data/mx.headneck.Rdata")
load("~/Cancer/data/prop.headneck.Rdata")

load("~/Cancer/data/mx.brain.Rdata")
load("~/Cancer/data/prop.brain.Rdata") 

load("~/Cancer/data/mx.lymphoma.Rdata")
load("~/Cancer/data/prop.lymphoma.Rdata") 

load("~/Cancer/data/mx.leukemia.Rdata")
load("~/Cancer/data/prop.leukemia.Rdata") 


library(foreign)
library(survival)

setwd("~/Dropbox/Cancer/Value")
source("R.code/lifetable.R")

decomp.fxn <- function(mx, prop, cancer, year.list) {
    value.list <- c("ex.overall","ex.localized","ex.regional","ex.distant","prop.localized","prop.regional","prop.distant")
    datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
    rownames(datos) <- year.list
    colnames(datos) <- value.list
    if (cancer %in% c("crc","lung","esophagus","stomach","pancreas","bladder","kidney","melanoma","headneck","brain","lymphoma","leukemia")) sex <- "dual"
    if (cancer %in% c("breast","cervix","ovary","uterus","prostate")) sex <- "single"

    if (sex=="single" & cancer!="prostate") {
        for (y in 1:length(year.list)) {
            ex.localized <- lifetab.grad.nax.nLxappr(nMx=c(rep(0,9),mx[,as.character(year.list[y]),"1. localized"]),a0=0.1385, Rdx=1)$ex[10]
            ex.regional <- lifetab.grad.nax.nLxappr(nMx=c(rep(0,9),mx[,as.character(year.list[y]),"2. regional"]),a0=0.1385, Rdx=1)$ex[10]
            ex.distant <- lifetab.grad.nax.nLxappr(nMx=c(rep(0,9),mx[,as.character(year.list[y]),"4. distant"]),a0=0.1385, Rdx=1)$ex[10]
            prop.localized <- prop[as.character(year.list[y]),"1. localized"]
            prop.regional <- prop[as.character(year.list[y]),"2. regional"]
            prop.distant <- prop[as.character(year.list[y]),"4. distant"]
            ex.overall <-ex.localized*prop.localized+ex.regional*prop.regional+ex.distant*prop.distant
            datos[y,"ex.overall"] <- ex.overall
            datos[y,"ex.localized"] <- ex.localized
            datos[y,"ex.regional"] <- ex.regional
            datos[y,"ex.distant"] <- ex.distant
            datos[y,"prop.localized"] <- prop.localized
            datos[y,"prop.regional"] <- prop.regional
            datos[y,"prop.distant"] <- prop.distant
        }
        datos <- data.frame(datos)
        
#Samir: I changed the output so it now spits 3 values for change.stage and change.ex corresponding to each cancer stage
decomp.fxn <- function(datos, year1, year2) {
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.stage <- c(diff(tmp$prop.localized)*mean(tmp$ex.localized), diff(tmp$prop.regional)*mean(tmp$ex.regional),diff(tmp$prop.distant)*mean(tmp$ex.distant)) 
    change.ex <- c(diff(tmp$ex.localized)*mean(tmp$prop.localized), diff(tmp$ex.regional)*mean(tmp$prop.regional), diff(tmp$ex.distant)*mean(tmp$prop.distant))
    return(list(ex.overall.diff=ex.overall.diff, change.stage=change.stage, change.ex=change.ex))
}
print(decomp.fxn(datos, 1981, 1991))
print(decomp.fxn(datos, 1991, 2001))
print(decomp.fxn(datos, 1981, 2001))
