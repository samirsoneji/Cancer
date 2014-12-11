load("~/Cancer/data/mx.breast.Rdata")
load("~/Cancer/data/prop.breast.Rdata") 

load("~/Cancer/data/mx.crc.Rdata")
load("~/Cancer/data/prop.crc.Rdata") 
prop.crc <- aperm(prop.crc,c(2,1,3))
  
load("~/Cancer/data/mx.lung.Rdata")
load("~/Cancer/data/prop.lung.Rdata") 
prop.lung <- aperm(prop.lung,c(2,1,3))

load("~/Cancer/data/mx.esophagus.Rdata")
load("~/Cancer/data/prop.esophagus.Rdata") 
prop.esophagus <- aperm(prop.esophagus,c(2,1,3))

load("~/Cancer/data/mx.stomach.Rdata")
load("~/Cancer/data/prop.stomach.Rdata") 
prop.stomach <- aperm(prop.stomach,c(2,1,3))

load("~/Cancer/data/mx.pancreas.Rdata")
load("~/Cancer/data/prop.pancreas.Rdata") 
prop.pancreas <- aperm(prop.pancreas,c(2,1,3))

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
prop.bladder <- aperm(prop.bladder,c(2,1,3))

load("~/Cancer/data/mx.kidney.Rdata")
load("~/Cancer/data/prop.kidney.Rdata") 
prop.kidney <- aperm(prop.kidney,c(2,1,3))

load("~/Cancer/data/mx.melanoma.Rdata")
load("~/Cancer/data/prop.melanoma.Rdata") 
prop.melanoma <- aperm(prop.melanoma,c(2,1,3))

load("~/Cancer/data/mx.headneck.Rdata")
load("~/Cancer/data/prop.headneck.Rdata")
prop.headneck <- aperm(prop.headneck,c(2,1,3))

load("~/Cancer/data/mx.lymphoma.Rdata")
load("~/Cancer/data/prop.lymphoma.Rdata") 
prop.lymphoma <- aperm(prop.lymphoma,c(2,1,3))

load("~/Cancer/data/mx.leukemia.Rdata")
load("~/Cancer/data/prop.leukemia.Rdata") 
prop.leukemia <- aperm(prop.leukemia,c(2,1,3))


library(foreign)
library(survival)

source("~/Cancer/R.code/lifetable.R")
source("~/Cancer/R.code/nAx.fxn.allcause.mort.r")


decomp.fxn <- function(datos, year.list) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=9))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.localized","change.stage.regional","change.stage.distant",
                      "change.ex.localized","change.ex.regional","change.ex.distant")
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.stage <- c(diff(tmp$prop.localized)*mean(tmp$ex.localized), diff(tmp$prop.regional)*mean(tmp$ex.regional),diff(tmp$prop.distant)*mean(tmp$ex.distant)) 
    change.ex <- c(diff(tmp$ex.localized)*mean(tmp$prop.localized), diff(tmp$ex.regional)*mean(tmp$prop.regional), diff(tmp$ex.distant)*mean(tmp$prop.distant))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.stage.localized","change.stage.regional","change.stage.distant")] <- change.stage
    results[y,c("change.ex.localized","change.ex.regional","change.ex.distant")] <- change.ex
  }
  return(results)
}

create.datos.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.localized","ex.regional","ex.distant","prop.localized","prop.regional","prop.distant")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.localized <- c(rep(0,9),mx[,as.character(year.list[y]),"1. localized"])
    mx.regional <- c(rep(0,9),mx[,as.character(year.list[y]),"2. regional"])
    mx.distant <- c(rep(0,9),mx[,as.character(year.list[y]),"4. distant"])
    mx.localized[which(is.na(mx.localized)==TRUE)] <- 0
    mx.regional[which(is.na(mx.regional)==TRUE)] <- 0
    mx.distant[which(is.na(mx.distant)==TRUE)] <- 0 
    ex.localized <- lifetab.grad.nax.nLxappr(nMx=mx.localized,a0=0.1385, Rdx=1)$ex[10]
    ex.regional <- lifetab.grad.nax.nLxappr(nMx=mx.regional,a0=0.1385, Rdx=1)$ex[10]
    ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant,a0=0.1385, Rdx=1)$ex[10]
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
  return(datos)
}
  
results.fxn <- function(mx, prop, cancer, year.list) {
  if (cancer %in% c("crc","lung","esophagus","stomach","pancreas","bladder","kidney","melanoma","headneck","lymphoma","leukemia")) sex <- "dual"
  if (cancer %in% c("breast","cervix","ovary","uterus","prostate")) sex <- "single"
  
  if (sex=="single" & cancer!="prostate")
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list)
  if (sex=="dual") {
    results.male <-  decomp.fxn(create.datos.fxn(mx[,,"male",], prop[,,"male"], year.list), year.list)
    results.female <-  decomp.fxn(create.datos.fxn(mx[,,"female",], prop[,,"female"], year.list), year.list)
    results <- results.male + results.female
    results[,c("year.start","year.end")] <- results.male[,c("year.start","year.end")]
  }
     
    return(results)
}

breast <- results.fxn(mx.breast, prop.breast, "breast", c(1981,1991,2001))
crc <- results.fxn(mx.crc, prop.crc, "crc", c(1981,1991,2001)) 
