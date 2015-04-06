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
prop.cervix <- t(prop.cervix)

load("~/Cancer/data/mx.uterus.Rdata")
load("~/Cancer/data/prop.uterus.Rdata") 
prop.uterus <- t(prop.uterus)

load("~/Cancer/data/mx.ovary.Rdata")
load("~/Cancer/data/prop.ovary.Rdata") 
prop.ovary <- t(prop.ovary)

load("~/Cancer/data/mx.prostate.Rdata")
load("~/Cancer/data/prop.prostate.Rdata") 
prop.prostate <- t(prop.prostate)

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

#load("~/Cancer/data/mx.leukemia.Rdata")
#load("~/Cancer/data/prop.leukemia.Rdata") 
#prop.leukemia <- aperm(prop.leukemia,c(2,1,3))

library(foreign)
library(survival)
source("~/Cancer/R.code/lifetable.R")
source("~/Cancer/R.code/nAx.fxn.allcause.mort.r")
source("~/Cancer/R.code/nAx.fxn.R")
source("~/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Cancer/R.code/Assoc_LT.r")

nMx1 <- rbind(matrix(0,nrow=9,ncol=2),t(mx.breast.cause[,,"1980","1. localized"]))
nMx2 <- rbind(matrix(0,nrow=9,ncol=2),t(mx.breast.cause[,,"1990","1. localized"]))
a01=0.1385
a11=1.6325
a12=2.2025
a02=0.1385
a21=1.6325
a22=2.2025
Rx <- 1
breast.decomp.80.90 <- decomp.ex.cd(nMx1, a01, a11, a12, Rx, nMx2, a02, a21, a22)

nMx1 <- rbind(matrix(0,nrow=9,ncol=2),t(mx.breast.cause[,,"1980","2. regional"]))
nMx2 <- rbind(matrix(0,nrow=9,ncol=2),t(mx.breast.cause[,,"1990","2. regional"]))
a01=0.1385
a11=1.6325
a12=2.2025
a02=0.1385
a21=1.6325
a22=2.2025
Rx <- 1
breast.decomp.80.90 <- decomp.ex.cd(nMx1, a01, a11, a12, Rx, nMx2, a02, a21, a22)


decomp.fxn <- function(datos, year.list, mx.cause) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=15))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.localized","change.stage.regional","change.stage.distant",
                      "change.ex.localized","change.ex.regional","change.ex.distant",
                      "change.ex.localized.cancer","change.ex.localized.other",
                      "change.ex.regional.cancer","change.ex.regional.other",
                      "change.ex.distant.cancer","change.ex.distant.other")
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
    results[y,c("change.ex.localized.cancer","change.ex.localized.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"1. localized"])),
                     a01=0.1385, a11=1.6325, a12=2.2025, Rx=1,
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"1. localized"])),
                     a02=0.1385, a21=1.6325, a22=2.2025)$Decomposition[1:2,2]*mean(datos$prop.localized[y:(y+1)])
    results[y,c("change.ex.regional.cancer","change.ex.regional.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"2. regional"])),
                     a01=0.1385, a11=1.6325, a12=2.2025, Rx=1,
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"2. regional"])),
                     a02=0.1385, a21=1.6325, a22=2.2025)$Decomposition[1:2,2]*mean(datos$prop.regional[y:(y+1)])
    results[y,c("change.ex.distant.cancer","change.ex.distant.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"4. distant"])),
                     a01=0.1385, a11=1.6325, a12=2.2025, Rx=1,
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"4. distant"])),
                     a02=0.1385, a21=1.6325, a22=2.2025)$Decomposition[1:2,2]*mean(datos$prop.distant[y:(y+1)])
  }
  return(results)
}

decomp.prostate.fxn <- function(datos, year.list) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=7))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.localized.regional","change.stage.distant",
                      "change.ex.localized.regional","change.ex.distant")
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.stage <- c(diff(tmp$prop.localized.regional)*mean(tmp$ex.localized.regional), diff(tmp$prop.distant)*mean(tmp$ex.distant)) 
    change.ex <- c(diff(tmp$ex.localized.regional)*mean(tmp$prop.localized.regional), diff(tmp$ex.distant)*mean(tmp$prop.distant))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.stage.localized.regional","change.stage.distant")] <- change.stage
    results[y,c("change.ex.localized.regional","change.ex.distant")] <- change.ex
  }
  return(results)
}

decomp.lymphoma.fxn <- function(datos, year.list) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=11))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.stageI","change.stage.stageII","change.stage.stageIII","change.stage.stageIV",
                      "change.ex.stageI","change.ex.stageII","change.ex.stageIII","change.ex.stageIV")
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.stage <- c(diff(tmp$prop.stageI)*mean(tmp$ex.stageI), diff(tmp$prop.stageII)*mean(tmp$ex.stageII),diff(tmp$prop.stageIII)*mean(tmp$ex.stageIII),diff(tmp$prop.stageIV)*mean(tmp$ex.stageIV))
    change.ex <- c(diff(tmp$ex.stageI)*mean(tmp$prop.stageI), diff(tmp$ex.stageII)*mean(tmp$prop.stageII), diff(tmp$ex.stageIII)*mean(tmp$prop.stageIII), diff(tmp$ex.stageIV)*mean(tmp$prop.stageIV))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.stage.stageI","change.stage.stageII","change.stage.stageIII","change.stage.stageIV")] <- change.stage
    results[y,c("change.ex.stageI","change.ex.stageII","change.ex.stageIII","change.ex.stageIV")] <- change.ex
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
    mx.localized[mx.localized==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.regional[mx.regional==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.distant[mx.distant==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
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
  
create.datos.prostate.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.localized.regional","ex.distant","prop.localized.regional","prop.distant")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.localized.regional <- c(rep(0,9),mx[,as.character(year.list[y]),"8. localized.regional"])
    mx.distant <- c(rep(0,9),mx[,as.character(year.list[y]),"4. distant"])
    mx.localized.regional[which(is.na(mx.localized.regional)==TRUE)] <- 0
    mx.distant[which(is.na(mx.distant)==TRUE)] <- 0
    mx.localized.regional[mx.localized.regional==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.distant[mx.distant==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    ex.localized.regional <- lifetab.grad.nax.nLxappr(nMx=mx.localized.regional,a0=0.1385, Rdx=1)$ex[10]
    ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant,a0=0.1385, Rdx=1)$ex[10]
    prop.localized.regional <- prop[as.character(year.list[y]),"8. localized.regional"]
    prop.distant <- prop[as.character(year.list[y]),"4. distant"]
    ex.overall <-ex.localized.regional*prop.localized.regional+ex.distant*prop.distant
    datos[y,"ex.overall"] <- ex.overall
    datos[y,"ex.localized.regional"] <- ex.localized.regional
    datos[y,"ex.distant"] <- ex.distant
    datos[y,"prop.localized.regional"] <- prop.localized.regional
    datos[y,"prop.distant"] <- prop.distant
  }
  datos <- data.frame(datos)
  return(datos)
}

create.datos.lymphoma.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.stageI","ex.stageII","ex.stageIII","ex.stageIV","prop.stageI","prop.stageII","prop.stageIII","prop.stageIV")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.stageI <- c(rep(0,9),mx[,as.character(year.list[y]),"1. stage I"])
    mx.stageII <- c(rep(0,9),mx[,as.character(year.list[y]),"2. stage II"])
    mx.stageIII <- c(rep(0,9),mx[,as.character(year.list[y]),"3. stage III"])
    mx.stageIV <- c(rep(0,9),mx[,as.character(year.list[y]),"4. stage IV"])
    mx.stageI[which(is.na(mx.stageI)==TRUE)] <- 0
    mx.stageII[which(is.na(mx.stageII)==TRUE)] <- 0
    mx.stageIII[which(is.na(mx.stageIII)==TRUE)] <- 0
    mx.stageIV[which(is.na(mx.stageIV)==TRUE)] <- 0
    mx.stageI[mx.stageI==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageII[mx.stageII==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageIII[mx.stageIII==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageIV[mx.stageIV==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    ex.stageI <- lifetab.grad.nax.nLxappr(nMx=mx.stageI,a0=0.1385, Rdx=1)$ex[10]
    ex.stageII <- lifetab.grad.nax.nLxappr(nMx=mx.stageII,a0=0.1385, Rdx=1)$ex[10]
    ex.stageIII <- lifetab.grad.nax.nLxappr(nMx=mx.stageIII,a0=0.1385, Rdx=1)$ex[10]
    ex.stageIV <- lifetab.grad.nax.nLxappr(nMx=mx.stageIV,a0=0.1385, Rdx=1)$ex[10]
    prop.stageI <- prop[as.character(year.list[y]),"1. stage I"]
    prop.stageII <- prop[as.character(year.list[y]),"2. stage II"]
    prop.stageIII <- prop[as.character(year.list[y]),"3. stage III"]
    prop.stageIV <- prop[as.character(year.list[y]),"4. stage IV"]
    ex.overall <-ex.stageI*prop.stageI+ex.stageII*prop.stageII+ex.stageIII*prop.stageIII++ex.stageIV*prop.stageIV
    datos[y,"ex.overall"] <- ex.overall
    datos[y,"ex.stageI"] <- ex.stageI
    datos[y,"ex.stageII"] <- ex.stageII
    datos[y,"ex.stageIII"] <- ex.stageIII
    datos[y,"ex.stageIV"] <- ex.stageIV
    datos[y,"prop.stageI"] <- prop.stageI
    datos[y,"prop.stageII"] <- prop.stageII
    datos[y,"prop.stageIII"] <- prop.stageIII
    datos[y,"prop.stageIV"] <- prop.stageIV
  }
  datos <- data.frame(datos)
  return(datos)
}

results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
  if (cancer %in% c("crc","lung","esophagus","stomach","pancreas","bladder","kidney","melanoma","headneck","lymphoma","leukemia")) sex <- "dual"
  if (cancer %in% c("breast","cervix","ovary","uterus","prostate")) sex <- "single"
  
  if (sex=="single" & cancer!="prostate")
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list, mx.cause)
  if (sex=="single" & cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.fxn(mx, prop, year.list), year.list)
  if (sex=="dual" & cancer!="lymphoma") {
    results.male <-  decomp.fxn(create.datos.fxn(mx[,,"male",], prop[,,"male"], year.list), year.list)
    results.female <-  decomp.fxn(create.datos.fxn(mx[,,"female",], prop[,,"female"], year.list), year.list)
    results <- results.male + results.female
    results[,c("year.start","year.end")] <- results.male[,c("year.start","year.end")]
  }
  if (sex=="dual" & cancer=="lymphoma") {
    results.male <-  decomp.lymphoma.fxn(create.datos.lymphoma.fxn(mx[,,"male",], prop[,,"male"], year.list), year.list)
    results.female <-  decomp.lymphoma.fxn(create.datos.lymphoma.fxn(mx[,,"female",], prop[,,"female"], year.list), year.list)
    results <- results.male + results.female
    results[,c("year.start","year.end")] <- results.male[,c("year.start","year.end")]
  }
     
    return(results)
}


#esophagus <- results.fxn(mx.esophagus, prop.esophagus, "esophagus", c(1973,2001)) 
#stomach <- results.fxn(mx.stomach, prop.stomach, "stomach", c(1973,2001))
#crc <- results.fxn(mx.crc, prop.crc, "crc", c(1973,2001))
#pancreas <- results.fxn(mx.pancreas, prop.pancreas, "pancreas", c(1973,2001))
#lung <- results.fxn(mx.lung, prop.lung, "lung", c(1988,2001))
#melanoma <- results.fxn(mx.melanoma, prop.melanoma, "melanoma", c(1973,2001))
breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(1980,1990))
#cervix <- results.fxn(mx.cervix, prop.cervix, "cervix", c(1973,2001))
#uterus <- results.fxn(mx.uterus, prop.uterus, "uterus", c(1973,2001))
#ovary <- results.fxn(mx.ovary, prop.ovary, "ovary", c(1973,2001))
#prostate <- results.fxn(mx.prostate, prop.prostate, "prostate", c(1995,2001))
#bladder <- results.fxn(mx.bladder, prop.bladder, "bladder", c(1973,2001))
#kidney <- results.fxn(mx.kidney, prop.kidney, "kidney", c(1973,2001))
#lymphoma <- results.fxn(mx.lymphoma, prop.lymphoma, "lymphoma", c(1983,2001))

#note (dec 11 2014): headneck mortality rates end at 95. there were no 100+ year olds that died of head and neck cancer.
#headneck <- results.fxn(mx.headneck, prop.headneck, "headneck", c(1973,2001)) 

breast.values <- c(unlist(breast[-c(1,2)]))
breast.values2 <- c(breast.values[1],0,breast.values[2:4],0,breast.values[5:7])
width.value <- 1
pdf("~/Cancer/figures/decomp.breast.pdf")
ymin <- 1.15*min(breast.values2)
ymax <- 1.15*max(breast.values2)
barplot(breast.values2,col=c("black",NA,rep("red",3),NA,rep(NA,3)),yaxt="n",xaxt="n",ylim=c(ymin,ymax),border=FALSE,width=width.value)
x.values <- barplot(breast.values2,plot=FALSE)
rect(x.values[7]-width.value/2,0,x.values[7]+width.value/2,breast.values[8],col="darkblue",border=FALSE)
rect(x.values[7]-width.value/2,breast.values[8],x.values[7]+width.value/2,sum(breast.values[8:9]),col="lightblue",border=FALSE)
rect(x.values[8]-width.value/2,0,x.values[8]+width.value/2,breast.values[10],col="darkblue",border=FALSE)
rect(x.values[8]-width.value/2,breast.values[10],x.values[8]+width.value/2,sum(breast.values[10:11]),col="lightblue",border=FALSE)
rect(x.values[9]-width.value/2,0,x.values[9]+width.value/2,breast.values[12],col="darkblue",border=FALSE)
rect(x.values[9]-width.value/2,breast.values[12],x.values[9]+width.value/2,sum(breast.values[12:13]),col="lightblue",border=FALSE)
select <- c(1,3:5,7:9)
positions <- rep(3,length(breast.values2))
positions[breast.values2<0] <- 1
text(x.values[select], breast.values2[select], round(breast.values2[select],1),pos=positions[select])
text(x.values[1],breast.values2[1]/2,"Overall Gain in Life Exp.",srt=90,col="white")
text(x.values[3:5],breast.values2[3:5]/2,c("L","R","D"),col="white")
text(x.values[7:9],breast.values2[7:9]/2,c("L","R","D"),col="white")
text(x.values[4],breast.values[1],"Stage Shift",col="red")
text(x.values[8],breast.values[1],"Improvements in Mortality",col="blue")
abline(h=0)
dev.off()

