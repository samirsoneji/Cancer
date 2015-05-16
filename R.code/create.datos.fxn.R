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
    ex.localized <- lifetab.grad.nax.nLxappr(nMx=mx.localized, Rdx=1)$ex[10]
    ex.regional <- lifetab.grad.nax.nLxappr(nMx=mx.regional, Rdx=1)$ex[10]
    ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant, Rdx=1)$ex[10]
    prop.localized <- prop[as.character(year.list[y]),"1. localized"]
    prop.regional <- prop[as.character(year.list[y]),"2. regional"]
    prop.distant <- prop[as.character(year.list[y]),"4. distant"]
    ex.overall <- ex.localized*prop.localized+ex.regional*prop.regional+ex.distant*prop.distant
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

create.datos.cervix.fxn <- function(mx, prop, year.list) {
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
    ex.localized <- lifetab.grad.nax.nLxappr(nMx=mx.localized, Rdx=1)$ex[10]
    ex.regional <- lifetab.grad.nax.nLxappr(nMx=mx.regional, Rdx=1)$ex[10]
    ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant, Rdx=1)$ex[10]
    prop.localized <- prop[as.character(year.list[y]),"1. localized"]
    prop.regional <- prop[as.character(year.list[y]),"2. regional"]
    prop.distant <- prop[as.character(year.list[y]),"4. distant"]
    ex.overall <- ex.localized*prop.localized+ex.regional*prop.regional+ex.distant*prop.distant
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

## create.datos.prostate.fxn <- function(mx, prop, year.list) {
##   value.list <- c("ex.overall","ex.localized.regional","ex.distant","prop.localized.regional","prop.distant")
##   datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
##   rownames(datos) <- year.list
##   colnames(datos) <- value.list
##   for (y in 1:length(year.list)) {
##     mx.localized.regional <- c(rep(0,9),mx[,as.character(year.list[y]),"8. localized.regional"])
##     mx.distant <- c(rep(0,9),mx[,as.character(year.list[y]),"4. distant"])
##     mx.localized.regional[which(is.na(mx.localized.regional)==TRUE)] <- 0
##     mx.distant[which(is.na(mx.distant)==TRUE)] <- 0
##     mx.localized.regional[mx.localized.regional==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
##     mx.distant[mx.distant==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
##     ex.localized.regional <- lifetab.grad.nax.nLxappr(nMx=mx.localized.regional, Rdx=1)$ex[10]
##     ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant, Rdx=1)$ex[10]
##     prop.localized.regional <- prop[as.character(year.list[y]),"8. localized.regional"]
##     prop.distant <- prop[as.character(year.list[y]),"4. distant"]
##     ex.overall <- ex.localized.regional*prop.localized.regional+ex.distant*prop.distant
##     datos[y,"ex.overall"] <- ex.overall
##     datos[y,"ex.localized.regional"] <- ex.localized.regional
##     datos[y,"ex.distant"] <- ex.distant
##     datos[y,"prop.localized.regional"] <- prop.localized.regional
##     datos[y,"prop.distant"] <- prop.distant
##   }
##   datos <- data.frame(datos)
##   return(datos)
## }

create.datos.prostate.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.stageI","ex.stageII","ex.stageIII","ex.stageIV","prop.stageI","prop.stageII","prop.stageIII","prop.stageIV")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.stageI <- c(rep(0,9),mx[,as.character(year.list[y]),"I"])
    mx.stageII <- c(rep(0,9),mx[,as.character(year.list[y]),"II"])
    mx.stageIII <- c(rep(0,9),mx[,as.character(year.list[y]),"III"])
    mx.stageIV <- c(rep(0,9),mx[,as.character(year.list[y]),"IV"])
    mx.stageI[which(is.na(mx.stageI)==TRUE)] <- 0
    mx.stageII[which(is.na(mx.stageII)==TRUE)] <- 0
    mx.stageIII[which(is.na(mx.stageIII)==TRUE)] <- 0
    mx.stageIV[which(is.na(mx.stageIV)==TRUE)] <- 0
    mx.stageI[mx.stageI==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageII[mx.stageII==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageIII[mx.stageIII==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.stageIV[mx.stageIV==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    ex.stageI <- lifetab.grad.nax.nLxappr(nMx=mx.stageI, Rdx=1)$ex[10]
    ex.stageII <- lifetab.grad.nax.nLxappr(nMx=mx.stageII, Rdx=1)$ex[10]
    ex.stageIII <- lifetab.grad.nax.nLxappr(nMx=mx.stageIII, Rdx=1)$ex[10]
    ex.stageIV <- lifetab.grad.nax.nLxappr(nMx=mx.stageIV, Rdx=1)$ex[10]
    prop.stageI <- prop[as.character(year.list[y]),"I"]
    prop.stageII <- prop[as.character(year.list[y]),"II"]
    prop.stageIII <- prop[as.character(year.list[y]),"III"]
    prop.stageIV <- prop[as.character(year.list[y]),"IV"]
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
    ex.stageI <- lifetab.grad.nax.nLxappr(nMx=mx.stageI, Rdx=1)$ex[10]
    ex.stageII <- lifetab.grad.nax.nLxappr(nMx=mx.stageII, Rdx=1)$ex[10]
    ex.stageIII <- lifetab.grad.nax.nLxappr(nMx=mx.stageIII, Rdx=1)$ex[10]
    ex.stageIV <- lifetab.grad.nax.nLxappr(nMx=mx.stageIV, Rdx=1)$ex[10]
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

  
