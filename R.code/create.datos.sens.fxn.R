create.datos.sens.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.size.I","ex.size.II","ex.size.III","ex.size.IV",
                  "prop.size.I","prop.size.II","prop.size.III","prop.size.IV")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.size.I <- c(rep(0,9),mx[,as.character(year.list[y]),1])
    mx.size.II <- c(rep(0,9),mx[,as.character(year.list[y]),2])
    mx.size.III <- c(rep(0,9),mx[,as.character(year.list[y]),3])
    mx.size.IV <- c(rep(0,9),mx[,as.character(year.list[y]),4])
    mx.size.I[which(is.na(mx.size.I)==TRUE)] <- 0
    mx.size.II[which(is.na(mx.size.II)==TRUE)] <- 0 
    mx.size.III[which(is.na(mx.size.III)==TRUE)] <- 0
    mx.size.IV[which(is.na(mx.size.IV)==TRUE)] <- 0
    mx.size.I[mx.size.I==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.size.II[mx.size.II==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.size.III[mx.size.III==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.size.IV[mx.size.IV==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    ex.size.I <- lifetab.grad.nax.nLxappr(nMx=mx.size.I, Rdx=1)$ex[10]
    ex.size.II <- lifetab.grad.nax.nLxappr(nMx=mx.size.II, Rdx=1)$ex[10]
    ex.size.III <- lifetab.grad.nax.nLxappr(nMx=mx.size.III, Rdx=1)$ex[10]
    ex.size.IV <- lifetab.grad.nax.nLxappr(nMx=mx.size.IV, Rdx=1)$ex[10]
    prop.size.I <- prop[as.character(year.list[y]),1]
    prop.size.II <- prop[as.character(year.list[y]),2]
    prop.size.III <- prop[as.character(year.list[y]),3]
    prop.size.IV <- prop[as.character(year.list[y]),4]
    ex.overall <- ex.size.I*prop.size.I+ex.size.II*prop.size.II+ex.size.III*prop.size.III+ex.size.IV*prop.size.IV
    datos[y,"ex.overall"] <- ex.overall
    datos[y,"ex.size.I"] <- ex.size.I
    datos[y,"ex.size.II"] <- ex.size.II
    datos[y,"ex.size.III"] <- ex.size.III
    datos[y,"ex.size.IV"] <- ex.size.IV
    datos[y,"prop.size.I"] <- prop.size.I
    datos[y,"prop.size.II"] <- prop.size.II
    datos[y,"prop.size.III"] <- prop.size.III
    datos[y,"prop.size.IV"] <- prop.size.IV
  }
  datos <- data.frame(datos)
  return(datos)
}


create.datos.prostate.sens.fxn <- function(mx, prop, year.list) {
  value.list <- c("ex.overall","ex.grade.I_II","ex.grade.III",
                  "prop.grade.I_II","prop.grade.III")
  datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
  rownames(datos) <- year.list
  colnames(datos) <- value.list
  for (y in 1:length(year.list)) {
    mx.grade.I_II <- c(rep(0,9),mx[,as.character(year.list[y]),"I_II"])
    mx.grade.III <- c(rep(0,9),mx[,as.character(year.list[y]),"III"])
    mx.grade.I_II[which(is.na(mx.grade.I_II)==TRUE)] <- 0
    mx.grade.III[which(is.na(mx.grade.III)==TRUE)] <- 0
    mx.grade.I_II[mx.grade.I_II==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    mx.grade.III[mx.grade.III==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
    ex.grade.I_II <- lifetab.grad.nax.nLxappr(nMx=mx.grade.I_II, Rdx=1)$ex[10]
    ex.grade.III <- lifetab.grad.nax.nLxappr(nMx=mx.grade.III, Rdx=1)$ex[10]
    prop.grade.I_II <- prop[as.character(year.list[y]),"I_II"]
    prop.grade.III <- prop[as.character(year.list[y]),"III"]
    ex.overall <- ex.grade.I_II*prop.grade.I_II+ex.grade.III*prop.grade.III
    datos[y,"ex.overall"] <- ex.overall
    datos[y,"ex.grade.I_II"] <- ex.grade.I_II
    datos[y,"ex.grade.III"] <- ex.grade.III
    datos[y,"prop.grade.I_II"] <- prop.grade.I_II
    datos[y,"prop.grade.III"] <- prop.grade.III
  }
  datos <- data.frame(datos)
  return(datos)
}

## create.datos.prostate.sens.fxn <- function(mx, prop, year.list) {
##   value.list <- c("ex.overall","ex.grade.I","ex.grade.II","ex.grade.III",
##                   "prop.grade.I","prop.grade.II","prop.grade.III")
##   datos <- matrix(NA, nrow=length(year.list), ncol=length(value.list))
##   rownames(datos) <- year.list
##   colnames(datos) <- value.list
##   for (y in 1:length(year.list)) {
##     mx.grade.I <- c(rep(0,9),mx[,as.character(year.list[y]),"I"])
##     mx.grade.II <- c(rep(0,9),mx[,as.character(year.list[y]),"II"])
##     mx.grade.III <- c(rep(0,9),mx[,as.character(year.list[y]),"III"])
##     mx.grade.I[which(is.na(mx.grade.I)==TRUE)] <- 0
##     mx.grade.II[which(is.na(mx.grade.II)==TRUE)] <- 0 
##     mx.grade.III[which(is.na(mx.grade.III)==TRUE)] <- 0
##     mx.grade.I[mx.grade.I==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
##     mx.grade.II[mx.grade.II==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
##     mx.grade.III[mx.grade.III==Inf] <- 10^6 #assigned infinite mortality rate a large numeric mortality rate
##     ex.grade.I <- lifetab.grad.nax.nLxappr(nMx=mx.grade.I, Rdx=1)$ex[10]
##     ex.grade.II <- lifetab.grad.nax.nLxappr(nMx=mx.grade.II, Rdx=1)$ex[10]
##     ex.grade.III <- lifetab.grad.nax.nLxappr(nMx=mx.grade.III, Rdx=1)$ex[10]
##     prop.grade.I <- prop[as.character(year.list[y]),"I"]
##     prop.grade.II <- prop[as.character(year.list[y]),"II"]
##     prop.grade.III <- prop[as.character(year.list[y]),"III"]
##     ex.overall <- ex.grade.I*prop.grade.I+ex.grade.II*prop.grade.II+ex.grade.III*prop.grade.III
##     datos[y,"ex.overall"] <- ex.overall
##     datos[y,"ex.grade.I"] <- ex.grade.I
##     datos[y,"ex.grade.II"] <- ex.grade.II
##     datos[y,"ex.grade.III"] <- ex.grade.III
##     datos[y,"prop.grade.I"] <- prop.grade.I
##     datos[y,"prop.grade.II"] <- prop.grade.II
##     datos[y,"prop.grade.III"] <- prop.grade.III
##   }
##   datos <- data.frame(datos)
##   return(datos)
## }

