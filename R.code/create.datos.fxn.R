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
    ex.localized <- lifetab.grad.nax.nLxappr(nMx=mx.localized,nax=nax, Rdx=1)$ex[1]
    ex.regional <- lifetab.grad.nax.nLxappr(nMx=mx.regional,nax=nax, Rdx=1)$ex[1]
    ex.distant <- lifetab.grad.nax.nLxappr(nMx=mx.distant,nax=nax, Rdx=1)$ex[1]
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
  
