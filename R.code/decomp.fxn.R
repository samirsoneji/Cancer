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
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"1. localized"])),
                   nax=nax,Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.localized[y:(y+1)])
    results[y,c("change.ex.regional.cancer","change.ex.regional.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"2. regional"])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"2. regional"])),
                     nax=nax,Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.regional[y:(y+1)])
    results[y,c("change.ex.distant.cancer","change.ex.distant.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"4. distant"])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"4. distant"])),
                     nax=nax,Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.distant[y:(y+1)])
}
  return(results)
}
