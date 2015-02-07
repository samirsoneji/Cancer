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
    change.stage <- c(diff(tmp$prop.localized)*mean(tmp$ex.localized),
                      diff(tmp$prop.regional)*mean(tmp$ex.regional),
                      diff(tmp$prop.distant)*mean(tmp$ex.distant)) 
    change.ex <- c(diff(tmp$ex.localized)*mean(tmp$prop.localized),
                   diff(tmp$ex.regional)*mean(tmp$prop.regional),
                   diff(tmp$ex.distant)*mean(tmp$prop.distant))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.stage.localized","change.stage.regional","change.stage.distant")] <- change.stage
    results[y,c("change.ex.localized","change.ex.regional","change.ex.distant")] <- change.ex
    results[y,c("change.ex.localized.cancer","change.ex.localized.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"1. localized"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"1. localized"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.localized[y:(y+1)])
    results[y,c("change.ex.regional.cancer","change.ex.regional.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"2. regional"])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"2. regional"])),
                     Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.regional[y:(y+1)])
    results[y,c("change.ex.distant.cancer","change.ex.distant.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"4. distant"])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"4. distant"])),
                     Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.distant[y:(y+1)])
}
  return(results)
}

decomp.prostate.fxn <- function(datos, year.list, mx.cause) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=11))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.localized.regional","change.stage.distant",
                      "change.ex.localized.regional","change.ex.distant",
                      "change.ex.localized.regional.cancer","change.ex.localized.regional.other",
                      "change.ex.distant.cancer","change.ex.distant.other")
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
    results[y,c("change.ex.localized.regional.cancer","change.ex.localized.regional.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"8. localized.regional"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"8. localized.regional"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.localized[y:(y+1)])
    results[y,c("change.ex.distant.cancer","change.ex.distant.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"4. distant"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"4. distant"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.distant[y:(y+1)])
  }
  return(results)
}


decomp.lymphoma.fxn <- function(datos, year.list, mx.cause) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=19))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.stage.stageI","change.stage.stageII","change.stage.stageIII","change.stage.stageIV",
                      "change.ex.stageI","change.ex.stageII","change.ex.stageIII","change.ex.stageIV",
                      "change.ex.stageI.cancer","change.ex.stageI.other",
                      "change.ex.stageII.cancer","change.ex.stageII.other",
                      "change.ex.stageIII.cancer","change.ex.stageIII.other",
                      "change.ex.stageIV.cancer","change.ex.stageIV.other")
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
    results[y,c("change.ex.stageI.cancer","change.ex.stageI.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"1. stage I"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"1. stage I"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp[y:(y+1),"prop.stageI"])
    results[y,c("change.ex.stageII.cancer","change.ex.stageII.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"2. stage II"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"2. stage II"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp[y:(y+1),"prop.stageII"])
    results[y,c("change.ex.stageIII.cancer","change.ex.stageIII.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"3. stage III"])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"3. stage III"])),
                     Rx=1)$Decomposition[1:2,2] * mean(tmp[y:(y+1),"prop.stageIII"])
    results[y,c("change.ex.stageIV.cancer","change.ex.stageIV.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),"4. stage IV"])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),"4. stage IV"])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp[y:(y+1),"prop.stageIV"])
  }
  return(results)
}
