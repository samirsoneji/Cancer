decomp.fxn <- function(datos, year.list, mx.cause) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=23))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.size.I","change.size.II","change.size.III","change.size.IV","change.size.V",
                      "change.ex.I","change.ex.II","change.ex.III","change.ex.IV","change.ex.V",
                      "change.ex.I.cancer","change.ex.I.other",
                      "change.ex.II.cancer","change.ex.II.other",
                      "change.ex.III.cancer","change.ex.III.other",
                      "change.ex.IV.cancer","change.ex.IV.other",
                      "change.ex.V.cancer","change.ex.V.other")
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.size <- c(diff(tmp$prop.size.I)*mean(tmp$ex.size.I), diff(tmp$prop.size.II)*mean(tmp$ex.size.II),diff(tmp$prop.size.III)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V)*mean(tmp$ex.size.V))
    change.ex <- c(diff(tmp$ex.size.I)*mean(tmp$prop.size.I), diff(tmp$ex.size.II)*mean(tmp$prop.size.II), diff(tmp$ex.size.III)*mean(tmp$prop.size.III),diff(tmp$ex.size.IV)*mean(tmp$prop.size.IV),diff(tmp$ex.size.V)*mean(tmp$prop.size.V))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.size.I","change.size.II","change.size.III","change.size.IV","change.size.V")] <- change.size
    results[y,c("change.ex.I","change.ex.II","change.ex.III","change.ex.IV","change.ex.V")] <- change.ex
    results[y,c("change.ex.I.cancer","change.ex.I.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer","change.ex.II.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer","change.ex.III.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer","change.ex.IV.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer","change.ex.V.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.size.V)
  }
  return(results)
}

decomp.prostate.fxn <- function(datos, year.list, mx.cause) {
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=11))
  names(results) <- c("year.start","year.end","ex.overall.diff",
                      "change.grade.I_II","change.grade.III",
                      "change.ex.I_II","change.ex.III",
                      "change.ex.I_II.cancer","change.ex.I_II.other",
                      "change.ex.III.cancer","change.ex.III.other")
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.grade <- c(diff(tmp$prop.grade.I_II)*mean(tmp$ex.grade.I_II),diff(tmp$prop.grade.III)*mean(tmp$ex.grade.III))
    change.ex <- c(diff(tmp$ex.grade.I_II)*mean(tmp$prop.grade.I_II),diff(tmp$ex.grade.III)*mean(tmp$prop.grade.III))
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.grade.I_II","change.grade.III")] <- change.grade
    results[y,c("change.ex.I_II","change.ex.III")] <- change.ex
    results[y,c("change.ex.I_II.cancer","change.ex.I_II.other")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.grade.I_II)
    results[y,c("change.ex.III.cancer","change.ex.III.other")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                     Rx=1)$Decomposition[1:2,2] * mean(tmp$prop.grade.III)
  }
  return(results)
}
