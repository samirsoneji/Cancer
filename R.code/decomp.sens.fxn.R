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

decomp.age.fxn <- function(datos, year.list, mx.cause) {
  values <- c("year.start","year.end","ex.overall.diff",
                      "change.size.I","change.size.II","change.size.III","change.size.IV","change.size.V",
                      "change.size.I.40","change.size.II.40","change.size.III.40","change.size.IV.40","change.size.V.40",
                      "change.size.I.50","change.size.II.50","change.size.III.50","change.size.IV.50","change.size.V.50",
                      "change.size.I.60","change.size.II.60","change.size.III.60","change.size.IV.60","change.size.V.60",
                      "change.size.I.70","change.size.II.70","change.size.III.70","change.size.IV.70","change.size.V.70",
                      "change.size.I.80","change.size.II.80","change.size.III.80","change.size.IV.80","change.size.V.80",
                      "change.size.I.90","change.size.II.90","change.size.III.90","change.size.IV.90","change.size.V.90",
                      "change.size.I.100","change.size.II.100","change.size.III.100","change.size.IV.100","change.size.V.100",
                      "change.ex.I","change.ex.II","change.ex.III","change.ex.IV","change.ex.V",
                      "change.ex.I.cancer","change.ex.I.other",
                      "change.ex.II.cancer","change.ex.II.other",
                      "change.ex.III.cancer","change.ex.III.other",
                      "change.ex.IV.cancer","change.ex.IV.other",
                      "change.ex.V.cancer","change.ex.V.other",
                      "change.ex.I.cancer.40","change.ex.I.other.40",
                      "change.ex.II.cancer.40","change.ex.II.other.40",
                      "change.ex.III.cancer.40","change.ex.III.other.40",
                      "change.ex.IV.cancer.40","change.ex.IV.other.40",
                      "change.ex.V.cancer.40","change.ex.V.other.40",
                      "change.ex.I.cancer.50","change.ex.I.other.50",
                      "change.ex.II.cancer.50","change.ex.II.other.50",
                      "change.ex.III.cancer.50","change.ex.III.other.50",
                      "change.ex.IV.cancer.50","change.ex.IV.other.50",
                      "change.ex.V.cancer.50","change.ex.V.other.50",
                      "change.ex.I.cancer.60","change.ex.I.other.60",
                      "change.ex.II.cancer.60","change.ex.II.other.60",
                      "change.ex.III.cancer.60","change.ex.III.other.60",
                      "change.ex.IV.cancer.60","change.ex.IV.other.60",
                      "change.ex.V.cancer.60","change.ex.V.other.60",
                      "change.ex.I.cancer.70","change.ex.I.other.70",
                      "change.ex.II.cancer.70","change.ex.II.other.70",
                      "change.ex.III.cancer.70","change.ex.III.other.70",
                      "change.ex.IV.cancer.70","change.ex.IV.other.70",
                      "change.ex.V.cancer.70","change.ex.V.other.70",
                      "change.ex.I.cancer.80","change.ex.I.other.80",
                      "change.ex.II.cancer.80","change.ex.II.other.80",
                      "change.ex.III.cancer.80","change.ex.III.other.80",
                      "change.ex.IV.cancer.80","change.ex.IV.other.80",
                      "change.ex.V.cancer.80","change.ex.V.other.80",
                      "change.ex.I.cancer.90","change.ex.I.other.90",
                      "change.ex.II.cancer.90","change.ex.II.other.90",
                      "change.ex.III.cancer.90","change.ex.III.other.90",
                      "change.ex.IV.cancer.90","change.ex.IV.other.90",
                      "change.ex.V.cancer.90","change.ex.V.other.90",
                      "change.ex.I.cancer.100","change.ex.I.other.100",
                      "change.ex.II.cancer.100","change.ex.II.other.100",
                      "change.ex.III.cancer.100","change.ex.III.other.100",
                      "change.ex.IV.cancer.100","change.ex.IV.other.100",
                      "change.ex.V.cancer.100","change.ex.V.other.100")
  results <- data.frame(matrix(NA, nrow=length(year.list)-1, ncol=length(values)))
  rownames(results) <- year.list[-length(year.list)]
  colnames(results) <- values
  for (y in 1:(length(year.list)-1)) {
    year1 <- year.list[y]
    year2 <- year.list[y+1]
    tmp <- datos[as.character(c(year1,year2)),]
    ex.overall.diff <- diff(tmp$ex.overall)
    change.size <- c(diff(tmp$prop.size.I)*mean(tmp$ex.size.I), diff(tmp$prop.size.II)*mean(tmp$ex.size.II),diff(tmp$prop.size.III)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V)*mean(tmp$ex.size.V))
    change.ex <- c(diff(tmp$ex.size.I)*mean(tmp$prop.size.I), diff(tmp$ex.size.II)*mean(tmp$prop.size.II), diff(tmp$ex.size.III)*mean(tmp$prop.size.III),diff(tmp$ex.size.IV)*mean(tmp$prop.size.IV),diff(tmp$ex.size.V)*mean(tmp$prop.size.V))

    change.size.40 <- c(diff(tmp$prop.size.I.40)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.40)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.40)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.40)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.40)*mean(tmp$ex.size.V))
    change.size.50 <- c(diff(tmp$prop.size.I.50)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.50)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.50)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.50)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.50)*mean(tmp$ex.size.V))
    change.size.60 <- c(diff(tmp$prop.size.I.60)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.60)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.60)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.60)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.60)*mean(tmp$ex.size.V))
    change.size.70 <- c(diff(tmp$prop.size.I.70)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.70)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.70)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.70)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.70)*mean(tmp$ex.size.V))
    change.size.80 <- c(diff(tmp$prop.size.I.80)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.80)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.80)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.80)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.80)*mean(tmp$ex.size.V))
    change.size.90 <- c(diff(tmp$prop.size.I.90)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.90)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.90)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.90)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.90)*mean(tmp$ex.size.V))
    change.size.100 <- c(diff(tmp$prop.size.I.100)*mean(tmp$ex.size.I), diff(tmp$prop.size.II.100)*mean(tmp$ex.size.II),diff(tmp$prop.size.III.100)*mean(tmp$ex.size.III),diff(tmp$prop.size.IV.100)*mean(tmp$ex.size.IV),diff(tmp$prop.size.V.100)*mean(tmp$ex.size.V))
  
    results[y,"year.start"] <- year1
    results[y,"year.end"] <- year2
    results[y,"ex.overall.diff"] <- ex.overall.diff
    results[y,c("change.size.I","change.size.II","change.size.III","change.size.IV","change.size.V")] <- change.size
    results[y,c("change.ex.I","change.ex.II","change.ex.III","change.ex.IV","change.ex.V")] <- change.ex
    results[y,c("change.size.I.40","change.size.II.40","change.size.III.40","change.size.IV.40","change.size.V.40")] <- change.size.40
    results[y,c("change.size.I.50","change.size.II.50","change.size.III.50","change.size.IV.50","change.size.V.50")] <- change.size.50
    results[y,c("change.size.I.60","change.size.II.60","change.size.III.60","change.size.IV.60","change.size.V.60")] <- change.size.60
    results[y,c("change.size.I.70","change.size.II.70","change.size.III.70","change.size.IV.70","change.size.V.70")] <- change.size.70
    results[y,c("change.size.I.80","change.size.II.80","change.size.III.80","change.size.IV.80","change.size.V.80")] <- change.size.80
    results[y,c("change.size.I.90","change.size.II.90","change.size.III.90","change.size.IV.90","change.size.V.90")] <- change.size.90
    results[y,c("change.size.I.100","change.size.II.100","change.size.III.100","change.size.IV.100","change.size.V.100")] <- change.size.100
    
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

    results[y,c("change.ex.I.cancer.40","change.ex.I.other.40")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,3] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.40","change.ex.II.other.40")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,3] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.40","change.ex.III.other.40")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,3] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.40","change.ex.IV.other.40")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,3] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.40","change.ex.V.other.40")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,3] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.50","change.ex.I.other.50")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,4] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.50","change.ex.II.other.50")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,4] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.50","change.ex.III.other.50")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,4] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.50","change.ex.IV.other.50")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,4] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.50","change.ex.V.other.50")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,4] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.60","change.ex.I.other.60")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,5] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.60","change.ex.II.other.60")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,5] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.60","change.ex.III.other.60")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,5] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.60","change.ex.IV.other.60")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,5] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.60","change.ex.V.other.60")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,5] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.70","change.ex.I.other.70")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,6] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.70","change.ex.II.other.70")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,6] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.70","change.ex.III.other.70")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,6] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.70","change.ex.IV.other.70")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,6] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.70","change.ex.V.other.70")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,6] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.80","change.ex.I.other.80")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,7] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.80","change.ex.II.other.80")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,7] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.80","change.ex.III.other.80")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,7] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.80","change.ex.IV.other.80")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,7] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.80","change.ex.V.other.80")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,7] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.90","change.ex.I.other.90")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,8] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.90","change.ex.II.other.90")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,8] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.90","change.ex.III.other.90")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,8] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.90","change.ex.IV.other.90")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,8] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.90","change.ex.V.other.90")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,8] * mean(tmp$prop.size.V)

     results[y,c("change.ex.I.cancer.100","change.ex.I.other.100")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),1])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),1])),
                   Rx=1)$Decomposition[1:2,9] * mean(tmp$prop.size.I)
    results[y,c("change.ex.II.cancer.100","change.ex.II.other.100")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),2])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),2])),
                   Rx=1)$Decomposition[1:2,9] * mean(tmp$prop.size.II)
    results[y,c("change.ex.III.cancer.100","change.ex.III.other.100")] <-
        decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),3])),
                     nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),3])),
                     Rx=1)$Decomposition[1:2,9] * mean(tmp$prop.size.III)
    results[y,c("change.ex.IV.cancer.100","change.ex.IV.other.100")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),4])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),4])),
                   Rx=1)$Decomposition[1:2,9] * mean(tmp$prop.size.IV)
    results[y,c("change.ex.V.cancer.100","change.ex.V.other.100")] <-
      decomp.ex.cd(nMx1=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year1),5])),
                   nMx2=rbind(matrix(0,nrow=9,ncol=2),t(mx.cause[,,as.character(year2),5])),
                   Rx=1)$Decomposition[1:2,9] * mean(tmp$prop.size.V)

  return(results)
  }}
  
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
