nAx.fxn2 <- function(nDx, age.list, cause, n, Ri, LT, nQx) {
  y <- nDx[,cause]
  len <- length(y)
  nax <- rep(NA, len)
  for (i in 1:len) {
  if (i %in% c(1))   
    nax[i] <- n[i]+Ri[i,cause]*((1-LT$npx[i])/nQx[i,cause])*(LT$nax[i]-n[i])
  if (i %in% c(2:(len-1))) {
      age <- c(age.list[i-1],age.list[i],age.list[i+1])	
      age2 <- seq(age.list[i],(age.list[i+1]-1)     )
      a <- matrix(c(age^2, age, rep(1,length(age))),length(age),length(age))
      b <- c(y[i-1],y[i],y[i+1])
      coef <- solve(a,b)	
      c <- coef[1]*age2^2 + coef[2]*age2 + coef[3]
      nax[i] <- sum(age2*c)/sum(c)-age.list[i]
     }
  if (i == len) {
      age <- c(age.list[i-1],age.list[i])	
      age2 <- seq(age.list[i],110)
      a <- matrix(c(age, rep(1,length(age))),length(age),length(age))
      b <- c(y[i-1],y[i])
      coef <- solve(a,b)	
      c <- coef[1]*age2 + coef[2]
      nax[i] <- sum(age2*c)/sum(c)-age.list[i]
     }
 nax[nax==NaN | nax==Inf] <- 0
  if (nQx[i,cause]==0) nax[i] <- 0
    }
  return(nax)
}
