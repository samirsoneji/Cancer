#rm(list=ls())
lt <- read.table("~/Desktop/Cancer/data/fltper_5x1.txt",header=TRUE,skip=2)
#load("~/Desktop/Cancer/data/mx.breast.size.Rdata")

life.table <- function( x, nMx){  #https://web.stanford.edu/group/heeh/cgi-bin/web/node/75
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  n <- c(diff(x),999)          #width of the intervals
 nax <- n / 2;		            # default to .5 of interval
 nax[1] <- b0 + b1 *nMx[1]    # from Keyfitz & Flieger(1968)
 nax[2] <- 1.5  ;              
 nax[nmax] <- 1/nMx[nmax] 	  # e_x at open age interval
 nqx <- (n*nMx) / (1 + (n-nax)*nMx)
 nqx<-ifelse( nqx > 1, 1, nqx); # necessary for high nMx
 nqx[nmax] <- 1.0
 lx <- c(1,cumprod(1-nqx)) ;  # survivorship lx
 lx <- lx[1:length(nMx)]
 ndx <- lx * nqx ;
 nLx <- n*lx - nax*ndx;       # equivalent to n*l(x+n) + (n-nax)*ndx
 nLx[nmax] <- lx[nmax]*nax[nmax]
 Tx <- rev(cumsum(rev(nLx)))
 ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
 life.tab <- data.frame(x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
 return(life.tab)
}

draws.fxn <- function(year.select,N,mx) {
    mx.bc <- c(rep(0,9),mx[,as.character(year.select),"breast"])
    mx.other <- c(rep(0,9),mx[,as.character(year.select),"other"])
    mx.all <- mx.bc + mx.other
    lt.all <- life.table(ages,mx.all)
    lt.bc <- life.table(ages,mx.bc)
    lt.other <- life.table(ages,mx.other)
    spline.all <- predict(smooth.spline(lt.all$x,lt.all$lx),x=0:100)
    prob.all <- spline.all$y[which(spline.all$y<0)] <- 0
    draws.all <- sample(spline.all$x,size=N,replace=TRUE,prob=-1*diff(prob.all))
    draws.bc <- sample(lt.bc$x,size=N,replace=TRUE,prob=lt.bc$ndx)
    draws.other <- sample(lt.other$x,size=N,replace=TRUE,prob=lt.other$ndx)
    return(list(draws.all=draws.all,draws.bc=draws.bc,draws.other=draws.other))
}

ages <- c(0,1,seq(5,100,5))
tmp <- draws.fxn(1975,10^6,mx.breast.overall.all.sizes)
age.death.1975 <- data.frame(cbind(tmp$draws.all,tmp$draws.bc,tmp$draws.other))
names(age.death.1975) <- c("all","breast cancer","other")
age.death.1975$realized <- apply(age.death.1975,1,min)

tmp <- draws.fxn(2002,10^6,mx.breast.overall.all.sizes)
age.death.2002 <- data.frame(cbind(tmp$draws.all,tmp$draws.bc,tmp$draws.other))
names(age.death.2002) <- c("all","breast cancer","other")
age.death.2002$realized <- apply(age.death.2002,1,min)

apply(age.death.2002,2,mean)-apply(age.death.1975,2,mean)
