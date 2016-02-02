rm(list=ls())
lt <- read.table("~/Desktop/Cancer/data/fltper_5x1.txt",header=TRUE,skip=2)
load("~/Desktop/Cancer/data/mx.breast.size.Rdata")

life.table <- function( x, nax, nMx){  #https://web.stanford.edu/group/heeh/cgi-bin/web/node/75
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  n <- c(diff(x),999)          #width of the intervals
# nax <- n / 2;		            # default to .5 of interval
# nax[1] <- b0 + b1 *nMx[1]    # from Keyfitz & Flieger(1968)
# nax[2] <- 1.5  ;              
# nax[nmax] <- 1/nMx[nmax] 	  # e_x at open age interval
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

draws.fxn <- function(lt,year.select,N,prop.bc) {
    ax <- subset(lt,Year==year.select)$ax
    mx.all <- subset(lt,Year==year.select)$mx
    mx.bc <- mx.all*prop.bc
    mx.other <- mx.all*(1-prop.bc)
    lt.all <- life.table(0:110,ax,mx.all)
    lt.bc <- life.table(0:110,ax,mx.bc)
    lt.other <- life.table(0:110,ax,mx.other)
    draws.all <- sample(lt.all$x,size=N,replace=TRUE,prob=lt.all$ndx)
    draws.bc <- sample(lt.bc$x,size=N,replace=TRUE,prob=lt.bc$ndx)
    draws.other <- sample(lt.other$x,size=N,replace=TRUE,prob=lt.other$ndx)
    return(list(draws.all=draws.all,draws.bc=draws.bc,draws.other=draws.other))
}

tmp <- draws.fxn(lt,1975,10^6,0.1)
age.death.1975 <- data.frame(cbind(tmp$draws.all,tmp$draws.bc,tmp$draws.other))
names(age.death.1975) <- c("all","breast cancer","other")
age.death.1975$realized <- apply(age.death.1975,1,min)

tmp <- draws.fxn(lt,2002,10^6,0.1)
age.death.2002 <- data.frame(cbind(tmp$draws.all,tmp$draws.bc,tmp$draws.other))
names(age.death.2002) <- c("all","breast cancer","other")
age.death.2002$realized <- apply(age.death.2002,1,min)

apply(age.death.2002,2,mean)-apply(age.death.1975,2,mean)
