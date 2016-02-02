library(plyr)
load("~/Dropbox/Cancer/data/popl.array.Rdata")
load("~/Dropbox/Cancer/death.array.Rdata")

popl.array2 <- apply(popl.array,c(1,2,3),sum,na.rm=TRUE)
death.array2 <- aperm(apply(death.array,c(1,2,3),function(x) x[c("All Causes of Death","Breast")]),c(2,3,4,1))
year.select <- as.character(1975:2002)

mx.female <- aperm(aaply(death.array2[,year.select,"female",],3,function(x) x/popl.array2[,year.select,"female"]),c(2,3,1))
mx.female[,"2002","Breast"] <- 1*mx.female[,"2002","Breast"]

life.table <- function(x, nMx){  #https://web.stanford.edu/group/heeh/cgi-bin/web/node/75
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

draws.fxn <- function(year.select,N,mx,ages) {
    mx.bc <- mx[,as.character(year.select),"Breast"]
    mx.all <- mx[,as.character(year.select),"All Causes of Death"]
    mx.bc[is.na(mx.bc)==TRUE] <- 0
    mx.other <- mx.all - mx.bc
    lt.all <- life.table(ages,mx.all)
    lt.bc <- life.table(ages,mx.bc)
    lt.other <- life.table(ages,mx.other)

    spline.all <- spline(lt.all$x,lt.all$lx/lt.all$lx[1], xmin=min(lt.all$x), xmax=max(lt.all$x), n=2002, method="fmm")
    spline.all$y[which(spline.all$y<0)] <- 0; spline.all$y[which(spline.all$y>=1)] <- 1
    prob.all <- -1*diff(spline.all$y)
    prob.all[which(prob.all<0)] <- 0
    draws.all <- sample(spline.all$x[-length(spline.all$x)],size=N*10,replace=TRUE,prob=prob.all)
   
    spline.bc <- spline(lt.bc$x,lt.bc$lx, xmin=0, xmax=100, n=2002, method="fmm")
    spline.bc$y[which(spline.bc$y<0)] <- 0; spline.bc$y[which(spline.bc$y>=1)] <- 1
    prob.bc <- -1*diff(spline.bc$y)
    prob.bc[which(prob.bc<0)] <- 0
    draws.bc <- sample(spline.bc$x[-length(spline.bc$x)],size=N,replace=TRUE,prob=prob.bc)

    spline.other <- spline(lt.other$x,lt.other$lx, xmin=0, xmax=100, n=2002, method="fmm")
    spline.other$y[which(spline.other$y<0)] <- 0; spline.other$y[which(spline.other$y>=1)] <- 1
    prob.other <- -1*diff(spline.other$y)
    prob.other[which(prob.other<0)] <- 0
    draws.other <- sample(spline.other$x[-length(spline.other$x)],size=N,replace=TRUE,prob=prob.other)

    return(list(draws.all=draws.all,draws.bc=draws.bc,draws.other=draws.other,lt.all=lt.all,lt.bc=lt.bc,lt.other=lt.other))
}

ages <- c(0,1,seq(5,85,5))
tmp.1975 <- draws.fxn(1975,10^6,mx.female,ages)
age.death.1975 <- data.frame(cbind(tmp.1975$draws.all,tmp.1975$draws.bc,tmp.1975$draws.other))
names(age.death.1975) <- c("all","breast cancer","other")
age.death.1975$realized <- apply(age.death.1975,1,function(x) min(x[2:3]))

tmp.2002 <- draws.fxn(2002,10^6,mx.female,ages)
age.death.2002 <- data.frame(cbind(tmp.2002$draws.all,tmp.2002$draws.bc,tmp.2002$draws.other))
names(age.death.2002) <- c("all","breast cancer","other")
age.death.2002$realized <- apply(age.death.2002,1,function(x) min(x[2:3]))


#print(apply(age.death.1975,2,mean))
print(apply(age.death.2002,2,mean))
#print(apply(age.death.2002,2,mean)-apply(age.death.1975,2,mean))
