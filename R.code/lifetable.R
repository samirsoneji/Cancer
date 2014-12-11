lifetab.grad.nax.nLxappr <- function(nMx, a0, Rdx)   {

#kk<-as.matrix(nMx)
#dime<-dim(kk)
#NMx <- matrix(0,dime[1],1)
#NMx[,1] <- kk

x = c(0,1,seq(5,100,by=5))
n <-c(1,4, rep(5,(length(nMx)-3)), 10)
nax=c(a0,rep(2.5,(length(nMx)-2)), 0)

#==========================================================================================
# First life table values assuming nax for ages 10-100 is 2.5
#==========================================================================================
nqx=n*nMx/(1+(n-nax)*nMx)
npx=1-nqx
lx=Rdx
  for(age in 1:(length(npx)-1)){
    lx=c(lx,npx[age]*lx[age])
  }
names(lx)<-names(nMx)

ndx=nqx*lx
ndx[length(ndx)]=lx[length(ndx)] #making sure everybody dies in the last age interval
names(ndx)<-names(lx)  

#==========================================================================================
#  nAx.fxn calibrates nax values by fitting a quadratic function for 3 successive age groups in the life table deaths
#==========================================================================================
nAx <-   nAx.fxn(ndx,age.list=x, n, lx, nqx)
nAx[1]<-a0

#==========================================================================================
# Final life table using the calibrated nax values
#==========================================================================================
nqx=n*nMx/(1+(n-nAx)*nMx)
nqx[length(nqx)]<-1

npx=1-nqx
lx=Rdx
  for(age in 1:(length(npx)-1)){
    lx=c(lx,npx[age]*lx[age])
  }
names(lx)<-names(nMx)

ndx=nqx*lx
ndx[length(ndx)]=lx[length(ndx)] #making sure everybody dies in the last age interval
names(ndx)<-names(lx)  

nLx=rep(NA, length(lx))
  for(age in 1:length(npx)){
    if(age<length(lx)) {
      nLx[age]=n[age]*lx[age+1]+nAx[age]*ndx[age]
     }
    if(age==length(lx)) {
      nLx[age]=lx[age]*(1-exp(-10*nMx[age]))/nMx[age]
     }   
    if(nMx[age]==0) nLx[age]=n[age]*lx[age]
  }  

Tx <- rev(cumsum(rev(nLx)))
Ex <- Tx/lx 

lt<-data.frame(x=x, nax=nAx, nMx=nMx, nqx=nqx, npx=npx, lx=lx, ndx=ndx, nLx=nLx, Tx=Tx, ex=Ex)            
return(lt)

}
