lifetab.grad.nax.nLxappr <- function(nMx, Rdx)   {

x = c(0,1,seq(5,100,by=5))
n <-c(1,4, rep(5,(length(nMx)-3)), 10)

nqx=1-exp(-n*nMx)
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
# Final life table using the calibrated nax values
#==========================================================================================
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
      nLx[age]=lx[age]*(-1/nMx[age]*(exp(-n[age]*nMx[age])-1))
     }
    if(age==length(lx)) {
      nLx[age]=lx[age]*(1-exp(-10*nMx[age]))/nMx[age]
     }   
    if(nMx[age]==0) nLx[age]=n[age]*lx[age]
  }  

Tx <- rev(cumsum(rev(nLx)))
Ex <- Tx/lx 

lt<-data.frame(x=x, nMx=nMx, nqx=nqx, npx=npx, lx=lx, ndx=ndx, nLx=nLx, Tx=Tx, ex=Ex)            
return(lt)

}
