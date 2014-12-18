assoc.lt <- function(Data,nax,Rdx)   {

datos<-as.matrix(Data)
age.list <- c(0,1,seq(5,100,5))
dime<-dim(datos)
nms<-colnames(Data)

nMx <- rowSums(datos)


Ri <-matrix(0,dime[1],dime[2])
    for(i in 1:dime[2]) {
       for(k in 1:dime[1]) {
          Ri[k,i]=datos[k,i]/nMx[k]
       }
    }

LT<-lifetab.grad.nax.nLxappr(nMx, nax, Rdx)

nPx <-matrix(0,dime[1],dime[2])
   for(i in 1:dime[2]) {
       for(k in 1:dime[1]) {
       nPx[k,i]=LT$npx[k]^Ri[k,i]
       }
   }

nQx <-1-nPx

Lx <-matrix(Rdx,dime[1],dime[2])
   for(i in 1:dime[2]) {
       for(k in 2:dime[1]) {
       Lx[k,i]=nPx[k-1,i]*Lx[k-1,i]
       }
   }

nDx<-matrix(0,dime[1],dime[2])
   for(i in 1:dime[2]) {
       for(k in 1:dime[1]) {
       nDx[k,i] = nQx[k,i]*Lx[k,i]
       }
   }

n <-c(1,4, rep(5,dime[1]-3), 10)
x = c(0,1,seq(5,(dime[1]-3)*5,by=5), (dime[1]-3)*5+5)

nAx <- matrix(0, dime[1],dime[2])
for (c in 1:length(nms))
  nAx[,c] <- nax#nAx.fxn2(nDx, age.list, c, n, Ri, LT, nQx)



#For the computation of person-years lived we approximate the open-ended interval
#by assuming that there are no person-years lived past age 100; thus, we do not need
#a nax value, we rather use the formula: l(100)*(1-exp(-10*Mi(100))/Mi(100). Where
#Mi(100) refers to the death rate due to cause i at age 100
NLx<-matrix(0,dime[1],dime[2])
  for(i in 1:dime[2]) {
      for(k in 1:dime[1]-1) {
      NLx[k,i] = n[k]*Lx[k+1,i]+nAx[k,i]*nDx[k,i]
      }
      NLx[dime[1],i]=Lx[dime[1],i]*(1-exp(-10*Ri[dime[1],i]*LT$nMx[dime[1]]))/(Ri[dime[1],i]*LT$nMx[dime[1]])
  }
#replacing nLx_i values with n*lx_i if nMx_i is 0
  for(j in 1:dime[2]) {
        if(datos[dime[1],j]==0) {NLx[dime[1],j]=n[dime[1]]*Lx[dime[1],j]}      
   }   

result<-data.frame(N=n,Age=x,nAx=nAx,nPx=nPx,lx=Lx,dx=nDx,nLx=NLx)

colnames(nDx)<-nms
rownames(nDx) <- x

return(NLx)

}  


