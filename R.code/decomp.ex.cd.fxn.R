decomp.ex.cd <- function(nMx1,a01,a11,a12,Rx,nMx2,a02,a21,a22) {


#nMx1<-rates.85.100(data=nMx1,min.age=30,max.age=80)
#nMx2<-rates.85.100(data=nMx2,min.age=30,max.age=80)
  nMx1[is.na(nMx1)]<-0
  nMx2[is.na(nMx2)]<-0
  nMx1[nMx1==Inf]<-10^6
  nMx2[nMx2==Inf]<-10^6

age.grp<-c(0,1,seq(5,100,5))
dime<-dim(nMx1)
nms<-colnames(nMx1)
n <-c(1,4, rep(5,dime[1]-3), 10)
x = c(0,1,seq(5,(dime[1]-3)*5,by=5), (dime[1]-3)*5+5)


t1.lt<-lifetab.grad.nax.nLxappr(rowSums(nMx1),a01, Rx)
t2.lt<-lifetab.grad.nax.nLxappr(rowSums(nMx2),a02, Rx)


t1.alt<-assoc.lt(nMx1,a01,Rx)
t2.alt<-assoc.lt(nMx2,a02,Rx)

t1.cdlt<-matrix(0,nrow=dime[1],ncol=dime[2])
t2.cdlt<-matrix(0,nrow=dime[1],ncol=dime[2])
   for(j in 1:dime[2]){
    for(i in 1:dime[1]) {
     t1.cdlt[i,j]=(t1.lt$nLx[i]/t1.alt[i,j])*n[i]
     t2.cdlt[i,j]=(t2.lt$nLx[i]/t2.alt[i,j])*n[i]
     }
   }

term1<-matrix(0,dime[1],dime[2])
   for(j in 1:dime[2]){
    for(i in 1:dime[1]) {
     term1[i,j]= (t2.alt[i,j]-t1.alt[i,j])*((t1.cdlt[i,j]+t2.cdlt[i,j])/(2*n[i]))
     }
  }

sum.term1<-colSums(term1)

change.ex<-t2.lt$ex[1]-t1.lt$ex[1]

decomp.change.ex<-data.frame(Cause.death=c(nms,"SUM"), Cause.contr=c(sum.term1,sum(sum.term1)))
out<-list(Decomposition=decomp.change.ex,
          ex.t1=t1.lt$ex[1],
          ex.t2=t2.lt$ex[1],
          change.ex=change.ex,
          life.table.t1=t1.lt,
          life.table.t2=t2.lt)
return(out)

}
