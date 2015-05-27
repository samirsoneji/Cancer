rm(list=ls())
library(RColorBrewer)
library(scales)
load("~/Desktop/Cancer/data/mx.breast.size.Rdata")
load("~/Desktop/Cancer/data/mx.prostate.grade.Rdata")

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.sens.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.sens.fxn.R")
source("~/Desktop/Cancer/R.code/results.sens.fxn.R")

color <- brewer.pal(9,"YlGnBu")[c(3,5,7,9)]
color2 <- alpha(color,0.5)

readjust.fxn <- function(x,categories,other.categories) {
  adjusted.categories <- sum(x[categories])
  ratio.other.categories <- x[other.categories]/sum(x[other.categories])
  adjusted.other.categories <- (1-adjusted.categories) * ratio.other.categories
  all <- c(x[categories],adjusted.other.categories)
  return(all)
}

odx.fxn <- function(scalar, categories, prop, mx, mx.cause,cancer,year.list) {
  other.categories <- dimnames(prop)[[2]][-which(categories %in% dimnames(prop)[[2]])]
  prop[,categories] <- (1-scalar) * prop[,categories]
  prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
  for (i in 1:nrow(prop.adj))
    prop.adj[i,] <- readjust.fxn(prop[i,],categories,other.categories)
  dimnames(prop.adj) <- dimnames(prop)
  mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
  mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
  res <- results.fxn(mx,mx.cause,prop.adj,cancer,year.list)
  return(res)
}

summary.fxn <- function(x)
    c(x[3],x[4],x[5],x[6],x[7],x[12],x[14],x[16],x[18],sum(x[seq(13,19,2)]))

graph.rates.fxn <- function(scalar, categories, prop, mx, mx.cause, size.rate, standard){
 other.categories <- dimnames(prop)[[2]][-which(categories %in% dimnames(prop)[[2]])]
 size.rate[,,categories] <- (1-scalar)*size.rate[,,categories]
 stand.size.rate <- apply(size.rate, c(1,3), function(x) x%*%standard)
 prop[,categories] <- (1-scalar) * prop[,categories]
 prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
 for (i in 1:nrow(prop.adj))
   prop.adj[i,] <- readjust.fxn(prop[i,],categories,other.categories)
 dimnames(prop.adj) <- dimnames(prop)
 mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
 mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
 name <- 100*scalar
 pdf(paste("~/Desktop/Cancer/figures/breast_rates_overdiagnosis_",name,".pdf",sep=""), height=4, width=8.5, paper="special")
 par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
 year <- 1975:2002
 
 scale <- 100000
 matplot(year,scale*stand.size.rate[as.character(year),],col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l",lwd=2)
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,700,100))
 text(year[length(year)-2],scale*stand.size.rate[as.character(year[length(year)]),],paste(c("<1cm","1-2cm","2-5cm","5+cm")),lty=1,col=color,pos=c(1,1,1,1))
 
 plot(year,rep(NA,length(year)),axes=FALSE,bty="l",ylim=c(0,100),xlab=NA,ylab=NA)
 polygon(c(year,rev(year)),
         100*c(rep(0,length(year)),rev(prop.adj[as.character(year),1])),
         col=color[1],border=FALSE)
 polygon(c(year,rev(year)),
         100*c(prop.adj[as.character(year),1],rev(apply(prop.adj[as.character(year),1:2],1,sum))),
         col=color[2],border=FALSE)
 polygon(c(year,rev(year)),
         100*c(apply(prop.adj[as.character(year),1:2],1,sum),rev(apply(prop.adj[as.character(year),1:3],1,sum))),
         col=color[3],border=FALSE)
 polygon(c(year,rev(year)),
         100*c(apply(prop.adj[as.character(year),1:3],1,sum),rev(apply(prop.adj[as.character(year),1:4],1,sum))),
         col=color[4],border=FALSE)
 axis(1,at=seq(1975,2002,9))
 axis(2,at=seq(0,100,10),las=1)
 text(1988,100*(prop.adj["1988",1]/2),"<1cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1])+ prop.adj["1988",2]/2),"1-2cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1:2])+ prop.adj["1988",3]/2),"2-5cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1:3])+ prop.adj["1988",4]/2),"5+cm",cex=1,col="white")
      
 mx.cause <- apply(mx.cause, c(1,2,3,4), function(x) min(12,x))
 stand.mx <- apply(mx.cause,c(1,3,4),function(x) sum(x * stand.breast, na.rm=TRUE))
 matplot(year,(stand.mx["breast",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="o",pch=19,cex=1,ylim=c(0,max(stand.mx[,as.character(year),])))
 matlines(year,(stand.mx["other",as.character(year),]),col=color,lty=1)
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,0.12,0.02))
 legend(1975,0.08,ncol=2,cex=0.6,paste(c("5+cm, Cancer","5+cm, Other","2-5cm, Cancer","2-5cm, Other","1-2cm, Cancer","1-2cm, Other","<1cm, Cancer","<1cm, Other")),lty=1,pch=c(19,NA),col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),text.col=c(color[4],color[4],color[3],color[3],color[2],color[2],color[1],color[1]),bty="n")
 mtext("A. Incidence Rates (Per 100,000)",side=3,line=0,outer=TRUE,at=1/6,cex=0.8)
 mtext("B. Size Distribution (%)",side=3,line=0,outer=TRUE,at=3/6,cex=0.8)
 mtext("C. Incidence-Based Mortality Rates",side=3,line=0,outer=TRUE,at=5/6,cex=0.8)
 dev.off()
    }

graph.decomp.results.fxn <- function(i,breast.results.sum)
  {name <- 100*as.numeric(names(breast.results.sum)[[i]])
   summary.breast <- breast.results.sum[[i]]
   width <- 0.3
    pdf(paste("~/Desktop/Cancer/figures/decomp_breast_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
   par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
   year <- 1975:2002
   xpos <-  barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),plot=FALSE,width=c(1,1,1))
   barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),las=1,axes=FALSE,ylim=c(-6,12),border=FALSE,col=c("black","darkgrey"),width=c(1,1,1))
   rect(xpos[2]-width/2,0,xpos[2]+width/2,summary.breast[[2]],col=color[1],border=NA)
   rect(xpos[2]-width/2,summary.breast[[2]],xpos[2]+width/2,summary.breast[[2]]+summary.breast[[3]],col=color[2],border=NA)
   rect(xpos[2]-width/2,0,xpos[2]+width/2,summary.breast[[4]],col=color[3],border=NA)
   rect(xpos[2]-width/2,summary.breast[[4]],xpos[2]+width/2,summary.breast[[4]]+summary.breast[[5]],col=color[4],border=NA)
        
   barplot(matrix(c(rep(NA,10),unlist(summary.breast[c(6,7,8,9,10)])),ncol=3,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"),width=c(1,1,1))
   axis(2,at=seq(-6,12,2),las=1)
   axis(1,at=xpos,paste(c("Overall","Size","Mortality")),tick=FALSE)
   abline(h=0,col="grey")
   text(xpos[2],summary.breast[[2]]/2,"<1cm",cex=0.9)
   text(xpos[2],summary.breast[[2]]+summary.breast[[3]]/2,"1-2cm",cex=0.9)
   text(xpos[2],summary.breast[[4]]/2,"2-5cm",cex=0.9)
   text(xpos[2],summary.breast[[4]]+summary.breast[[5]]/2,"5+cm",cex=0.9,col="white")
   text(xpos[3],sum(unlist(summary.breast[6]))/2,"<1cm, Cancer",cex=0.9)
   text(xpos[3],sum(unlist(summary.breast[6]))+summary.breast[[7]]/2,"1-2cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:7]))+summary.breast[[8]]/2,"2-5cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:8]))+summary.breast[[9]]/2,"5+cm, Cancer",cex=0.75,col="white")
text(xpos[3],sum(unlist(summary.breast[6:9]))+summary.breast[[10]]/2,"All Other Causes",cex=1)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=2)
dev.off()
  }

graph.decomp.results.fxn2 <- function(i,breast.results.sum)
  {name <- 100*as.numeric(names(breast.results.sum)[[i]])
   summary.breast <- breast.results.sum[[i]]
   width <- 0.3
    pdf(paste("~/Desktop/Cancer/figures/decomp_breast_overdiagnosis2_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
   par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
   year <- 1975:2002
   xpos <-  barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),plot=FALSE,width=c(1,1,1))
   barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),las=1,axes=FALSE,ylim=c(0,12),border=FALSE,col=c("black","darkgrey"),width=c(1,1,1))
   barplot(c(NA,sum(unlist(summary.breast[c(2,3,4,5)])),NA),add=TRUE,axes=FALSE,
             col=alpha("darkgrey",1),border=alpha("darkgrey",1),width=c(1,1,1))
        
   barplot(matrix(c(rep(NA,10),unlist(summary.breast[c(6,7,8,9,10)])),ncol=3,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"),width=c(1,1,1))
   axis(2,at=seq(-6,12,2),las=1)
   axis(1,at=xpos,paste(c("Overall","Size","Mortality")),tick=FALSE)
   abline(h=0,col="grey")
   text(xpos[3],sum(unlist(summary.breast[6]))/2,"<1cm, Cancer",cex=0.9)
   text(xpos[3],sum(unlist(summary.breast[6]))+summary.breast[[7]]/2,"1-2cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:7]))+summary.breast[[8]]/2,"2-5cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:8]))+summary.breast[[9]]/2,"5+cm, Cancer",cex=0.75,col="white")
text(xpos[3],sum(unlist(summary.breast[6:9]))+summary.breast[[10]]/2,"All Other Causes",cex=1)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=2)
dev.off()
}

overdiagnosis.fxn <- function(breast.results.sum,name,overdiagnosis=FALSE) {
    total.le <- unlist(lapply(breast.results.sum,function(x) x[[1]]))
    change.size <- unlist(lapply(breast.results.sum,function(x) sum(unlist(x[2:5]))))
    change.cancer.mort <- unlist(lapply(breast.results.sum,function(x) sum(unlist(x[6:9]))))
    change.other.mort <- unlist(lapply(breast.results.sum,function(x) x[[10]]))
    
    pdf(paste("~/Desktop/Cancer/figures/sensitity_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
    par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
    plot(scalar.list,total.le,bty="l",ylab="",xlab="",type="l",lwd=5,las=1,ylim=c(0,max(unlist(total.le))),axes=FALSE)
    axis(1,at=scalar.list[seq(1,21,2)],paste(100*scalar.list[seq(1,21,2)]))
    axis(2,at=seq(0,20,1),las=1)
    polygon(c(scalar.list,rev(scalar.list)),c(rep(0,length(scalar.list)),rev(change.size)),col=brewer.pal(3,"Reds")[3],border=brewer.pal(3,"Reds")[3])
    polygon(c(scalar.list,rev(scalar.list)),c(change.size,rev(change.size+change.cancer.mort)),col=brewer.pal(9,"YlGnBu")[6],border=brewer.pal(9,"YlGnBu")[6])
    polygon(c(scalar.list,rev(scalar.list)),c(change.size+change.cancer.mort,rev(change.size+change.cancer.mort+change.other.mort)),col="darkgrey",border="darkgrey")
    if (overdiagnosis==TRUE) {
      polygon(c(scalar.list,rev(scalar.list)),c(change.size+change.cancer.mort+change.other.mort,rev(rep(total.le[1],length(scalar.list)))),col="black",border="black")
      text(scalar.list[11],total.le[11]+0.35,"Overdiagnosis",col="white",srt=-4,pos=4)
      Arrows(scalar.list[21]-0.001,total.le[18],scalar.list[21]-0.001,total.le[3],code=3,col='white')
      text(scalar.list[21],total.le[21]+diff(total.le[c(21,1)])/2,paste(round(100*(diff(total.le[c(21,1)])/total.le[1]),0),"%",sep=""),pos=2,col="white")
    }
    grid(lty=2,col="lightgrey")
    text(scalar.list[11],change.size[11]/2,"Size",col="white")
    text(scalar.list[11],change.size[11]+change.cancer.mort[11]/2,"Breast Cancer Mortality",col="white")
    text(scalar.list[11],change.size[11]+change.cancer.mort[11]+change.other.mort[11]/2,"Other Cause Mortality",col="white",srt=-4)
    mtext("Years",side=2,line=-1,outer=TRUE,at=1/2,cex=2)
    mtext("% Overdiagnosed",side=1,line=-1,outer=TRUE,at=1/2,cex=2)
    dev.off()
}

scalar.list <- seq(0,0.20,0.01)
breast.results <- list()
for (i in 1:length(scalar.list)){
  graph.rates.fxn(scalar.list[i],c("<1cm","1-2cm"),prop.breast,mx.breast,mx.breast.cause,size.rate,standard)
  breast.results[[i]]<- odx.fxn(scalar.list[i],c("<1cm","1-2cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
}
breast.results.sum <- lapply(breast.results,summary.fxn)
names(breast.results.sum) <- scalar.list
for (i in 1:length(scalar.list))
  graph.decomp.results.fxn(i,breast.results.sum)

graph.decomp.results.fxn2(1,breast.results.sum)
overdiagnosis.fxn(breast.results.sum,"small",overdiagnosis=FALSE)
overdiagnosis.fxn(breast.results.sum,"small_overdiagnosis",overdiagnosis=TRUE)


scalar.list <- seq(0,0.20,0.01)
breast.results2 <- list()
for (i in 1:length(scalar.list))
  breast.results2[[i]]<- odx.fxn(scalar.list[i],c("<1cm","1-2cm","2-5cm","5+cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
breast.results.sum2 <- lapply(breast.results2,summary.fxn)
names(breast.results.sum2) <- scalar.list
overdiagnosis.fxn(breast.results.sum2,"all",overdiagnosis=FALSE)
overdiagnosis.fxn(breast.results.sum2,"all_overdiagnosis",overdiagnosis=TRUE)

scalar.list <- seq(0,0.20,0.01)
breast.results3 <- list()
for (i in 1:length(scalar.list))
  breast.results3[[i]]<- odx.fxn(scalar.list[i],c("<1cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
breast.results.sum3 <- lapply(breast.results3,summary.fxn)
names(breast.results.sum3) <- scalar.list
overdiagnosis.fxn(breast.results.sum3,"1cm",overdiagnosis=FALSE)
overdiagnosis.fxn(breast.results.sum3,"1cm_overdiagnosis",overdiagnosis=TRUE)
