#rm(list=ls())
library(RColorBrewer)
library(scales)
library(shape)
library(lattice)
load("~/Desktop/Cancer/data/mx.breast.size.Rdata")

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.sens.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.sens.fxn.R")
source("~/Desktop/Cancer/R.code/results.sens.fxn.R")

red <- brewer.pal(11,"RdYlBu")[1]
color <- brewer.pal(11,"RdYlBu")[c(3,5,8,9,11)]
color2 <- alpha(color,0.5)
color3 <- alpha(color,1)

readjust.fxn <- function(x,categories,other.categories) {
  adjusted.categories <- sum(x[categories])
  ratio.other.categories <- x[other.categories]/sum(x[other.categories])
  adjusted.other.categories <- (1-adjusted.categories) * ratio.other.categories
  all <- c(x[categories],adjusted.other.categories)
  return(all)
}

odx.fxn <- function(scalar, scalar2, categories, categories2, prop, mx, mx.cause,cancer,year.list) {
  categories.all <- c(categories,categories2)
  other.categories <- dimnames(prop)[[2]][-which(categories.all %in% dimnames(prop)[[2]])]
  prop[,categories] <- (1-scalar) * prop[,categories]
  prop[,categories2] <- (1-scalar2) * prop[,categories2]
  prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
  for (i in 1:nrow(prop.adj))
    prop.adj[i,] <- readjust.fxn(prop[i,],categories.all,other.categories)
  dimnames(prop.adj) <- dimnames(prop)
  mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
  mx[,,categories2] <- (1-scalar2)^-1 * mx[,,categories2]
  mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
  mx.cause[,,,categories2] <- (1-scalar2)^-1 * mx.cause[,,,categories2]
  res <- results.fxn(mx,mx.cause,prop.adj,cancer,year.list)
  return(res)
}

odx.age.fxn <- function(scalar, scalar2, categories, categories2, prop, counts, mx, mx.cause,cancer,year.list) {
  categories.all <- c(categories,categories2)
  other.categories <- dimnames(prop)[[2]][-which(categories.all %in% dimnames(prop)[[2]])]
  prop[,categories] <- (1-scalar) * prop[,categories]
  prop[,categories2] <- (1-scalar2) * prop[,categories2]
  prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
  for (i in 1:nrow(prop.adj))
    prop.adj[i,] <- readjust.fxn(prop[i,],categories.all,other.categories)
  dimnames(prop.adj) <- dimnames(prop)
  mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
  mx[,,categories2] <- (1-scalar2)^-1 * mx[,,categories2]
  mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
  mx.cause[,,,categories2] <- (1-scalar2)^-1 * mx.cause[,,,categories2]
  res <- results.age.fxn(mx,mx.cause,prop.adj,counts,cancer,year.list)
  return(res)
}

summary.fxn <- function(x)
    c(x[3],x[4],x[5],x[6],x[7],x[8],x[14],x[16],x[18],x[20],x[22],sum(x[seq(15,23,2)]))

graph.rates.fxn <- function(scalar, scalar2, categories, categories2, prop, mx, mx.cause, size.rate, standard){
  categories.all <- c(categories,categories2)
  other.categories <- dimnames(prop)[[2]][-which(categories.all %in% dimnames(prop)[[2]])]
  size.rate[,,categories] <- (1-scalar)*size.rate[,,categories]
  size.rate[,,categories2] <- (1-scalar2)*size.rate[,,categories2]
  stand.size.rate <- apply(size.rate, c(1,3), function(x) x%*%standard)
  prop[,categories] <- (1-scalar) * prop[,categories]
  prop[,categories2] <- (1-scalar2) * prop[,categories2]
  prop.adj <- matrix(NA,nrow=nrow(prop),ncol=ncol(prop))
  for (i in 1:nrow(prop.adj))
    prop.adj[i,] <- readjust.fxn(prop[i,],categories.all,other.categories)
  dimnames(prop.adj) <- dimnames(prop)
  mx[,,categories] <- (1-scalar)^-1 * mx[,,categories]
  mx[,,categories2] <- (1-scalar2)^-1 * mx[,,categories2]
  mx.cause[,,,categories] <- (1-scalar)^-1 * mx.cause[,,,categories]
  mx.cause[,,,categories2] <- (1-scalar2)^-1 * mx.cause[,,,categories2]
  name <- paste(100*scalar,100*scalar2)
  pdf(paste("~/Desktop/Cancer/figures/breast_rates_overdiagnosis_",name,".pdf",sep=""), height=4, width=8.5, paper="special")
  par (mfrow=c(1,3),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
  year <- 1975:2002
 
 scale <- 100000
 matplot(year,scale*stand.size.rate[as.character(year),],col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l",lwd=2)
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,700,10))
 text(year[length(year)-2],scale*stand.size.rate[as.character(year[length(year)]),],paste(c("<1cm","1-2cm","2-3cm","3-5cm","5+cm")),lty=1,col=color,pos=c(3,1,1,1,1))
 
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
 polygon(c(year,rev(year)),
         100*c(apply(prop.adj[as.character(year),1:4],1,sum),rev(apply(prop.adj[as.character(year),1:5],1,sum))),
         col=color[5],border=FALSE)
 axis(1,at=seq(1975,2002,9))
 axis(2,at=seq(0,100,10),las=1)
 text(1988,100*(prop.adj["1988",1]/2),"<1cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1])+ prop.adj["1988",2]/2),"1-2cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1:2])+ prop.adj["1988",3]/2),"2-3cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1:3])+ prop.adj["1988",4]/2),"3-5cm",cex=1)
 text(1988,100*(sum(prop.adj["1988",1:4])+ prop.adj["1988",5]/2),"5+cm",cex=1,col="white")
      
 mx.cause <- apply(mx.cause, c(1,2,3,4), function(x) min(12,x))
 stand.mx <- apply(mx.cause,c(1,3,4),function(x) sum(x * stand.breast, na.rm=TRUE))
 matplot(year,(stand.mx["breast",as.character(year),]),col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="o",pch=19,cex=1,ylim=c(0,max(stand.mx[,as.character(year),])))
 matlines(year,(stand.mx["other",as.character(year),]),col=color,lty=1)
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,0.12,0.02))
  legend(1975,0.08,ncol=2,cex=0.5,paste(c("5+cm, Cancer","3-5cm, Cancer","2-3cm, Cancer","1-2cm, Cancer","<1cm, Cancer",
                              "5+cm, Other","3-5cm, Other","2-3cm, Other","1-2cm, Other","<1cm, Other")),lty=1,pch=c(rep(19,5),rep(NA,5)),col=c(rep(color[5:1],2)),text.col=c(rep(color[5:1],2)),bty="n")
 mtext("A. Incidence Rates (Per 100,000)",side=3,line=0,outer=TRUE,at=1/6,cex=0.8)
 mtext("B. Size Distribution (%)",side=3,line=0,outer=TRUE,at=3/6,cex=0.8)
 mtext("C. Incidence-Based Case Fatality Rates",side=3,line=0,outer=TRUE,at=5/6,cex=0.8)
 dev.off()
    }

graph.decomp.results.fxn <- function(i,breast.results.sum)
  {name <- i
   summary.breast <- breast.results.sum
   width <- 0.3
   space.value <- c(0.3,0.3,0.3,0.025,0.3)
   width.value <- c(1,1,1/3,1,1)
   cex.value <- 0.75
   pdf(paste("~/Desktop/Cancer/figures/decomp_breast_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
   par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
   year <- 1975:2002
   xpos <-  barplot(c(summary.breast[["ex.overall.diff"]],NA,NA,NA,NA),plot=FALSE,width=width.value,space=space.value)
   barplot(c(summary.breast[["ex.overall.diff"]],NA,NA,NA,NA),las=1,axes=FALSE,ylim=c(-6,12),border=FALSE,col=c("white"),width=width.value)
   abline(h=seq(-6,12,1),col="lightgrey",lty=1)
   barplot(c(summary.breast[["ex.overall.diff"]],NA,NA,NA,NA),las=1,axes=FALSE,ylim=c(-6,12),border=FALSE,col=c("black","darkgrey"),width=width.value,add=TRUE)
   barplot(matrix(c(rep(NA,4),unlist(summary.breast[c(2:3)]),rep(NA,4)),ncol=5,nrow=2),add=TRUE,axes=FALSE,col=c(color3[1],color3[2]),border=c(color3[1],color3[2]),width=width.value,space=space.value)
   barplot(matrix(c(rep(NA,6),unlist(summary.breast[c(4:6)]),rep(NA,6)),ncol=5,nrow=3),add=TRUE,axes=FALSE,col=c(color3[3],color3[4],color3[5]),border=c(color3[3],color3[4],color3[5]),width=width.value,space=space.value)
   
   barplot(c(NA,NA,NA,sum(unlist(summary.breast[2:6])),NA),las=1,axes=FALSE,add=TRUE,col=red,border=red,width=width.value,space=space.value)
        
   barplot(matrix(c(rep(NA,24),unlist(summary.breast[c(7:12)])),ncol=5,nrow=6),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"),width=width.value,space=space.value)

   axis(2,at=seq(-6,12,2),las=1)
  # axis(1,at=c(xpos[1],xpos[2],mean(xpos[2:3]),xpos[4]),paste(c("Overall Gain in\n Life Expectancy","","Contribution from Temporal\nShift in Tumor Size","Contribution from Reductions\n in Case Fatality Rates")),tick=FALSE)
   abline(h=0,col="grey")
   text(xpos[3],summary.breast[[2]]/2,"<1cm",cex=cex.value)
   text(xpos[3],summary.breast[[2]]+summary.breast[[3]]/2,"1-2cm",cex=cex.value)
   text(xpos[3],summary.breast[[4]]/2,"2-3cm",cex=cex.value)
   text(xpos[3],summary.breast[[4]]+summary.breast[[5]]/2,"3-5cm",cex=cex.value)
   text(xpos[3],summary.breast[[4]]+summary.breast[[5]]+summary.breast[[6]]/2,"5+cm",cex=cex.value,col="white")
   text(xpos[4],sum(unlist(summary.breast[2:6]))/2,"Net Contribution",cex=cex.value,col="white")
   text(xpos[5],sum(unlist(summary.breast[7]))/2,"<1cm, Cancer",cex=cex.value)
   text(xpos[5],sum(unlist(summary.breast[7]))+summary.breast[[8]]/2,"1-2cm, Cancer",cex=cex.value)
   text(xpos[5],sum(unlist(summary.breast[7:8]))+summary.breast[[9]]/2,"2-3cm, Cancer",cex=cex.value)
   text(xpos[5],sum(unlist(summary.breast[7:9]))+summary.breast[[10]]/2,"3-5cm, Cancer",cex=cex.value,col="white")
   text(xpos[5],sum(unlist(summary.breast[7:10]))+summary.breast[[11]]/2,"5+cm, Cancer",cex=cex.value,col="white")
   text(xpos[5],sum(unlist(summary.breast[7:11]))+summary.breast[[12]]/2,"Competing Causes",cex=cex.value)
   mtext("Years",side=2,line=-0.5,outer=TRUE,at=1/2,cex=2)
   mtext("Overall Gain in\n Life Expectancy",side=1,line=-1,outer=TRUE,at=0.2,cex=2)
   mtext("Contribution From:",side=1,line=-1.5,outer=TRUE,at=mean(c(0.2,0.63)),cex=2)
   mtext("Temporal Shift\n in Tumor Size",side=1,line=-1,outer=TRUE,at=0.63,cex=2)
   mtext("Reduction in Case\n Fatality Rates",side=1,line=-1,outer=TRUE,at=0.875,cex=2)
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

overdiagnosis.fxn <- function(breast.results,name,scalar.list,overdiagnosis=FALSE) {
    total.le <- diag(breast.results[,,"ex.overall.diff"])
    change.size <- diag(apply(breast.results,c(1,2),function(x) sum(unlist(x[4:8]))))
    change.cancer.mort <- diag(apply(breast.results,c(1,2),function(x) sum(unlist(x[seq(14,22,2)]))))
    change.other.mort <- diag(apply(breast.results,c(1,2),function(x) sum(unlist(x[seq(14,22,2)+1]))))
    last <- length(scalar.list)
    middle <- round(last/2)
    pdf(paste("~/Desktop/Cancer/figures/sensitivity_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
    par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
    plot(scalar.list,change.size+change.cancer.mort+change.other.mort,bty="l",ylab="",xlab="",type="l",lwd=5,las=1,ylim=c(0,max(unlist(total.le))),axes=FALSE)
    axis(1,at=scalar.list[seq(1,last,2)],paste(100*scalar.list[seq(1,last,2)]))
    axis(2,at=seq(0,52,1),las=1)
    polygon(c(scalar.list,rev(scalar.list)),c(rep(0,length(scalar.list)),rev(change.size)),col=brewer.pal(3,"Reds")[3],border=brewer.pal(3,"Reds")[3])
    polygon(c(scalar.list,rev(scalar.list)),c(change.size,rev(change.size+change.cancer.mort)),col=brewer.pal(9,"YlGnBu")[6],border=brewer.pal(9,"YlGnBu")[6])
    polygon(c(scalar.list,rev(scalar.list)),c(change.size+change.cancer.mort,rev(change.size+change.cancer.mort+change.other.mort)),col="darkgrey",border="darkgrey")
    lines(scalar.list,change.size+change.cancer.mort+change.other.mort,lwd=5)
    if (overdiagnosis==TRUE) {
      polygon(c(scalar.list,rev(scalar.list)),c(change.size+change.cancer.mort+change.other.mort,rev(rep(total.le[1],length(scalar.list)))),col="black",border="black")
      text(scalar.list[middle],total.le[middle]+0.35,"Overdiagnosis",col="white",srt=-4,pos=4)
      Arrows(scalar.list[last]-0.001,total.le[last-2],scalar.list[last]-0.001,total.le[3],code=3,col='white')
      text(scalar.list[last],total.le[last]+diff(total.le[c(last,1)])/2,paste(round(100*(diff(total.le[c(last,1)])/total.le[1]),0),"%",sep=""),pos=2,col="white")
      Arrows(scalar.list[11],total.le[11-2],scalar.list[11],total.le[3],code=3,col='white',arr.length=0.3)
       text(scalar.list[11],total.le[11]+diff(total.le[c(11,1)])/2,paste(round(100*(diff(total.le[c(11,1)])/total.le[1]),0),"%",sep=""),pos=2,col="white")
    }
    grid(lty=2,col="lightgrey")
    text(scalar.list[middle],change.size[middle]/2,"Tumor Size",col="white")
    text(scalar.list[middle],change.size[middle]+change.cancer.mort[middle]/2,"Breast Cancer Fatality",col="white")
    text(scalar.list[middle],change.size[middle]+change.cancer.mort[middle]+change.other.mort[middle]/2,"Other Cause Fatality",col="white",srt=-8)
    mtext("Gain in Life Expectancy (Years)",side=2,line=-1,outer=TRUE,at=1/2,cex=2)
    mtext("% Overdiagnosed",side=1,line=-1,outer=TRUE,at=1/2,cex=2)
    dev.off()
}

mx.breast[,"1975",] <- (1-0.2)*mx.breast[,"1975",]
mx.breast[,"2002",] <- (1-0.2)*mx.breast[,"2002",]
mx.breast.cause[,,c("1975","2002"),] <- (1-0.2)*mx.breast.cause[,,c("1975","2002"),]
###main analysis (10% overdiagnosis)###
scalar.list <- c(0.10)
scalar.list2 <- c(0.10)
var.names <- names(odx.fxn(scalar.list[1],scalar.list2[1],"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002)))
breast.results <- array(NA,dim=c(length(scalar.list),length(scalar.list2),length(var.names)),
                        dimnames=list(scalar.list,scalar.list2,var.names))
for (i in 1:length(scalar.list)){
  for (j in 1:length(scalar.list2)){
    graph.rates.fxn(scalar.list[i],scalar.list2[j],"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,size.rate,standard)
    tmp <- odx.fxn(scalar.list[i],scalar.list2[j],"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
    breast.results[i,j,]<- c(unlist(tmp))
    print(paste(i,j))
  }}

breast.results.sum10 <- summary.fxn(breast.results["0.1","0.1",])
graph.decomp.results.fxn("10",breast.results.sum10)
