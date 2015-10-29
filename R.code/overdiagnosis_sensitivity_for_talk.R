rm(list=ls())
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
color <- brewer.pal(5,"Set1")
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
  name <- paste(100*scalar,"_",100*scalar2,sep="")
  pdf(paste("~/Dropbox/talks/figures/breast_rates_overdiagnosis1_",name,".pdf",sep=""), height=4, width=8.5, paper="special")
  par (mfrow=c(1,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
  year <- 1975:2002
 
 scale <- 100000
 matplot(year,scale*stand.size.rate[as.character(year),],col=color,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l",lwd=4)
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
  mtext("Incidence Rates (Per 100,000)",side=3,line=0,outer=TRUE,at=1/4,cex=0.8)
 mtext("Size Distribution (%)",side=3,line=0,outer=TRUE,at=3/4,cex=0.8)
 dev.off()

pdf(paste("~/Dropbox/talks/figures/breast_rates_overdiagnosis2_",name,".pdf",sep=""), height=4, width=8.5, paper="special")
  par (mfrow=c(1,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0,0.4,0), tcl=-0.25,bg="white",cex=0.8,cex.main=0.8)
  year <- 1975:2002
 
  mx.cause <- apply(mx.cause, c(1,2,3,4), function(x) min(12,x))
 stand.mx <- apply(mx.cause,c(1,3,4),function(x) sum(x * stand.breast, na.rm=TRUE))
 matplot(year,(stand.mx["breast",as.character(year),]),col=color,lwd=4,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l",cex=1,ylim=c(0,max(stand.mx[,as.character(year),])))
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,0.12,0.02))
  text(2001, stand.mx["breast","2002",],paste(c("5+cm","3-5cm","2-3cm","1-2cm","<1cm")),col=color,pos=c(1,3,3,3,1))
matplot(year,(stand.mx["other",as.character(year),]),col=color,lwd=4,lty=1,bty="l",xlab=NA,ylab=NA,axes=FALSE,type="l",cex=1,ylim=c(0,max(stand.mx[,as.character(year),])))
 axis(1,at=seq(1975,2002,9))
 axis(2,las=1,at=seq(0,0.12,0.02))
 mtext("Incidence-Based Case Fatality Rates,\nBreast Cancer (10% Overdiagnosis, <3cm)",side=3,line=0,outer=TRUE,at=1/4,cex=0.8)
   mtext("Incidence-Based Case Fatality Rates,\nOther Causes (10% Overdiagnosis, <3cm)",side=3,line=0,outer=TRUE,at=3/4,cex=0.8)
 dev.off()
}

graph.decomp.results.fxn <- function(i,breast.results.sum)
  {name <- i
   summary.breast <- breast.results.sum
   width <- 0.3
   space.value <- c(0.3,0.3,0.3,0.025,0.3)
   width.value <- c(1,1,1/3,1,1)
   cex.value <- 0.75
   pdf(paste("~/Dropbox/talks/figures/decomp_breast_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
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
    pdf(paste("~/Dropbox/talks/figures/decomp_breast_overdiagnosis2_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
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
    pdf(paste("~/Dropbox/talks/figures/sensitivity_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
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

###main analysis (10% overdiagnosis)###
scalar.list <- c(0,0.10,0.20)
scalar.list2 <- c(0,0.10,0.20)
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

## sensitivity analysis varying overdiagnosis level from 0% to 52% (<=3cm, equal level for <1cm, 1-2cm, and 2-3cm)
scalar.list <- seq(0,0.52,0.01)
scalar.list2 <- seq(0,0.52,0.01)
var.names <- names(odx.fxn(scalar.list[1],scalar.list2[1],"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002)))
breast.results.sens <- array(NA,dim=c(length(scalar.list),length(scalar.list2),length(var.names)),
                        dimnames=list(scalar.list,scalar.list2,var.names))
for (i in 1:length(scalar.list)){
  for (j in 1:length(scalar.list2)){
       tmp <- odx.fxn(scalar.list[i],scalar.list2[j],"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
       breast.results.sens[i,j,]<- c(unlist(tmp))
    print(paste(i,j))
  }}
overdiagnosis.fxn(breast.results.sens,"sens",scalar.list,overdiagnosis=FALSE) 

###contribution by age
age <- odx.age.fxn(0.10, 0.10, "<1cm", c("1-2cm","2-3cm"), prop.breast, counts.breast.age, mx.breast, mx.breast.cause, "breast", c(1975,2002))
age.scr.results <- c(sum(age[4:8]),
                     sum(age[9:13+5*0]),
                     sum(age[9:13+5*1]),
                     sum(age[9:13+5*2]),
                     sum(age[9:13+5*3]),
                     sum(age[9:13+5*4]),
                     sum(age[9:13+5*5]),
                     sum(age[9:13+5*6]))
age.cancer.results <- c(sum(age[seq(49,57,2)]),
                        sum(age[seq(59,67,2)]),
                        sum(age[seq(69,77,2)]),
                        sum(age[seq(79,87,2)]),
                        sum(age[seq(89,97,2)]),
                        sum(age[seq(99,107,2)]),
                        sum(age[seq(109,117,2)]),
                        sum(age[seq(119,127,2)]))
age.other.results <- c(sum(age[seq(50,58,2)]),
                       sum(age[seq(60,68,2)]),
                       sum(age[seq(70,78,2)]),
                       sum(age[seq(80,88,2)]),
                       sum(age[seq(90,98,2)]),
                       sum(age[seq(100,108,2)]),
                       sum(age[seq(110,118,2)]),
                       sum(age[seq(120,128,2)]))
age.results <- rbind(age.scr.results, age.cancer.results, age.other.results)
age.results <- rbind(age.results, apply(age.results,2,sum))
colnames(age.results) <- c("overall","40-49","50-59","60-69","70-79","80-89","90-99","100+")
rownames(age.results) <- c("screening","cancer mortality","other mortality","total")
write.csv(age.results, "~/Dropbox/talks/figures/age.results.csv")

age.scr.results2 <- cbind(c(age[4:8]),
                          c(age[4:8+5*1]),
                          c(age[4:8+5*2]),
                          c(age[4:8+5*3]),
                          c(age[4:8+5*4]),
                          c(age[4:8+5*5]),
                          c(age[4:8+5*6]),
                          c(age[4:8+5*7]))
matplot(seq(40,100,10),t(age.scr.results2[,-1]),type="b",lty=1,xlab="age group",ylab="years",las=1,bty="l")
abline(h=0,col="grey")
lines(seq(40,100,10),apply(age.scr.results2[,-1],2,function(x) sum(unlist(x))),col="grey",lwd=4)

age2020 <- odx.age.fxn(0.20, 0.20, "<1cm", c("1-2cm","2-3cm"), prop.breast, counts.breast.age, mx.breast, mx.breast.cause, "breast", c(1975,2002))
age2020.scr.results <- c(sum(age2020[4:8]),
                     sum(age2020[9:13+5*0]),
                     sum(age2020[9:13+5*1]),
                     sum(age2020[9:13+5*2]),
                     sum(age2020[9:13+5*3]),
                     sum(age2020[9:13+5*4]),
                     sum(age2020[9:13+5*5]),
                     sum(age2020[9:13+5*6]))
age2020.cancer.results <- c(sum(age2020[seq(49,57,2)]),
                        sum(age2020[seq(59,67,2)]),
                        sum(age2020[seq(69,77,2)]),
                        sum(age2020[seq(79,87,2)]),
                        sum(age2020[seq(89,97,2)]),
                        sum(age2020[seq(99,107,2)]),
                        sum(age2020[seq(109,117,2)]),
                        sum(age2020[seq(119,127,2)]))
age2020.other.results <- c(sum(age2020[seq(50,58,2)]),
                       sum(age2020[seq(60,68,2)]),
                       sum(age2020[seq(70,78,2)]),
                       sum(age2020[seq(80,88,2)]),
                       sum(age2020[seq(90,98,2)]),
                       sum(age2020[seq(100,108,2)]),
                       sum(age2020[seq(110,118,2)]),
                       sum(age2020[seq(120,128,2)]))


###uspstf 15% reduction in breast cancer mortality###
baseline <- odx.fxn(0.10,0.10,"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
mx.breast2 <- mx.breast
mx.breast.cause2 <- mx.breast.cause
mx.breast2[,"1975",] <- (1-0.15)*mx.breast2[,"1975",]
mx.breast.cause2[,,"1975",] <- (1-0.15)*mx.breast.cause2[,,"1975",]                    
new <- odx.fxn(0.10,0.10,"<1cm",c("1-2cm","2-3cm"),prop.breast,mx.breast2,mx.breast.cause2,"breast",c(1975,2002))
                     
prop.breast.age <- array(NA,dim=c(dim(prop.breast),7))
dimnames(prop.breast.age) <- list(dimnames(prop.breast)[[1]],dimnames(prop.breast)[[2]],seq(40,100,10))
for (i in 1:dim(prop.breast.age)[1]) {
    for (j in 1:dim(prop.breast.age)[2]) {
        prop.breast.age[i,j,] <- prop.breast[i,j] * counts.breast.age[i,j,]/sum(counts.breast.age[i,j,])
    }}
prop.breast.age2 <- apply(prop.breast.age,c(1,2),cumsum)

year <- 1975:2002
size.title <- c("I","II","III","IV","V")
pdf("~/Dropbox/talks/figures/breast_prop_age.pdf", height=5.5, width=11, paper="special")
par(mfrow=c(1,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=1,cex.main=1)
for (s in 1:5) {
    plot(year,prop.breast[3:30,s],bty="l",xlab="year",ylab="percentage",las=1,type="l",lwd=3,ylim=c(0,max(prop.breast[3:30,])))
    polygon(c(year,rev(year)),c(rep(0,length(year)),rev(prop.breast.age2[1,3:30,s])),col=1,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[1,3:30,s],rev(prop.breast.age2[2,3:30,s])),col=2,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[2,3:30,s],rev(prop.breast.age2[3,3:30,s])),col=3,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[3,3:30,s],rev(prop.breast.age2[4,3:30,s])),col=4,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[4,3:30,s],rev(prop.breast.age2[5,3:30,s])),col=5,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[5,3:30,s],rev(prop.breast.age2[6,3:30,s])),col=6,border=NA)
    polygon(c(year,rev(year)),c(prop.breast.age2[6,3:30,s],rev(prop.breast.age2[7,3:30,s])),col=7,border=NA)
    title(paste("cumulative, size: ",size.title[s]))
    matplot(year,prop.breast.age[3:30,s,],ylim=c(0,max(prop.breast.age[3:30,,])),bty="l",xlab="year",ylab="percentage",las=1,type="b",cex=0.75,lty=1,pch=as.character(c(4:9,0)))
    title(paste("individual, size: ",size.title[s]))
    grid(lty=1)
}
dev.off()    

