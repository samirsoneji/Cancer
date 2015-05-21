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

graph.decomp.results.fxn <- function(i,breast.results.sum)
  {name <- 100*as.numeric(names(breast.results.sum)[[i]])
   summary.breast <- breast.results.sum[[i]]
   width <- 0.3
    pdf(paste("~/Desktop/Cancer/figures/decomp_breast_overdiagnosis_",name,".pdf",sep=""), height=8.5, width=14, paper="special")
   par (mfrow=c(1,1),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.2,0.4,0), tcl=-0.25,bg="white",cex=2,cex.main=2)
   year <- 1975:2002
   xpos <-  barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),plot=FALSE,width=c(1,1,1))
   barplot(c(summary.breast[["ex.overall.diff"]],NA,NA),las=1,axes=FALSE,ylim=c(-6,12),border=FALSE,col=c("black","darkgrey"),width=c(1,1,1))
   rect(xpos[2]-width/2,0,xpos[2]+width/2,summary.breast[[2]],col=color2[1],border=NA)
   rect(xpos[2]-width/2,summary.breast[[2]],xpos[2]+width/2,summary.breast[[2]]+summary.breast[[3]],col=color2[2],border=NA)
   rect(xpos[2]-width/2,0,xpos[2]+width/2,summary.breast[[4]],col=color2[3],border=NA)
   rect(xpos[2]-width/2,summary.breast[[4]],xpos[2]+width/2,summary.breast[[4]]+summary.breast[[5]],col=color2[4],border=NA)
     barplot(c(NA,sum(unlist(summary.breast[c(2,3,4,5)])),NA),add=TRUE,axes=FALSE,
             col=alpha("grey",0.5),border=alpha("grey",0.5),width=c(1,1,1))
        
   barplot(matrix(c(rep(NA,10),unlist(summary.breast[c(6,7,8,9,10)])),ncol=3,nrow=5),add=TRUE,axes=FALSE,col=c(color,"darkgrey"),border=c(color,"darkgrey"),width=c(1,1,1))
   axis(2,at=seq(-6,12,2),las=1)
   axis(1,at=xpos,paste(c("Overall","Size","Mortality")),tick=FALSE)
   abline(h=0,col="grey")
   text(xpos[2],summary.breast[[2]]/1.25,"<1cm",cex=0.9)
   text(xpos[2],summary.breast[[2]]+summary.breast[[3]]/2,"1-2cm",cex=0.9)
   text(xpos[2],summary.breast[[4]]/2,"2-5cm",cex=0.9)
   text(xpos[2],summary.breast[[4]]+summary.breast[[5]]/2,"5+cm",cex=0.9,col="white")
   text(xpos[3],sum(unlist(summary.breast[6]))/2,"<1cm, Cancer",cex=0.9)
   text(xpos[3],sum(unlist(summary.breast[6]))+summary.breast[[7]]/2,"1-2cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:7]))+summary.breast[[8]]/2,"2-5cm, Cancer",cex=1)
   text(xpos[3],sum(unlist(summary.breast[6:8]))+summary.breast[[9]]/2,"5+cm, Cancer",cex=0.75,col="white")
text(xpos[3],sum(unlist(summary.breast[6:9]))+summary.breast[[10]]/2,"All Other Causes",cex=1)
mtext(paste(name,"% Overdiagnosis",sep=""),side=3,line=0,outer=TRUE,at=1/2,cex=2)
mtext("Years",side=2,line=0,outer=TRUE,at=1/2,cex=2)
dev.off()
  }

scalar.list <- seq(0,0.20,0.01)
breast.results <- list()
for (i in 1:length(scalar.list))
  breast.results[[i]]<- odx.fxn(scalar.list[i],c("<1cm","1-2cm"),prop.breast,mx.breast,mx.breast.cause,"breast",c(1975,2002))
breast.results.sum <- lapply(breast.results,summary.fxn)
names(breast.results.sum) <- scalar.list

color <- brewer.pal(9,"YlGnBu")[c(3,5,7,9)]
color2 <- alpha(color,0.5)
for (i in 1:length(scalar.list))
  graph.decomp.results.fxn(i,breast.results.sum)
