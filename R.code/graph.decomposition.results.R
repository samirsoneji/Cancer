load("~/Cancer/results/results.Rdata")

graph.fxn <- function(res,cancer) {
  values <- c(unlist(res[-c(1,2)]))
  values2 <- c(values[1],0,values[2:4],0,values[5:7])
  width.value <- 1
  cex.value <- 0.5
  ymin <- min(values2)-0.5
  ymax <- 1.5+max(values2)
  barplot(values2,col=c("black",NA,rep("red",3),NA,rep(NA,3)),main=cancer,yaxt="n",xaxt="n",ylim=c(ymin,ymax),border=FALSE,width=width.value)
  x.values <- barplot(values2,plot=FALSE)
  rect(x.values[7]-width.value/2,0,x.values[7]+width.value/2,values[8],col="darkblue",border=FALSE)
  rect(x.values[7]-width.value/2,values[8],x.values[7]+width.value/2,sum(values[8:9]),col="lightblue",border=FALSE)
  rect(x.values[8]-width.value/2,0,x.values[8]+width.value/2,values[10],col="darkblue",border=FALSE)
  rect(x.values[8]-width.value/2,values[10],x.values[8]+width.value/2,sum(values[10:11]),col="lightblue",border=FALSE)
  rect(x.values[9]-width.value/2,0,x.values[9]+width.value/2,values[12],col="darkblue",border=FALSE)
  rect(x.values[9]-width.value/2,values[12],x.values[9]+width.value/2,sum(values[12:13]),col="lightblue",border=FALSE)
  select <- c(1,3:5,7:9)
  positions <- rep(3,length(values2))
  positions[values2<0] <- 1
  text(x.values[select], values2[select], round(values2[select],1),pos=positions[select],cex=cex.value)
  text(x.values[1],ymax,"Overall Gain\nin Life Exp.",col="black",pos=1,cex=cex.value)
  text(x.values[3:5],values2[3:5]/2,c("L","R","D"),col="white",cex=cex.value)
  text(x.values[7:9],values2[7:9]/2,c("L","R","D"),col="white",cex=cex.value)
  text(x.values[4],ymax,"Stage Shift",col="red",pos=1,cex=cex.value)
  text(x.values[8],ymax,"Improvements in Mortality",col="blue",pos=1,cex=cex.value)
  abline(h=0)
}

pdf("~/Cancer/figures/decomposition.results.pdf", height=8.5, width=11, paper="special")
par (mfrow=c(2,2),mgp=c(2.75,1,0)*0.55,mar=c(1.6,1.5,0.5,1.0)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25,bg="white", cex=0.75,cex.main=0.75)
for (i in 1:length(results))
  graph.fxn(results[[i]],names(results)[i])
dev.off()




  
