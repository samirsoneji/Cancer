mammo <- read.csv("~/Desktop/Cancer/data/mammo_scr_nhis.csv",header=FALSE)
rownames(mammo) <- c("40-49","50-64","65+","65-74","75+")
colnames(mammo) <- c(1987,1990,1991,1993,1994,1998,1999,2000,2003,2005,2008,2010,2013)
age.select <- c("40-49","50-64","65-74","75+")
mammo <- mammo[age.select,]
years <- colnames(mammo)
matplot(years,t(mammo),ylim=c(0,100),bty="l",ylab="Mammogram in Past 2 Years (%)",las=1,pch=19,col=1:4)
matlines(years,t(mammo),lty=1,col=1:4)
abline(v=2002,lwd=2)
grid(lty=1,col="lightgrey")
text(rep(2003,3),mammo[,"2003"],c(rownames(mammo)),col=1:4,pos=c(3,3,1,1))

