library(foreign)

mort75 <- read.dta("~/Downloads/mort1975.dta")
mort75$age[mort75$age %in% 200:699] <- 0
mort75$age[mort75$age %in% 100:199] <- 100
mort75$age[mort75$age >= 900] <- NA
mort75$cod <- "other"
mort75$cod[mort75$ucod==174] <- "breast"
mort75$sex[mort75$sex==1] <- "male"
mort75$sex[mort75$sex==2] <- "female"
deaths75 <- table(mort75$age, mort75$sex, mort75$cod)[,"female",]

popl <- read.table("~/Desktop/Cancer/data/Population.txt",skip=2,header=TRUE)
popl75 <- subset(popl,Year==1975)$Female
popl75 <- c(popl75[1:100],sum(popl75[101:111]))
mx75 <- t(aaply(deaths75,2,function(x) x/popl75))
save(mx75,file="~/Desktop/Cancer/data/mx75.Rdata")

mort02 <- read.dta("~/Downloads/mort2002.dta")
mort02$age[mort02$age %in% 200:699] <- 0
mort02$age[mort02$age %in% 100:199] <- 100
mort02$age[mort02$age >= 900] <- NA
mort02$cod <- "other"
mort02$cod[mort02$ucod %in% c("C500","C509")] <- "breast"
mort02$sex[mort02$sex==1] <- "male"
mort02$sex[mort02$sex==2] <- "female"
deaths02 <- table(mort02$age, mort02$sex, mort02$cod)[,"female",]

popl02 <- subset(popl,Year==2002)$Female
popl02 <- c(popl02[1:100],sum(popl02[101:111]))
mx02 <- t(aaply(deaths02,2,function(x) x/popl02))
save(mx02,file="~/Desktop/Cancer/data/mx02.Rdata")

matplot(log(mx75),type="p",col=1:2,bty="l",las=1,ylab="log mortality rate",xlab="age",pch=1,cex=0.5,ylim=c(min(log(mx75[mx75!=0]),log(mx02[mx02!=0]),na.rm=TRUE),max(log(mx75[mx75!=0]),log(mx02[mx02!=0]),na.rm=TRUE)))
matpoints(log(mx02),col=1:2,pch=19,cex=0.5)
text(80,log(mx75[81,]),c("breast cancer","other cause"),col=c(1,2),pos=c(3,3))
