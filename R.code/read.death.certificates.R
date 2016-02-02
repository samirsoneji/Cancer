library(foreign)

#mort75 <- read.dta("~/Downloads/mort1975.dta")
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

matplot(log(mx75),type="p",col=1:2,bty="l",las=1,ylab="log mortality rate",xlab="age",pch=19,cex=0.5)
text(80,log(mx75[81,]),c("breast cancer","other cause"),col=c(1,2),pos=c(3,3))

save(mx75,file="~/Desktop/Cancer/data/mx75.Rdata")
