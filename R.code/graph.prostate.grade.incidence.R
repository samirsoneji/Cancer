prostate <- read.csv("~/Desktop/Cancer/data/prostate.grade.txt",sep=";",header=FALSE)
names(prostate) <- c("grade","year.dx","rate","count","popl")

#adjusted to year 2000 age distribution, rate per 100000

prostate <- subset(prostate,year.dx>0)
prostate$year.dx <- prostate$year.dx+1972
prostate$grade[prostate$grade==0] <- "grade I"
prostate$grade[prostate$grade==1] <- "grade II"
prostate$grade[prostate$grade==2] <- "grade III"
prostate$grade[prostate$grade==3] <- "grade IV"
prostate$grade[prostate$grade %in% 4:8] <- NA

prostate.grade.array <- by(prostate$rate,list(prostate$year.dx,prostate$grade), function(x) x)
