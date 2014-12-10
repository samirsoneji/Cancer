kan<-function(data,ages,kan.age.start,kan.age.end,max.proj.age){
    data<-data.frame(cbind(ages,data))
    colnames(data)<-c("age","nmx")
    foo<-subset(data,kan.age.start<=age & age<=kan.age.end) #selecting the ages where the Kannisto model is fitted
    data.centered<-data.frame(age=foo$age-foo$age[1],logit=log(foo[,2]/(1-foo[,2]))) # centering the ages so the intercept corresponds to age=0, and computing the logit of mort rates
    data.centered<-data.frame(data.centered,logit.c=data.centered$logit-data.centered$logit[1]) # centering the logits so the linear model on the logits gives back the starting value when age=0
    lmfit<-lm(logit.c~0+age,dat=data.centered) # linear fit with no intercept, we only care about the slope.
    newage<-data.frame(age=c(seq(kan.age.start,max.proj.age,5)-kan.age.start))
    pred<-predict.lm(lmfit,newdata=newage)
    mx.pred<-exp(pred+data.centered$logit[1])/(1+exp(pred+data.centered$logit[1])) #transforming back the logits into rates using the Kannisto-logistic function
    return(data.frame(age=newage+kan.age.start,Mx=mx.pred))
}
