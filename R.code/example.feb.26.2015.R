load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 

stage <- t(prop.breast[as.character(c(1973,2001)),])
mx <- mx.breast["75",as.character(c(1973)),]
ex.stage <- create.datos.fxn(mx.breast,prop.breast,c(1973))[c("ex.insitu","ex.localized","ex.regional","ex.distant")]
