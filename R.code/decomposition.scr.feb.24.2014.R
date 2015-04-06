rm(list=ls())

load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 

load("~/Desktop/Cancer/data/mx.crc.Rdata")
load("~/Desktop/Cancer/data/prop.crc.Rdata")
prop.crc <- t(prop.crc)
load("~/Desktop/Cancer/data/mx.crc.female.Rdata")
load("~/Desktop/Cancer/data/prop.crc.female.Rdata") 
prop.crc.female <- t(prop.crc.female)
load("~/Desktop/Cancer/data/mx.crc.male.Rdata")
load("~/Desktop/Cancer/data/prop.crc.male.Rdata") 
prop.crc.male <- t(prop.crc.male)


load("~/Desktop/Cancer/data/mx.cervix.Rdata")
load("~/Desktop/Cancer/data/prop.cervix.Rdata") 

load("~/Desktop/Cancer/data/mx.prostate.Rdata")
load("~/Desktop/Cancer/data/prop.prostate.Rdata") 

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.fxn.R")
  
results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (!(cancer %in% c("prostate","cervix")))
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="cervix")
    results <- decomp.cervix.fxn(create.datos.cervix.fxn(mx, prop, year.list), year.list, mx.cause)
    return(results)
}

year0 <- 1973
year1 <- 2001

breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(year0,year1))
crc <- results.fxn(mx.crc, mx.crc.cause, prop.crc, "crc", c(year0,year1))
crc.female <- results.fxn(mx.crc.female, mx.crc.female.cause, prop.crc.female, "crc", c(year0,year1))
crc.male <- results.fxn(mx.crc.male, mx.crc.male.cause, prop.crc.male, "crc", c(year0,year1))
cervix <- results.fxn(mx.cervix, mx.cervix.cause, prop.cervix, "cervix", c(year0,year1))
prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1995,year1))
