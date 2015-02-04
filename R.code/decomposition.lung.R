#rm(list=ls())

load("~/Cancer/data/mx.breast.Rdata")
load("~/Cancer/data/prop.breast.Rdata") 

load("~/Cancer/data/mx.lung.female.Rdata")
load("~/Cancer/data/prop.lung.female.Rdata") 
prop.lung.female <- t(prop.lung.female)

load("~/Cancer/data/mx.lung.male.Rdata")
load("~/Cancer/data/prop.lung.male.Rdata") 
prop.lung.male <- t(prop.lung.male)

load("~/Cancer/data/mx.crc.female.Rdata")
load("~/Cancer/data/prop.crc.female.Rdata") 
prop.crc.female <- t(prop.crc.female)

load("~/Cancer/data/mx.crc.male.Rdata")
load("~/Cancer/data/prop.crc.male.Rdata") 
prop.crc.male <- t(prop.crc.male)


library(foreign)
library(survival)
source("~/Cancer/R.code/lifetable.R")
source("~/Cancer/R.code/nAx.fxn.allcause.mort.r")
source("~/Cancer/R.code/nAx.fxn.R")
source("~/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Cancer/R.code/Assoc_LT.r")
source("~/Cancer/R.code/create.datos.fxn.R")
source("~/Cancer/R.code/decomp.fxn.R")
nax <- rep(1,22)
  
results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (cancer!="prostate")
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.fxn(mx, prop, year.list), year.list, mx.cause)
     
    return(results)
}

year0 <- 1981
year1 <- 1988
year2 <- 2001
breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(year0,year2))
lung.female <- results.fxn(mx.lung.female, mx.lung.female.cause, prop.lung.female, "lung", c(year1,year2))
lung.male <- results.fxn(mx.lung.male, mx.lung.male.cause, prop.lung.male, "lung", c(year1,year2))
crc.female <- results.fxn(mx.crc.female, mx.crc.female.cause, prop.crc.female, "crc", c(year0,year2))
crc.male <- results.fxn(mx.crc.male, mx.crc.male.cause, prop.crc.male, "crc", c(year0,year2))
