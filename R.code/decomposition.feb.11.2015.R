rm(list=ls())

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

load("~/Cancer/data/mx.esophagus.female.Rdata")
load("~/Cancer/data/prop.esophagus.female.Rdata") 
prop.esophagus.female <- t(prop.esophagus.female)
load("~/Cancer/data/mx.esophagus.male.Rdata")
load("~/Cancer/data/prop.esophagus.male.Rdata") 
prop.esophagus.male <- t(prop.esophagus.male)

load("~/Cancer/data/mx.stomach.female.Rdata")
load("~/Cancer/data/prop.stomach.female.Rdata") 
prop.stomach.female <- t(prop.stomach.female)
load("~/Cancer/data/mx.stomach.male.Rdata")
load("~/Cancer/data/prop.stomach.male.Rdata") 
prop.stomach.male <- t(prop.stomach.male)

load("~/Cancer/data/mx.pancreas.female.Rdata")
load("~/Cancer/data/prop.pancreas.female.Rdata") 
prop.pancreas.female <- t(prop.pancreas.female)
load("~/Cancer/data/mx.pancreas.male.Rdata")
load("~/Cancer/data/prop.pancreas.male.Rdata") 
prop.pancreas.male <- t(prop.pancreas.male)

load("~/Cancer/data/mx.cervix.Rdata")
load("~/Cancer/data/prop.cervix.Rdata") 

load("~/Cancer/data/mx.uterus.Rdata")
load("~/Cancer/data/prop.uterus.Rdata") 

load("~/Cancer/data/mx.ovary.Rdata")
load("~/Cancer/data/prop.ovary.Rdata") 

load("~/Cancer/data/mx.prostate.Rdata")
load("~/Cancer/data/prop.prostate.Rdata") 

load("~/Cancer/data/mx.bladder.female.Rdata")
load("~/Cancer/data/prop.bladder.female.Rdata") 
prop.bladder.female <- t(prop.bladder.female)
load("~/Cancer/data/mx.bladder.male.Rdata")
load("~/Cancer/data/prop.bladder.male.Rdata") 
prop.bladder.male <- t(prop.bladder.male)


load("~/Cancer/data/mx.kidney.female.Rdata")
load("~/Cancer/data/prop.kidney.female.Rdata") 
prop.kidney.female <- t(prop.kidney.female)
load("~/Cancer/data/mx.kidney.male.Rdata")
load("~/Cancer/data/prop.kidney.male.Rdata") 
prop.kidney.male <- t(prop.kidney.male)

load("~/Cancer/data/mx.melanoma.female.Rdata")
load("~/Cancer/data/prop.melanoma.female.Rdata") 
prop.melanoma.female <- t(prop.melanoma.female)
load("~/Cancer/data/mx.melanoma.male.Rdata")
load("~/Cancer/data/prop.melanoma.male.Rdata") 
prop.melanoma.male <- t(prop.melanoma.male)

load("~/Cancer/data/mx.headneck.female.Rdata")
load("~/Cancer/data/prop.headneck.female.Rdata") 
prop.headneck.female <- t(prop.headneck.female)
load("~/Cancer/data/mx.headneck.male.Rdata")
load("~/Cancer/data/prop.headneck.male.Rdata") 
prop.headneck.male <- t(prop.headneck.male)

load("~/Cancer/data/mx.lymphoma.female.Rdata")
load("~/Cancer/data/prop.lymphoma.female.Rdata") 
prop.lymphoma.female <- t(prop.lymphoma.female)
load("~/Cancer/data/mx.lymphoma.male.Rdata")
load("~/Cancer/data/prop.lymphoma.male.Rdata") 
prop.lymphoma.male <- t(prop.lymphoma.male)

source("~/Cancer/R.code/lifetable.R")
source("~/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Cancer/R.code/Assoc_LT.r")
source("~/Cancer/R.code/create.datos.fxn.R")
source("~/Cancer/R.code/decomp.fxn.R")
  
results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (!(cancer %in% c("prostate","lymphoma")))
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="lymphoma")
    results <- decomp.lymphoma.fxn(create.datos.lymphoma.fxn(mx, prop, year.list), year.list, mx.cause)
    return(results)
}

year0 <- 1973
year1 <- 2001

breast <- results.fxn(mx.breast, mx.breast.cause, prop.breast, "breast", c(year0,year1))
lung.female <- results.fxn(mx.lung.female, mx.lung.female.cause, prop.lung.female, "lung", c(1988,year1))
lung.male <- results.fxn(mx.lung.male, mx.lung.male.cause, prop.lung.male, "lung", c(1998,year1))
crc.female <- results.fxn(mx.crc.female, mx.crc.female.cause, prop.crc.female, "crc", c(year0,year1))
crc.male <- results.fxn(mx.crc.male, mx.crc.male.cause, prop.crc.male, "crc", c(year0,year1))
esophagus.female <- results.fxn(mx.esophagus.female, mx.esophagus.female.cause, prop.esophagus.female, "esophagus", c(year0,year1))
esophagus.male <- results.fxn(mx.esophagus.male, mx.esophagus.male.cause, prop.esophagus.male, "esophagus", c(year0,year1))
stomach.female <- results.fxn(mx.stomach.female, mx.stomach.female.cause, prop.stomach.female, "stomach", c(year0,year1))
stomach.male <- results.fxn(mx.stomach.male, mx.stomach.male.cause, prop.stomach.male, "stomach", c(year0,year1))
pancreas.female <- results.fxn(mx.pancreas.female, mx.pancreas.female.cause, prop.pancreas.female, "pancreas", c(year0,year1))
pancreas.male <- results.fxn(mx.pancreas.male, mx.pancreas.male.cause, prop.pancreas.male, "pancreas", c(year0,year1))
cervix <- results.fxn(mx.cervix, mx.cervix.cause, prop.cervix, "cervix", c(year0,year1))
uterus <- results.fxn(mx.uterus, mx.uterus.cause, prop.uterus, "uterus", c(year0,year1))
ovary <- results.fxn(mx.ovary, mx.ovary.cause, prop.ovary, "ovary", c(year0,year1))
prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1995,year1))
bladder.female <- results.fxn(mx.bladder.female, mx.bladder.female.cause, prop.bladder.female, "bladder", c(year0,year1))
bladder.male <- results.fxn(mx.bladder.male, mx.bladder.male.cause, prop.bladder.male, "bladder", c(year0,year1))
kidney.female <- results.fxn(mx.kidney.female, mx.kidney.female.cause, prop.kidney.female, "kidney", c(year0,year1))
kidney.male <- results.fxn(mx.kidney.male, mx.kidney.male.cause, prop.kidney.male, "kidney", c(year0,year1))
melanoma.female <- results.fxn(mx.melanoma.female, mx.melanoma.female.cause, prop.melanoma.female, "melanoma", c(year0,year1))
melanoma.male <- results.fxn(mx.melanoma.male, mx.melanoma.male.cause, prop.melanoma.male, "melanoma", c(year0,year1))
headneck.female <- results.fxn(mx.headneck.female, mx.headneck.female.cause, prop.headneck.female, "headneck", c(year0,year1))
headneck.male <- results.fxn(mx.headneck.male, mx.headneck.male.cause, prop.headneck.male, "headneck", c(year0,year1))
lymphoma.female <- results.fxn(mx.lymphoma.female, mx.lymphoma.female.cause, prop.lymphoma.female, "lymphoma", c(1983,year1))
lymphoma.male <- results.fxn(mx.lymphoma.male, mx.lymphoma.male.cause, prop.lymphoma.male, "lymphoma", c(1983,year1))

results <- list()
results[[1]] <- breast
results[[2]] <- lung.female
results[[3]] <- lung.male
results[[4]] <- crc.female
results[[5]] <- crc.male
results[[6]] <- esophagus.female
results[[7]] <- esophagus.male
results[[8]] <- stomach.female
results[[9]] <- stomach.male
results[[10]] <- pancreas.female
results[[11]] <- pancreas.male
results[[12]] <- cervix
results[[13]] <- uterus
results[[14]] <- ovary
results[[15]] <- prostate
results[[16]] <- bladder.female
results[[17]] <- bladder.male
results[[18]] <- kidney.female
results[[19]] <- kidney.male
results[[20]] <- melanoma.female
results[[21]] <- melanoma.male
results[[22]] <- headneck.female
results[[23]] <- headneck.male
results[[24]] <- lymphoma.female
results[[25]] <- lymphoma.male

names(results) <- c("breast",
                    "lung.female",
                    "lung.male",
                    "crc.female",
                    "crc.male",
                    "esophagus.female",
                    "esophagus.male",
                    "stomach.female",
                    "stomach.male",
                    "pancreas.female",
                    "pancreas.male",
                    "cervix",
                    "uterus",
                    "ovary",
                    "prostate",
                    "bladder.female",
                    "bladder.male",
                    "kidney.female",
                    "kidney.male",
                    "melanoma.female",
                    "melanoma.male",
                    "headneck.female",
                    "headneck.male",
                    "lymphoma.female",
                    "lymphoma.male")

save(results, file="~/Cancer/results/results.Rdata")
