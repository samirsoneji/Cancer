rm(list=ls())

load("~/Desktop/Cancer/data/mx.breast.Rdata")
load("~/Desktop/Cancer/data/prop.breast.Rdata") 

load("~/Desktop/Cancer/data/mx.lung.Rdata")
load("~/Desktop/Cancer/data/prop.lung.Rdata")
prop.lung <- t(prop.lung)
load("~/Desktop/Cancer/data/mx.lung.female.Rdata")
load("~/Desktop/Cancer/data/prop.lung.female.Rdata") 
prop.lung.female <- t(prop.lung.female)
load("~/Desktop/Cancer/data/mx.lung.male.Rdata")
load("~/Desktop/Cancer/data/prop.lung.male.Rdata") 
prop.lung.male <- t(prop.lung.male)

load("~/Desktop/Cancer/data/mx.crc.Rdata")
load("~/Desktop/Cancer/data/prop.crc.Rdata")
prop.crc <- t(prop.crc)
load("~/Desktop/Cancer/data/mx.crc.female.Rdata")
load("~/Desktop/Cancer/data/prop.crc.female.Rdata") 
prop.crc.female <- t(prop.crc.female)
load("~/Desktop/Cancer/data/mx.crc.male.Rdata")
load("~/Desktop/Cancer/data/prop.crc.male.Rdata") 
prop.crc.male <- t(prop.crc.male)

load("~/Desktop/Cancer/data/mx.esophagus.Rdata")
load("~/Desktop/Cancer/data/prop.esophagus.Rdata")
prop.esophagus <- t(prop.esophagus)
load("~/Desktop/Cancer/data/mx.esophagus.female.Rdata")
load("~/Desktop/Cancer/data/prop.esophagus.female.Rdata") 
prop.esophagus.female <- t(prop.esophagus.female)
load("~/Desktop/Cancer/data/mx.esophagus.male.Rdata")
load("~/Desktop/Cancer/data/prop.esophagus.male.Rdata") 
prop.esophagus.male <- t(prop.esophagus.male)

load("~/Desktop/Cancer/data/mx.stomach.Rdata")
load("~/Desktop/Cancer/data/prop.stomach.Rdata")
prop.stomach <- t(prop.stomach)
load("~/Desktop/Cancer/data/mx.stomach.female.Rdata")
load("~/Desktop/Cancer/data/prop.stomach.female.Rdata") 
prop.stomach.female <- t(prop.stomach.female)
load("~/Desktop/Cancer/data/mx.stomach.male.Rdata")
load("~/Desktop/Cancer/data/prop.stomach.male.Rdata") 
prop.stomach.male <- t(prop.stomach.male)

load("~/Desktop/Cancer/data/mx.pancreas.Rdata")
load("~/Desktop/Cancer/data/prop.pancreas.Rdata")
prop.pancreas <- t(prop.pancreas)
load("~/Desktop/Cancer/data/mx.pancreas.female.Rdata")
load("~/Desktop/Cancer/data/prop.pancreas.female.Rdata") 
prop.pancreas.female <- t(prop.pancreas.female)
load("~/Desktop/Cancer/data/mx.pancreas.male.Rdata")
load("~/Desktop/Cancer/data/prop.pancreas.male.Rdata") 
prop.pancreas.male <- t(prop.pancreas.male)

load("~/Desktop/Cancer/data/mx.cervix.Rdata")
load("~/Desktop/Cancer/data/prop.cervix.Rdata") 

load("~/Desktop/Cancer/data/mx.uterus.Rdata")
load("~/Desktop/Cancer/data/prop.uterus.Rdata") 

load("~/Desktop/Cancer/data/mx.ovary.Rdata")
load("~/Desktop/Cancer/data/prop.ovary.Rdata") 

load("~/Desktop/Cancer/data/mx.prostate.Rdata")
load("~/Desktop/Cancer/data/prop.prostate.Rdata") 

load("~/Desktop/Cancer/data/mx.bladder.Rdata")
load("~/Desktop/Cancer/data/prop.bladder.Rdata")
prop.bladder <- t(prop.bladder)
load("~/Desktop/Cancer/data/mx.bladder.female.Rdata")
load("~/Desktop/Cancer/data/prop.bladder.female.Rdata") 
prop.bladder.female <- t(prop.bladder.female)
load("~/Desktop/Cancer/data/mx.bladder.male.Rdata")
load("~/Desktop/Cancer/data/prop.bladder.male.Rdata") 
prop.bladder.male <- t(prop.bladder.male)

load("~/Desktop/Cancer/data/mx.kidney.Rdata")
load("~/Desktop/Cancer/data/prop.kidney.Rdata")
prop.kidney <- t(prop.kidney)
load("~/Desktop/Cancer/data/mx.kidney.female.Rdata")
load("~/Desktop/Cancer/data/prop.kidney.female.Rdata") 
prop.kidney.female <- t(prop.kidney.female)
load("~/Desktop/Cancer/data/mx.kidney.male.Rdata")
load("~/Desktop/Cancer/data/prop.kidney.male.Rdata") 
prop.kidney.male <- t(prop.kidney.male)

load("~/Desktop/Cancer/data/mx.melanoma.Rdata")
load("~/Desktop/Cancer/data/prop.melanoma.Rdata")
prop.melanoma <- t(prop.melanoma)
load("~/Desktop/Cancer/data/mx.melanoma.female.Rdata")
load("~/Desktop/Cancer/data/prop.melanoma.female.Rdata") 
prop.melanoma.female <- t(prop.melanoma.female)
load("~/Desktop/Cancer/data/mx.melanoma.male.Rdata")
load("~/Desktop/Cancer/data/prop.melanoma.male.Rdata") 
prop.melanoma.male <- t(prop.melanoma.male)

load("~/Desktop/Cancer/data/mx.headneck.Rdata")
load("~/Desktop/Cancer/data/prop.headneck.Rdata")
prop.headneck <- t(prop.headneck)
load("~/Desktop/Cancer/data/mx.headneck.female.Rdata")
load("~/Desktop/Cancer/data/prop.headneck.female.Rdata") 
prop.headneck.female <- t(prop.headneck.female)
load("~/Desktop/Cancer/data/mx.headneck.male.Rdata")
load("~/Desktop/Cancer/data/prop.headneck.male.Rdata") 
prop.headneck.male <- t(prop.headneck.male)

load("~/Desktop/Cancer/data/mx.lymphoma.Rdata")
load("~/Desktop/Cancer/data/prop.lymphoma.Rdata")
prop.lymphoma <- t(prop.lymphoma)
load("~/Desktop/Cancer/data/mx.lymphoma.female.Rdata")
load("~/Desktop/Cancer/data/prop.lymphoma.female.Rdata") 
prop.lymphoma.female <- t(prop.lymphoma.female)
load("~/Desktop/Cancer/data/mx.lymphoma.male.Rdata")
load("~/Desktop/Cancer/data/prop.lymphoma.male.Rdata") 
prop.lymphoma.male <- t(prop.lymphoma.male)

source("~/Desktop/Cancer/R.code/lifetable.R")
source("~/Desktop/Cancer/R.code/decomp.ex.cd.fxn.R")
source("~/Desktop/Cancer/R.code/Assoc_LT.r")
source("~/Desktop/Cancer/R.code/create.datos.fxn.R")
source("~/Desktop/Cancer/R.code/decomp.fxn.R")
  
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

lung <- results.fxn(mx.lung, mx.lung.cause, prop.lung, "lung", c(1988,year1))
lung.female <- results.fxn(mx.lung.female, mx.lung.female.cause, prop.lung.female, "lung", c(1988,year1))
lung.male <- results.fxn(mx.lung.male, mx.lung.male.cause, prop.lung.male, "lung", c(1998,year1))
crc <- results.fxn(mx.crc, mx.crc.cause, prop.crc, "crc", c(year0,year1))
crc.female <- results.fxn(mx.crc.female, mx.crc.female.cause, prop.crc.female, "crc", c(year0,year1))
crc.male <- results.fxn(mx.crc.male, mx.crc.male.cause, prop.crc.male, "crc", c(year0,year1))
esophagus <- results.fxn(mx.esophagus, mx.esophagus.cause, prop.esophagus, "esophagus", c(year0,year1))
esophagus.female <- results.fxn(mx.esophagus.female, mx.esophagus.female.cause, prop.esophagus.female, "esophagus", c(year0,year1))
esophagus.male <- results.fxn(mx.esophagus.male, mx.esophagus.male.cause, prop.esophagus.male, "esophagus", c(year0,year1))
stomach <- results.fxn(mx.stomach, mx.stomach.cause, prop.stomach, "stomach", c(year0,year1))
stomach.female <- results.fxn(mx.stomach.female, mx.stomach.female.cause, prop.stomach.female, "stomach", c(year0,year1))
stomach.male <- results.fxn(mx.stomach.male, mx.stomach.male.cause, prop.stomach.male, "stomach", c(year0,year1))
pancreas <- results.fxn(mx.pancreas, mx.pancreas.cause, prop.pancreas, "pancreas", c(year0,year1))
pancreas.female <- results.fxn(mx.pancreas.female, mx.pancreas.female.cause, prop.pancreas.female, "pancreas", c(year0,year1))
pancreas.male <- results.fxn(mx.pancreas.male, mx.pancreas.male.cause, prop.pancreas.male, "pancreas", c(year0,year1))
cervix <- results.fxn(mx.cervix, mx.cervix.cause, prop.cervix, "cervix", c(1996,year1))
uterus <- results.fxn(mx.uterus, mx.uterus.cause, prop.uterus, "uterus", c(year0,year1))
ovary <- results.fxn(mx.ovary, mx.ovary.cause, prop.ovary, "ovary", c(year0,year1))
prostate <- results.fxn(mx.prostate, mx.prostate.cause, prop.prostate, "prostate", c(1995,year1))
bladder <- results.fxn(mx.bladder, mx.bladder.cause, prop.bladder, "bladder", c(year0,year1))
bladder.female <- results.fxn(mx.bladder.female, mx.bladder.female.cause, prop.bladder.female, "bladder", c(year0,year1))
bladder.male <- results.fxn(mx.bladder.male, mx.bladder.male.cause, prop.bladder.male, "bladder", c(year0,year1))
kidney <- results.fxn(mx.kidney, mx.kidney.cause, prop.kidney, "kidney", c(year0,year1))
kidney.female <- results.fxn(mx.kidney.female, mx.kidney.female.cause, prop.kidney.female, "kidney", c(year0,year1))
kidney.male <- results.fxn(mx.kidney.male, mx.kidney.male.cause, prop.kidney.male, "kidney", c(year0,year1))
melanoma <- results.fxn(mx.melanoma, mx.melanoma.cause, prop.melanoma, "melanoma", c(year0,year1))
melanoma.female <- results.fxn(mx.melanoma.female, mx.melanoma.female.cause, prop.melanoma.female, "melanoma", c(year0,year1))
melanoma.male <- results.fxn(mx.melanoma.male, mx.melanoma.male.cause, prop.melanoma.male, "melanoma", c(year0,year1))
headneck <- results.fxn(mx.headneck, mx.headneck.cause, prop.headneck, "headneck", c(year0,year1))
headneck.female <- results.fxn(mx.headneck.female, mx.headneck.female.cause, prop.headneck.female, "headneck", c(year0,year1))
headneck.male <- results.fxn(mx.headneck.male, mx.headneck.male.cause, prop.headneck.male, "headneck", c(year0,year1))
lymphoma <- results.fxn(mx.lymphoma, mx.lymphoma.cause, prop.lymphoma, "lymphoma", c(1983,year1))
lymphoma.female <- results.fxn(mx.lymphoma.female, mx.lymphoma.female.cause, prop.lymphoma.female, "lymphoma", c(1983,year1))
lymphoma.male <- results.fxn(mx.lymphoma.male, mx.lymphoma.male.cause, prop.lymphoma.male, "lymphoma", c(1983,year1))

results <- list()
results[[1]] <- breast
results[[2]] <- lung
results[[3]] <- lung.female
results[[4]] <- lung.male
results[[5]] <- crc
results[[6]] <- crc.female
results[[7]] <- crc.male
results[[8]] <- esophagus
results[[9]] <- esophagus.female
results[[10]] <- esophagus.male
results[[11]] <- stomach
results[[12]] <- stomach.female
results[[13]] <- stomach.male
results[[14]] <- pancreas
results[[15]] <- pancreas.female
results[[16]] <- pancreas.male
results[[17]] <- cervix
results[[18]] <- uterus
results[[19]] <- ovary
results[[20]] <- prostate
results[[21]] <- bladder
results[[22]] <- bladder.female
results[[23]] <- bladder.male
results[[24]] <- kidney
results[[25]] <- kidney.female
results[[26]] <- kidney.male
results[[27]] <- melanoma
results[[28]] <- melanoma.female
results[[29]] <- melanoma.male
results[[30]] <- headneck
results[[31]] <- headneck.female
results[[32]] <- headneck.male
results[[33]] <- lymphoma
results[[34]] <- lymphoma.female
results[[35]] <- lymphoma.male

names(results) <- c("breast",
                    "lung",
                    "lung.female",
                    "lung.male",
                    "crc",
                    "crc.female",
                    "crc.male",
                    "esophagus",
                    "esophagus.female",
                    "esophagus.male",
                    "stomach",
                    "stomach.female",
                    "stomach.male",
                    "pancreas",
                    "pancreas.female",
                    "pancreas.male",
                    "cervix",
                    "uterus",
                    "ovary",
                    "prostate",
                    "bladder",
                    "bladder.female",
                    "bladder.male",
                    "kidney",
                    "kidney.female",
                    "kidney.male",
                    "melanoma",
                    "melanoma.female",
                    "melanoma.male",
                    "headneck",
                    "headneck.female",
                    "headneck.male",
                    "lymphoma",
                    "lymphoma.female",
                    "lymphoma.male")

save(results, file="~/Desktop/Cancer/results/results.Rdata")
   
