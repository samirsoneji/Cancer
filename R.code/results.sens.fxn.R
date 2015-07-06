results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (cancer=="breast")
    results <- decomp.fxn(create.datos.sens.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.sens.fxn(mx, prop, year.list), year.list, mx.cause)
    return(results)
}

results.age.fxn <- function(mx, mx.cause, prop, counts, cancer, year.list) {
   if (cancer=="breast")
    results <- decomp.age.fxn(create.datos.age.sens.fxn(mx, prop, counts, year.list), year.list, mx.cause)
   return(results)
}
