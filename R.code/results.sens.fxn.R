results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (cancer=="breast")
    results <- decomp.fxn(create.datos.sens.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.sens.fxn(mx, prop, year.list), year.list, mx.cause)
    return(results)
}
