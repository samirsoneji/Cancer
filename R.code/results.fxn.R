results.fxn <- function(mx, mx.cause, prop, cancer, year.list) {
   if (!(cancer %in% c("prostate","lymphoma","cervix")))
    results <- decomp.fxn(create.datos.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="prostate")
    results <- decomp.prostate.fxn(create.datos.prostate.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="cervix")
    results <- decomp.cervix.fxn(create.datos.cervix.fxn(mx, prop, year.list), year.list, mx.cause)
  if (cancer=="lymphoma")
    results <- decomp.lymphoma.fxn(create.datos.lymphoma.fxn(mx, prop, year.list), year.list, mx.cause)
    return(results)
}
