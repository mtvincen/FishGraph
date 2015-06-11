FGGetOptions <- function() {
  if (is.null(getOption('FGoptions'))) {
    FGSetDefaults()
  }
  getOption('FGoptions')
}
