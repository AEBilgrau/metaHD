# Stouffer's method
getStouffer <- function (x) {
  return(rowSums(-qnorm(x))/sqrt(ncol(x)))
}
