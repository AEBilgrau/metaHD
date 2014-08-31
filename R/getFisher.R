# Fisher's method
getFisher <- function (x) {
  return(-2*base::rowSums(log(x)))
}
