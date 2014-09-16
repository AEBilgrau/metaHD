getMin <- function (x) {  # Compute min statistic
  return(do.call(pmin, as.list(data.frame(x))))
  #return(apply(x, 1, min))
}
