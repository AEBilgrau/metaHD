getMax <- function (x) {  # Compute max statistic
  return(do.call(pmax, as.list(data.frame(x)))) 
  #return(apply(x, 1, max))
}
