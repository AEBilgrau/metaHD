# Product (p.values for test is the same as Fisher's test)
getProd <- function (x) {  # Compute product statistic
  return(exp(rowSums(log(x))))
}
