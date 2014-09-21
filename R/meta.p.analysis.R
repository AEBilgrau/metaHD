meta.p.analysis <- function (p, # Matrix of p values or ranked p values 
                             permute.p.values = missing(null.p), 
                             method = c("max","min","fisher","stouffer")[3],
                             null.p,  # list of matrices of p-values under null,
                             n.permutations = 50) {
  if (!(method %in% c("max","min","fisher","stouffer")))
    stop("The method is not supported")
  if (!is.matrix(p))
    stop("p is not a list of matrices")
  
  getStat <- switch(method, "max" = getMax, "min" = getMin, 
                    "fisher" = getFisher, 
                    "stouffer" = getStouffer, "AW" = getAW)
  meta.stat <- getStat(p)
  
  switch(method, 
         "max" = {meta.p <- punif(meta.stat)^(ncol(p))}, 
         "min" = {meta.p <- 1-(1-punif(meta.stat))^(ncol(p))}, 
         "fisher" = {meta.p <- 1-pchisq(meta.stat, df = 2*ncol(p))}, 
         "stouffer" = {meta.p <- 1-pnorm(meta.stat)})
  
  ans <- data.frame(cbind("meta.stat" = meta.stat, "meta.p" = meta.p))
  rownames(ans) <- rownames(p)
  
  if (!missing(null.p)) {
    if (!all(sapply(null.p, is.matrix) & is.list(null.p)))
      stop("null.p is not a list of matrices")
    null.stat.p         <- sapply(null.p, getStat)
    ans$meta.p.emp.data <- ecdf(c(null.stat.p))(meta.stat)
    if (method %in% c("fisher", "stouffer"))
      ans$meta.p.emp.data <- 1 - ans$meta.p.emp.data
  }
  
  if (permute.p.values) {
    permuted.p  <- replicate(n.permutations, apply(p, 2, sample),
                             simplify = FALSE)
    null.stat.p <- sapply(permuted.p, getStat)
    ans$meta.p.emp.perm <- ecdf(c(null.stat.p))(meta.stat)
    if (method %in% c("fisher", "stouffer"))
      ans$meta.p.emp.perm <- 1-ans$meta.p.emp.perm
  }
  
  return(ans)
}


