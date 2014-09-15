# Adaptively Weighted Fisher
getAW <- function(x, null.x) {
  k <- ncol(x)
  W <- do.call("expand.grid", replicate(k, c(0,1), simplify = FALSE))
  W <- as.matrix(W[-1, ]); dimnames(W) <- NULL
  meta.stat <- tcrossprod(-log(x), W)
  attributes(meta.stat)$weight <- W
  colnames(meta.stat) <- paste("comb", 1:nrow(W), sep = "")
  return(meta.stat)
}
