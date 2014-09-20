

#
# Effect based 
#

# (U)FEM (unequal) fixed effects model (Inverse variance method)
# efs is a matrix of effect sizes with rows corresponding to genes 
# and colums to studies
# sds is the corresponding standard deviation estimates the mean ef
meta.FEM <-
  meta.UFEM <- function(efs, sds) {
    prec<- 1/(sds*sds)
    w <- t(t(prec))
    W <- rowSums(w)
    invW <- 1/W
    meta.sd <- sqrt(invW)
    meta.z <- rowSums(efs*w)*invW
    meta.stat <- meta.z/meta.sd
    meta.p <- 2*pnorm(-abs(meta.stat))
    return(data.frame(meta.z, meta.sd, meta.stat, meta.p))
  }

meta.REM <- function(efs, sds) { # DerSimonian & Laird estimator
  d <- ncol(efs)
  prec <- 1/(sds*sds)
  #w <- t(t(prec)*n)w
  w <- prec
  W <- rowSums(w)
  
  invW <- 1/W
  meta.sds.ufem <- sqrt(invW)
  meta.efs.ufem <- rowSums(efs*w)*invW
  
  # DerSimonian-Laird estimate of gamma  
  Q <- rowSums(w*(efs - replicate(d, meta.efs.ufem))^2)
  M1 <- rowSums(w)
  M2 <- rowSums(w^2)
  gamma.sq <- ((Q-(d-1))*(sign(Q-(d-1))+1)/2)/(M1 - M2/M1)
  
  w.star <- (w^-1 + gamma.sq)^(-1)
  W.star <- rowSums(w.star)
  invW.star <- 1/W.star
  meta.efs.rem   <- rowSums(w.star*efs)*invW.star 
  meta.sds.rem   <- sqrt(invW.star)
  meta.stat.rem  <- meta.efs.rem/meta.sds.rem
  meta.p.rem     <- 2*pnorm(-abs(meta.stat.rem))
  
  return(data.frame("meta.z" = meta.efs.rem, "meta.sd" = meta.sds.rem, 
                    "meta.stat" =   meta.stat.rem, "meta.p" = meta.p.rem))
}