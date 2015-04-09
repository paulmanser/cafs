
surv_sim <- function(n.trt, n.pbo, med.surv.pbo, med.surv.trt, end.study){

  # potential future options: drp.out.rate = .1
  lam.pbo <- log(2) / med.surv.pbo
  lam.trt <- log(2) / med.surv.trt
  
  sv.pbo <- rexp(n.pbo, lam.pbo)
  sv.trt <- rexp(n.trt, lam.trt)
    
  sv.out <- Surv(c(sv.pbo, sv.trt))
  
#   cens.subs <- sample(nrow(sv.out),    nrow(sv.out) * sample(drp.out.rate)    )
#   sv.out[cens.subs, 2] <- 0
  
  sv.out[which(sv.out[, 1] > end.study), 2] <- 0
  sv.out[which(sv.out[, 1] > end.study), 1] <- end.study
    
  return(sv.out)

}