
long_sim <- function(time.pts, baseline, s.dev, pbo.slope, n.trt, n.pbo, decl.red){
  
  pbo.dat <- matrix(baseline + time.pts*pbo.slope, nr = n.pbo, nc = length(time.pts), byrow=TRUE)
#   pbo.dat <- pbo.dat + rnorm(n = nrow(pbo.dat), mean = 0, sd = s.dev)
  pbo.dat <- pbo.dat + rnorm(n = length(pbo.dat), mean = 0, sd = s.dev)
  
  trt.dat <- matrix(baseline + time.pts*pbo.slope*(1-red.decl), 
                    nr = n.pbo, nc = length(time.pts), byrow=TRUE)
  
#   trt.dat <- trt.dat + rnorm(n = nrow(trt.dat), mean = 0, sd = s.dev)
  trt.dat <- trt.dat + rnorm(n = length(trt.dat), mean = 0, sd = s.dev)
  
  dat <- rbind(pbo.dat, trt.dat)
  grp <- rep(c('pbo', 'trt'), c(n.pbo, n.trt))
  
  data.frame(grp, dat)
}



