context("Testing methods for 'cafs_obj' construction")

library(survsim)

# create dummy cafs data

group <- factor(rep(c('trt','ctrl'), each = 100))  # trt/ctl group membshp

long <- rbind(matrix(0, nr = 100, nc = 4) + rnorm(400),
              matrix(0, nr = 100, nc = 4, byrow=TRUE) + rnorm(400))  # long data
              
visit.times <- c(0, 90, 180, 270)  # check in times including baseline

trt.sv <- runif(n = 100, min = 10, max = 300)  # generate basic survival data
ctl.sv <- runif(n = 100, min = 10, max = 300)

trt.sv[trt.sv > 270] <- 270  # patients are right cens if survival past end of study
ctl.sv[ctl.sv > 270] <- 270

sv.dat <- c(trt.sv, ctl.sv)

drop.out <- sample(length(sv.dat), 5)

for(ii in 1:length(drop.out)){
  if( sv.dat[drop.out[ii]] != 270){
    n.m <- sum(visit.times > sv.dat[drop.out[ii]])
    long[drop.out[ii],  (ncol(long) - n.m + 1): ncol(long)    ] <- NA
  }  
}

cens <- rep(1, 200)
cens[union(drop.out, which(sv.dat==270))] <- 0

sv.fin <- Surv(sv.dat, cens)
bl.d <- long[, -1] - long[, 1]

rm.ind <- which(rowSums(is.na(bl.d))==3)

bl.d <- bl.d[-rm.ind, ]
sv.fin <- sv.fin[-rm.ind]
group <- group[-rm.ind]

# create object -----------------------------------------------------------

cafs.t <- CafsSet(survival = sv.fin, baseline_diff = bl.d,
                  visit_times = visit.times[-1], trt_group = group)
  
# CafsSet creation
test_that("CafsSet creation", {
  expect_that(class(cafs.t)[1], equals("CafsSet"))
})


# subsetting --------------------------------------------------------------

# asdf = compute_cafs_scores(cafs.t)





