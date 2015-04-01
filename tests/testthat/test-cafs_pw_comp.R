

context("Testing paiwise comparison method for CAFS")


# compare on survival alone -----------------------------------------------
grp <- factor(1:2)
x <- CafsSet(survival = Surv(40, 1), baseline_diff = matrix(1:2, nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(22, 1), baseline_diff = matrix(1:2, nr=1), visit_times = 1:2, trt_group = grp[1])

test_that("Compare on Survival alone (x>y)", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))
})

test_that("Compare on Survival alone (y<x)", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))
})

x <- y
test_that("Compare on Survival alone (y=x)", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(0))
})

# survival and censoring --------------------------------------------------

x <- CafsSet(survival = Surv(40, 0), baseline_diff = matrix(1:2, nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(22, 1), baseline_diff = matrix(1:2, nr=1), visit_times = 1:2, trt_group = grp[1])

test_that("Compare on Survival & Censoring (x>y)", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))
})

test_that("Compare on Survival & Censoring (y<x)", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))
})

x <- CafsSet(survival = Surv(25, 1), baseline_diff = matrix(c(0,0), nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(20, 1), baseline_diff = matrix(-(1:2), nr=1), visit_times = 1:2, trt_group = grp[1])

test_that("Compare on Baseline Diff 1 censored", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))  
})

test_that("Compare on Baseline Diff 1 censored", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))  
})


# both censored -----------------------------------------------------------

x <- CafsSet(survival = Surv(25, 0), baseline_diff = matrix(c(0,0), nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(20, 0), baseline_diff = matrix(-(1:2), nr=1), visit_times = 1:2, trt_group = grp[1])

test_that("Compare on Baseline Diff both censored", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))  
})

test_that("Compare on Baseline Diff both censored", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))  
})

x <- CafsSet(survival = Surv(25, 0), baseline_diff = matrix(c(0,NA), nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(20, 0), baseline_diff = matrix(c(-1,4), nr=1), visit_times = 1:2, trt_group = grp[1])


test_that("Compare on Baseline Diff both censored 1 not obs at end of study", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))  
})

test_that("Compare on Baseline Diff both censored 1 not obs at end of study", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))  
})


x <- CafsSet(survival = Surv(25, 0), baseline_diff = matrix(c(0,NA), nr=1), visit_times = 1:2, trt_group = grp[1])
y <- CafsSet(survival = Surv(20, 0), baseline_diff = matrix(c(-1,NA), nr=1), visit_times = 1:2, trt_group = grp[1])


test_that("Compare on Baseline Diff both censored both not obs at end of study", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(x, y)), equals(1))  
})

test_that("Compare on Baseline Diff both censored both not obs at end of study", {
  expect_that(as.numeric(cafs:::.cafs_pw_comp(y, x)), equals(-1))  
})



