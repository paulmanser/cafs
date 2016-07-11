
gw_test <- function(x, permutation_test = FALSE, n_perm = 1e3) { # currently only 2-sided functionality
  
  if (ncol(x) != 3 | !identical(colnames(x), c("trt_group", "cafs_scores", "cafs_ranks")))
    stop("'x' must be a data.frame output from the 'compute_cafs_scores' function 
         with 3 columns named 'trt_group', 'cafs_scores', and 'cafs_ranks', respectively")
  
  if (!is(x$trt_group, 'factor'))
    stop("The 'trt_group' column of x must be a factor")
  
  if (nlevels(x$trt_group) != 2) {
    stop("'trt_group' must be a factor with 2 levels")
  }
    
  if (!permutation_test){ 
    
    test_stat <- .gw_stat(x)
    p_val <- 1 - pnorm(abs(test_stat))
    return(c(test_stat = test_stat, p_val = p_val))
    
  }
  
  if (permutation_test){
    test_stat <- .gw_stat(x)
    p_val <- 2 * (1 - pnorm(abs(test_stat)))  # compute 2-sided p-value
    perm.tmp <- numeric(n_perm)
    tmp <- x
    
    for(ii in 1:n_perm){
      tmp$trt_group <- sample(tmp$trt_group)
      perm.tmp[ii] <- .gw_stat(tmp)
    }
    
    perm_pval <- sum(abs(perm.tmp) > abs(test_stat)) / n_perm
    
    return(c(test_stat = test_stat, p_val = p_val, perm_pval = perm_pval))
  }
  
}

.gw_stat <- function(x){
  
  n <- nrow(x)
  m <- sum(x$trt_group == 'trt')
  
  t <- sum( (x$trt_group == 'trt') * x$cafs_scores)
  v <- sum(x$cafs_scores^2) * ( m*(n-m)/(n*n-1) )
  
  return(t/sqrt(v)) 
  
}




