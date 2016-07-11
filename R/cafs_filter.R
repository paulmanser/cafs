
cafs_filter <- function(object){
  
  n <- dim(object)[1]
  info.ind <- numeric(n)
  for (ii in 1:n)
    info.ind[ii] <- as.numeric(object[ii]@survival[, 2] == 0 & all(is.na(object[ii]@baseline_diff)))
      
  keep <- which(info.ind == 0)
  return(object[keep])
  
}
  