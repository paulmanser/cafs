
compute_cafs_scores <- function(object, return.scores = TRUE, return.ranks = TRUE){
  
  if(!is(object, 'CafsSet')) stop("object must be a 'CafsSet'")
  
  n <- dim(object)[1]
  mat.hold <- matrix(nr = n, nc = n)
  
  for (ii in 1:n){
    for (jj in ii:n){
      mat.hold[ii, jj] <- .cafs_pw_comp(object[ii], object[jj])
      mat.hold[jj, ii] <- -mat.hold[ii, jj]
    }
  }
  
  scores <- rowSums(mat.hold)
  
  output <- data.frame(trt_group = object@trt_group,
                       cafs_scores = scores,
                       cafs_ranks = order(scores))
  return(output)
}

.cafs_pw_comp <- function(x, y){
  
  comp.ind <- as.numeric(x@survival[, 2] == 1) + as.numeric(y@survival[, 2] == 1) # how many obs events
  
  if (comp.ind == 2){ # compare by survival if both events obs
    r.val <- 1*(x@survival[, 1] > y@survival[, 1]) + -1*(x@survival[, 1] < y@survival[, 1])
    return(r.val)
  }
  
  
  if (comp.ind == 1){ # only 1 obs event
    
    if (x@survival[, 2] == 0){ # if x is censored
      
      if (x@survival[, 1] >= y@survival[, 1]){ # x > y
        return(1)
      } else {
        dat <- rbind(x@baseline_diff, y@baseline_diff)
        comp.time <- dat[, which.max(which(colSums(is.na(dat)) == 0))]
        r.val <- 1*(comp.time[1] > comp.time[2]) + -1*(comp.time[1] < comp.time[2])
        return(r.val)     
      }
        
    }else{  # if y is censored
      if (x@survival[, 1] <= y@survival[, 1]){ # x > y
        return(-1)
      } else {
        dat <- rbind(x@baseline_diff, y@baseline_diff)
        comp.time <- dat[, which.max(which(colSums(is.na(dat)) == 0))]
        r.val <- 1*(comp.time[1] > comp.time[2]) + -1*(comp.time[1] < comp.time[2])
        return(r.val)      
      }
    }
  }
 
  if (comp.ind == 0){ # if neither event is observed
    dat <- rbind(x@baseline_diff, y@baseline_diff)
    comp.time <- dat[, which.max(which(colSums(is.na(dat)) == 0))]
    r.val <- 1*(comp.time[1] > comp.time[2]) + -1*(comp.time[1] < comp.time[2])
    return(r.val)
  }
    
}