
# Define ------------------------------------------------------------------

#' @exportClass CafsSet

setOldClass("Surv")

setClass("CafsSet",
         slots = c(survival = "Surv",
                   baseline_diff = "matrix",
                   visit_times = "numeric",
                   trt_group = "factor"))

# Validate ----------------------------------------------------------------

.valid_CafsSet <- function(object) {
  
  if (ncol(object@baseline_diff) != length(object@visit_times)) {
    stop("Number of columns of 'baseline_diff' must match length of 'visit_times'")
  }  
  
  if (length(object@trt_group) != nrow(object@baseline_diff)) {
    stop("Length of 'trt_group' must be the same as number of rows in 'baseline_diff'")
  }
  
  if (nlevels(object@trt_group) != 2) {
    stop("'trt_group' must be a factor with 2 levels")
  }
  
  if (nrow(object@survival) != nrow(object@baseline_diff)) {
    stop("Length of 'survival' must be equal to number of rows in 'baseline_diff'")
  }
  
  return(TRUE)
}

setValidity("CafsSet", .valid_CafsSet)


# Construct ---------------------------------------------------------------

CafsSet <- function(survival, baseline_diff, visit_times, trt_group){
  
  new("CafsSet", 
      survival = survival,
      baseline_diff = baseline_diff,
      visit_times = visit_times,
      trt_group = trt_group)  
}

# Accessors ---------------------------------------------------------------

#' @export get_survival
setMethod("get_survival", "CafsSet", function(x) x@survival)

#' @export get_baseline_diff
setMethod("get_baseline_diff", "CafsSet", function(x) x@baseline_diff)

#' @export get_visit_times
setMethod("get_visit_times", "CafsSet", function(x) x@visit_times)

#' @export get_trt_group
setMethod("get_trt_group", "CafsSet", function(x) x@trt_group)

# Summaries ---------------------------------------------------------------

setMethod("show", "CafsSet", function(object) {
  cat("A CafsSet object \n")
  cat(nrow(object@baseline_diff), "subjects \n")
  cat("Differences from baseline bserved at", ncol(object@baseline_diff), "time points \n")
  cat(sum(object@survival[, 2]==1), "patients experienced event \n")
})

setMethod("dim", "CafsSet", function(x){
  c(patients = nrow(x@baseline_diff), 
    n_visits = ncol(x@baseline_diff), 
    n_events = sum(x@survival[, 2] == 1))
})


setMethod("[", c("CafsSet", "ANY", "missing"),
          function(x, i, j, ..., drop = FALSE){
            new("CafsSet", 
                survival = x@survival[i, ],
                baseline_diff = x@baseline_diff[i, , drop=FALSE],
                visit_times = x@visit_times,
                trt_group = x@trt_group[i])  
          })


setMethod("plot", 'CafsSet',
          function(x){
            par(mfrow=c(1, 2))
            
            boxplot(x@baseline_diff[, ncol(x@baseline_diff)] ~ x@trt_group,
                    main = 'Difference from Baseline at End of Study')
            abline(h = 0)
            
            km <- npsurv(x@survival ~ x@trt_group)
            survplot(km, label.curves = FALSE, conf='none',
                     xlab = 'Time to Event', lwd = 1.6, col = 1:2)
            title('Survival by Treatment Arm')
            legend('bottomleft', legend = levels(x@trt_group), fill = 1:2)
            
            
          })








