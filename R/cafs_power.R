

cafs_power <- function(alpha = 0.05, n.trt = 130, n.pbo = 130, study.length = 18, dropout = .25, 
                       frs.sd = 9.1, pbo.slope = 1.1, reduce.decline = .25,
                       med.surv.pbo = 24, med.surv.trt = 24 + 6){
  

  # ~65 % for n = 170, alpha = .05, study.length = 18, sd = 8.1
  
#   n.trt <- n.trt * (1-dropout)
#   n.pbo <- n.pbo * (1-dropout)
  
  lam.pbo <- log(2)/med.surv.pbo
  lam.trt <- log(2)/med.surv.trt
  theta <- lam.pbo/lam.trt

  mean.pbo <- pbo.slope*study.length
  mean.trt <- mean.pbo*(1-reduce.decline)
    
  p1 <- pexp(study.length, lam.pbo)  # p(dying before end of study|pbo)
  p2 <- pexp(study.length, lam.trt)  # p(dying before end of study|trt)
  q1 <- 1 - p1
  q2 <- 1 - p2
  
  x1 <- 1 - q2^(1+theta) #
  y1 <- (1+theta)*p2 #
  pi_t1 <- (1/(1-q2^theta)) * ( 1 - (x1) / (y1) ) #
  
  x1 <- 1 - q2^(1+2*theta) #
  y1 <- 1 + 2*theta #
  x2 <- 2*(1-q2^(1+theta)) #
  y2 <- 1 + theta #
  pi_t2 <- (1/(1-q2^theta))^2 * (1 + (1/p2) * (  (x1/y1) - (x2/y2) )) #
  
  x1 <- theta*(1 - q2^(2+theta)) #
  y1 <- (2+theta) * (1-q2^theta) * q2^2 #
  x2 <- 2*theta*(1-q2^(1+theta)) #
  y2 <- (1+theta)*(1-q2^theta)*q2 #
  pi_t3 <- (q2/p2)^2  * (1 + x1/y1 - x2/y2) #
  
  
  delta <- abs((mean.trt - mean.pbo) / (sqrt(2)*frs.sd)) #
  
  
  pi_x1 <- pnorm(delta) # 
  pi_x2 <- pi_x3 <- pmvnorm(lower=-Inf, upper=c(delta, delta), mean=c(0,0), corr = matrix(c(1, .5, .5, 1), 2, 2))[1] #
  
  pi_u1 <- p1*p2*pi_t1 + p1*q2 + q1*q2*pi_x1  #
  pi_u2 <- p1^2*q2 + p1^2*p2*pi_t2 + 2*p1*q1*q2*pi_x1 + q1^2*q2*pi_x2  #
  pi_u3 <- p1*q2^2 + p1*p2^2*pi_t3 + 2*p1*p2*q2*pi_t1 + q1*q2^2*pi_x3  #
  
  
  mu0 <- 0.5  # 
  sigma2_0 <- (n.trt+n.pbo+1) / (12*n.trt*n.pbo) #
  
  mu1 <- pi_u1
  sigma2_1 <- (1/(n.trt*n.pbo)) *  ( pi_u1*(1-pi_u1) +  (n.pbo-1)*(pi_u2 - pi_u1^2)  + (n.trt - 1)*(pi_u3 - pi_u1^2) )  #
   
  sigma0 <- sqrt(sigma2_0)
  sigma1 <- sqrt(sigma2_1)
  
  pnorm((sigma0/sigma1)*(qnorm(alpha/2)) + abs(mu1-mu0)/sigma1)
  
  
}
  
  