## 'params' should contain b0, a0, a1 and b1
## ... arranged in this weird order to make it easy to
## remember that the sequence of values should be decreasing.
## additive_var is the amount of additive genetic variation.
## The output includes the mean value of z over time,
## and the predicted equilibrium value (a0-b0)/(b1-b0+a0-a1).

barnacle <- function(params, additive_var) {
  
  z <- 0.1
  b0 <- params[1]
  a0 <- params[2]
  a1 <- params[3]
  b1 <- params[4]

  ## (the number of generations could of course have been made 
  ## an argument of this function rather than stating it here)
  
  maxt <- 1000

  for (t in 1:(maxt-1)) {
    
    ## first Eq. (3.4)
    W <- z[t] * (z[t]/2*b1 + (1-z[t]/2)*b0) + (1-z[t])*((1+z[t])/2*a1+(1-z[t])/2*a0)
    
    ## then Eq. (3.6)
    deltaz <- additive_var / W * (z[t]*(b1-a1)+(1-z[t])*(b0-a0))
    
    ## the new z is the old one plus the change that occurred
    z[t+1] <- z[t] + deltaz
    
  }

  print(xyplot(z ~ 1:maxt, type="l",
               xlab="Generations", ylab="Switchpoint"))
  
  equilibrium <- (a0 - b0) / (b1 - b0 + a0 -a1)

  return(list(z=z, equilibrium=equilibrium))

}
