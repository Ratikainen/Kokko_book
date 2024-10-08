## Graphical explanation for the shifting distributions,
## given the additive genetic variance, the initial mean of a 
## distribution, and a and b of the function r(z) = a(z-b).
## Outputs are the magnitude of the shift, and the old
## and new distributions f0 and f1, which can be plotted 
## against z.

distr_shift <- function(variance, initial_mean, a, b) {

  ## we create 201 different
  ## values of z that lie between 13 and 17 (note that the
  ## function is not very general as we assume the
  ## initial mean will fall between these values)
  z <- seq(13, 17, length.out=201)
  
  ## the following is the density function of the normal
  ## distribution
  ## f <- 1/(2*sqrt(variance))*exp(-0.5*((z-initial_mean)/(sqrt(variance)))^2)
  ## here we use a standard R function instead
  f <- dnorm(z, mean=initial_mean, sd=sqrt(variance))

  ## Then normalize it, which means ensuring that values sum up to 1
  ## - they might not initially because we only have a selection of 
  ## discrete values of z, not 'all possible' values
  f0 <- f / sum(f)

  r <- a * (z-b)

  f1 <- f0 * exp(r) ## the new distribution
  f1 <- f1 / sum(f1) ## normalize f1 as well

  print(xyplot(f0 + f1 ~ z, type="l"))

  ## calculate the new mean as a weighted sum of values of z
  new_mean <- sum(f1 * z)
  shift <- new_mean - initial_mean

  return(list(shift=shift, f0=f0, f1=0, z=z))

}
