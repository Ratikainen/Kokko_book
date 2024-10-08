## s contains migrant (s[1]) and non-migrant (s[2]) survival
## and parameters a, b describe the habitat.
## This function uses function pip() in file pip.R
## (make it available with 'source("pip.R")').

migrate <- function(s, a, b) {
  
  sm <- s[1]
  sn <- s[2]
  x <- seq(0, 1, length.out=101)

  ## equilibrium population sizes
  nm <- 2/b * sm * x * ((1+a) * (sm*x+sn*(1-x))-1)/(sm*x+sn*(1-x))^2
  nn <- 2/b * sn * (1-x) * ((1+a)*(sm*x+sn*(1-x))-1)/(sm*x+sn*(1-x))^2

  ## check if some extra modelling work is needed before
  ## these parameter values can be used
  if (any(a-b*(nn+nm) < 0)) {
    warning("Some individuals cannot breed! Better not to trust the program output right now.")
  }

  ## calculate average reproductive success: first
  ## nonmigrants, then migrants
  Rn <- a - b*nn/2
  Rm <- a - b*nn - b*nm/2

  ## fitness of nonmigrants and migrants in one year
  fit_nonmigr <- sn * (Rn+1)
  fit_migr <- sm * (Rm+1)

  ## then plot everything of interest: first the population consequences
  ## of a particular resident strategy
  figure1a <- xyplot(nn + nm + (nm+nn) ~ x, type=c("l", "g"),
                     xlab="", ylab="Spring population size")
  figure1b <- xyplot(fit_nonmigr + fit_migr ~ x, type=c("l", "g"),
                     xlab="Proportion of migrants", ylab="Fitness")

  ## this figure will show the difference between mutant and resident fitness
  xmut <- seq(0, 1, length.out=101)
  mutfit <- resfit <- matrix(nrow=101, ncol=101)
  for (i in 1:101) {
    for (j in 1:101) {
      mutfit[i,j] <- xmut[j] * fit_migr[i] + (1-xmut[j]) * fit_nonmigr[i]
      resfit[i,j] <- x[i] * fit_migr[i] + (1-x[i]) * fit_nonmigr[i]
    }
  }
  figure2 <- pip(x, xmut, mutfit-resfit)

  plot(figure1a, split=c(1, 1, 1, 3), more=TRUE)
  plot(figure1b, split=c(1, 2, 1, 3), more=TRUE)
  plot(figure2, split=c(1, 3, 1, 3))
  
  return(list(fit_migr=fit_migr, fit_nonmigr=fit_nonmigr, nm=nm, nn=nn))

}
