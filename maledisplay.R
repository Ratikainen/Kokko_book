## 'resourcerange' should be a vector with two numbers
## - one for the low end of the range considered, the other the high end
## There are four outputs:
## Trait - the male trait (for each value of R)
## Seasons - how many mating seasons each male is expected to live
## Fit - the males' fitness
## R - the resource values considered here; these will be 100 values 
## between resourcerange[1] and resourcerange[2]

maledisplay <- function(resourcerange) {

  R <- seq(resourcerange[1], resourcerange[2], length.out=100)
  ## the above lets us consider 100 different males,
  ## resources varying between the lowest and highest
  ## value given as inputs

  Trait <- Seasons <- Fit <- numeric(length(R))

  for (i in seq(along=R)) {
    
    ## we consider the ith male, i.e. ith possible resource value
    ## possible trait values range between 0 and R[i]
    trait <- seq(0 , R[i], length.out=1000)
    netcondition <- R[i] - trait
    
    ## this is the function used in Fig. (4.6)
    survival <- 1 / (1 + exp(-10 * (netcondition - 0.5)))
    
    ## expected number of future mating seasonsmigr is the
    ## result of the infinite sum given in Eq. 4.7
    futurematingseasons <- survival / (1 - survival)

    ## mating success is just proportional to the trait...
    m <- trait
    
    ## fitness now from eq. 4.8
    fitness <- m + m * futurematingseasons
    
    ## R's 'which.max' function finds the location (index) of the maximum value
    ## within a vector of numbers
    bestindex <- which.max(fitness)
    
    ## then we need to check which trait value produced this best value...
    Trait[i] <- trait[bestindex]

    ## ...and let's also calculate how long the male survives in this case
    Seasons[i] <- 1 + futurematingseasons[bestindex]
    
    ## ...as well as save the information on this male's fitness 
    ## when displaying optimally
    Fit[i] <- fitness[bestindex]
    
  }
  
  subplot1 <- xyplot(Trait ~ R, type="l",
                     xlab="Resources", ylab="Trait")
  subplot2 <- xyplot(Seasons ~ R, type="l",
                     xlab="Resources", ylab="Expected number\nof mating seasons")
  subplot3 <- xyplot(Fit ~ R, type="l",
                     xlab="Resources", ylab="Fitness")

  print(subplot1, split=c(1, 1, 1, 3), more=TRUE)
  print(subplot2, split=c(1, 2, 1, 3), more=TRUE)
  print(subplot3, split=c(1, 3, 1, 3))
                  
  return(list(Trait=Trait, Seasons=Seasons, Fit=Fit, R=R))

}
