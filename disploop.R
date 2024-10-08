## Search for the U-shaped evolutionary rescue, by
## looking at 101 values of m from 0 to 1. Parameters:
## N: number of sites (must be an integer)
## B: competition, number of individuals per patch who can breed.
## Some sites get occasionally destroyed, probability p.
## Mutation probability is q.
## Breeding success is b, this should be an integer too.
## This function uses function dispersal() in file dispersal.R
## (make it available with 'source("dispersal.R")').

disploop <- function(N, B, p, q, b) {

  m <- seq(0, 1, length.out=101)

  ## Let's randomize the order in which each value of m is
  ## tested. This is just for entertainment: the plot will 
  ## begin to take shape more quickly than when starting
  ## from the low end of values for m.
  m <- sample(m)

  ## Meanx and Occupied will record the outcomes: mean
  ## dispersal propensity, and proportion of sites
  ## occupied.
  ## (Here we use capital letters to remind us that they
  ## gather the output from a large number of simulation
  ## runs)
  ## First make them vectors that are not numbers (NaN=not-
  ## a-number), but equal m in length, so values that are not
  ## yet calculated can be 'plotted' (they remain empty)
  ## along completed ones
  Meanx <- Occupied <- NaN*m

  ## then find out what happens...
  for (i in 1:length(m)) {
    
    print(i) ## display which value of i we are distcalculating now
    noplot <- TRUE ## let's prevent plotting in function dispersal()
    disp <- dispersal(N, B, p, q, b, m[i], noplot)

    ## the last value of the time series is of interest here
    Meanx[i] <- tail(disp$xmean, 1)
    Occupied[i] <- tail(disp$occupied, 1)
    
    ## only plot values of X if there is a population
    ## (i.e. Occupied > 0)
    figure1a <- xyplot(Meanx[Occupied > 0] ~ m[Occupied > 0],
                       xlim=c(-.04, 1.04), ylim=c(-.04, 1.04),
                       xlab="Migration mortality",
                       ylab="Mean dispersal at the end of the simulation")
    figure1b <- xyplot(Occupied ~ m,
                       xlim=c(-.04, 1.04), ylim=c(-.04, 1.04),
                       xlab="Migration mortality",
                       ylab="Proportion of occupied sites")
    print(figure1a, split=c(1, 1, 1, 2), more=TRUE)
    print(figure1b, split=c(1, 2, 1, 2))
    
  }
  
  return(list(Meanx=Meanx, Occupied=Occupied, m=m))

}
