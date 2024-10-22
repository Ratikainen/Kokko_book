## N sites. Migrants get randomly distributed.
## Competition: only some (B, must be an integer) can breed.
## Some sites get occasionally destroyed, probability p.
## Mutation probability is q.
## Breeding success is b, this too should be an integer.
## Migration mortality is m.
## The optional 'no_plot', if set to TRUE, suppresses the
## graphical output.
## Outputs: the time series of the mean migratory
## propensity in the population and its standard
## deviation over time (xmean, xstd),
## the size of the population (n),
## and the number of sites occupied (occupied).

dispersal <- function(N, B, p, q, b, m, no_plot=FALSE) {

  ## initialize population: 
  ## 1st column=strategy, 2nd column=location
  pop <- data.frame(strategy=runif(N), location=1:N)
  
  xmean <- xstd <- n <- occupied <- numeric()
  
  for (t in 1:1000) {
    
    ## data collection
    xmean[t] <- mean(pop[,1])
    xstd[t] <- sd(pop[,1])
    n[t] <- length(pop[,1])

    ## plot things
    if (!no_plot & t/20 == floor(t/20)) {
      figure1a <- xyplot(xmean ~ 1:t, type=c("l"),
                         xlab="", ylab="Mean dispersal rate",
                         main=paste("Mortality =", m),
                         xlim=c(-40, 1040), ylim=c(-.04, 1.04))
      figure1b <- xyplot(n ~ 1:t, type="l",
                         xlab="Time", ylab="Population size",
                         xlim=c(-40, 1040), ylim=c(-N*B*b*0.04, N*B*b*1.04))
      print(figure1a, split=c(1, 1, 1, 2), more=TRUE)
      print(figure1b, split=c(1, 2, 1, 2))
    }
    
    ## randomize order of individuals using ready-made function 'sample'
    ind <- sample(1:nrow(pop))
    pop <- pop[ind,]
    
    ## destruction of some sites. If a random number falls
    ## below p then that site will be listed in 'disasterplaces'.
    disaster <- which(runif(N) < p)
    disasterplaces <- intersect(pop[,2], disaster)
    
    ## now that disasterplaces have been found,
    ## individuals residing in them will cease to exist
    pop <- pop[!pop[,2] %in% disasterplaces,]
    
    ## competition occurs among the remaining individuals
    for (i in 1:N) {
      potentialbreeders <- which(pop[,2] == i)
      ## how many in site i?
      if (length(potentialbreeders) > B) {
        ## too many try to breed here
        ## superfluous individuals are removed
        pop <- pop[-potentialbreeders[seq(along=potentialbreeders) > B],]
      }
    }
    
    ## collect data:
    ## how many sites are occupied, thus what proportion is empty?
    occupied[t] <- length(unique(pop[,2])) / N
    
    ## if the population went extinct, we may stop here
    if (nrow(pop) == 0) break
    
    ## reproductive output is collected in 'newpop'
    newpop <- data.frame()
    for (i in 1:b) newpop <- rbind(newpop, pop)
    
    ## mutation occurs among newborns, with probability q
    ## for each of them
    mutate <- runif(nrow(newpop)) < q
    
    ## 1st column of newpop stays the same if there is no
    ## mutation, but if there is, a new random number is
    ## inserted
    newpop[,1]  <- ifelse(mutate, runif(nrow(newpop)), newpop[,1])
    
    ## an individual disperses to a randomly determined
    ## location, if a random number falls below the
    ## dispersal gene value (indicated in the 1st
    ## column of newpop)
    move <- runif(nrow(newpop)) < newpop[,1]
    newlocation <- ceiling(N * runif(nrow(newpop))) ## see '?ceiling'
    
    ## 2nd column of newpop stays the same if there is no
    ## movement but if there is, the predefined new
    ## location is inserted
    newpop[,2] <- ifelse(move, newlocation, newpop[,2])
    
    ## mortality for the ones who moved
    death <- which(as.logical((runif(nrow(newpop)) < m) * move))
    if (length(death) != 0) newpop <- newpop[-death,]
    
    ## the new generation is ready
    pop <- newpop
    
    ## again, if the population went extinct, we may stop here
    if (nrow(pop) == 0) break
    
  }
  
  return(list(xmean=xmean, n=n, xstd=xstd, occupied=occupied))
  
}
