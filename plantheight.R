## This computes a plot of best response curves for the two plants
## hvalues should give all height allocation options considered
## alpha is the exponent in f(h)=1-h^alpha
## PL is the low limit of photosynthesis (poor light)
## PH is the high limit of photosynthesis (plenty of light)
## Examples of use: plantheight(c(0, 1/3, 2/3, 1), 2, .25, 1)
## or plantheight(seq(0 1, 0.01), 2, .25, 1)

plantheight <- function(hvalues, alpha, PL, PH) {

  fitA <- fitB <- matrix(ncol=length(hvalues), nrow=length(hvalues))
  
  for (i in 1:length(hvalues)) {
    for (j in 1:length(hvalues)) {
      
      ## Inside these loops, we investigate the option in
      ## which plant A 'tries out' strategy number i, i.e. hvalues[i]
      ## and plant B 'tries out' strategy number j, i.e. hvalues[j]
        
      fA <- 1 - hvalues[i]^alpha
      fB <- 1 - hvalues[j]^alpha
      gA <- PL + (PH-PL) / (1 + exp(-5*(hvalues[i]-hvalues[j])))
      gB <- PL + (PH-PL) / (1 + exp(-5*(hvalues[j]-hvalues[i])))
      
      fitA[i,j] <- fA * gA
      fitB[i,j] <- fB * gB
      
    }
  }
  
  ## if hvalues has very many values, we assume that one
  ## does not want to see the values rolling endlessly on
  ## the screen. Therefore we only ask to see them if hvalues
  ## is not a too long vector
  if (length(hvalues) <= 15) {
    print(fitA)
    print(fitB)
  }
  
  ## Now find the best solutions using 'which.max', and plot the result.
  ## Note the counterintuitive way to plot hvalues against A, 
  ## and B against hvalues. This is because the definition of 
  ## the 'x axis' differs in these plots between plant A and 
  ## plant B, so for plant B the hvalues will go to what 'xyplot'
  ## knows as the y axis
  bestindex <- apply(fitA, 2, which.max)
  bestA <- hvalues[bestindex]
  bestindex <- apply(fitB, 1, which.max)
  bestB <- hvalues[bestindex]
  print(xyplot(c(bestB, hvalues) ~ c(hvalues, bestA), type="l",
               groups=factor(rep(c("B", "A"), each=length(hvalues))),
               xlab="Height of plant A", ylab="Height of plant B"))
  
}
