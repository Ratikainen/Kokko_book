## This computes a plot of best response curves for the extended plant game
## n should be a 1x2 vector describing how many candidate strategies 
##   are examined for plant A (n[1]) and plant B (n[2])
## alpha should be a 1x2 vector, describing its value 
##   for plant A (alpha[1]) and plant B (alpha[1])
## PL is the low limit of photosynthesis (poor light)
## PH is the high limit of photosynthesis (plenty of light)
## r is relatedness between the plants

plantheight_advanced <- function(n, alpha, PL, PH, r) {

  ## create vectors that describe options for A and B
  h1 <- seq(0, 1, length.out=n[1])
  h2 <- seq(0, 1, length.out=n[2])
  
  ## now create matrices H1 and H2 that contain all combinations of values required
  H1 <- matrix(rep(h1, length(h2)), ncol=length(h1), byrow=TRUE)
  H2 <- matrix(rep(h2, length(h1)), ncol=length(h1))
  deltaH <- H1 - H2
  
  ## now that everything is in matrix form, all subsequent calculations can be 
  ## performed at once (without 'for' loops)
  fA <- 1 - H1^alpha[1]
  fB <- 1 - H2^alpha[2]
  gA <- PL + (PH-PL) / (1+exp(-5*deltaH))
  gB <- PL + (PH-PL) / (1+exp(+5*deltaH)) ## question: why did the '-' become a '+' here?
  fitA <- fA * gA + r * fB * gB
  fitB <- fB * gB + r * fA * gA

  ## now find the best solutions
  bestindex <- apply(fitA, 1, which.max)
  bestA <- h1[bestindex]
  bestindex <- apply(fitB, 2, which.max)
  bestB <- h2[bestindex]
  
  print(xyplot(c(bestB, h2) ~ c(h1, bestA), type=c("l", "g"),
               groups=factor(c(rep("B", n[1]), rep("A", n[2]))),
               xlab="Height of plant A", ylab="Height of plant B"))
  
}

