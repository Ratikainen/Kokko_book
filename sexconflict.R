## Allele frequency change in one generation.
## Give m, b, N, and B according to the definitions in
## Chapter 2 and accuracy as an integer number that
## tells how many values between 0 and 1 the variable x
## should take.
## The function computes xnew and x.

sexconflict <- function(m, b, N, B, accuracy) {
  
  x <- seq(0, 1, length.out=accuracy) ## creates many values between 0 and 1

  ## first calculate how y depends on x.
  ## Note there is no need to define a value for m now - it
  ## will have been supplied as an argument when using
  ## the function 'sexconflict'.
  y <- m * x / (m*x + 1 -x)

  ## now calculate mating proportions
  pAA <- x * y
  pAa <- x * (1-y)
  paA <- (1-x) * y
  paa <- (1-x) * (1-y)

  ## now calculate nA and na
  nA <- N * (pAA*b*B + 0.5*pAa*B + 0.5*paA*b*B)
  na <- N * (0.5*pAa*B + 0.5*paA*b*B + paa*B)

  ## then we get xnew and can plot it against x
  xnew <- nA / (nA+na)

  ## then create the plots: first xnew against x, and then x against x
  ## - the latter will produce a diagonal
  print(xyplot(xnew + x ~ x, type="l",
               xlab="Frequency of A in generation 0",
               ylab="Frequency of A males among mating males"))

  return(list(xnew=xnew, x=x))

}
