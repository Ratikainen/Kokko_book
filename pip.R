## Draws a PIP. Takes the range of x and y values
## as vectors, and invfitness as a matrix. The fourth
## argument, which is optional, creates a greyscale
## plot which is easier to interpret for colourblind
## people.

pip <- function(x, y, invfitness, colourblind=FALSE) {

  z <- matrix(nrow=nrow(invfitness), ncol=ncol(invfitness))
  
  ## colours are 1, 2, 3 for negative, zero, positive sign
  ## of invasion fitness
  z <- sign(invfitness) + 2

  ## if fitness is not a proper number, mark it as colour 4
  ## which is grey
  z[is.nan(invfitness)] <- 4
  
  ## the following colour definition uses red-green-blue
  ## values to mark negative, zero, positive, and NaN
  ## (not-a-number) as red, white, green, and grey,
  ## respectively
  colour <- c("red", "white", "green", "grey")

  ## if the argument colourblind is true
  ## then we'll redefine colours
  if (colourblind) {
    colour <- c("grey80", "white", "black", "grey50")
  }

  sx <- length(x)
  sy <- length(y)
  ## just in case the matrix is arranged the wrong way, swap it
  if (sx != nrow(z)) z <- t(z)

  ## draw the figure
  figure <- levelplot(z, col.regions=colour, at=0:4, aspect="fill", colorkey=FALSE,
                      xlab="Resident strategy", ylab="Mutant strategy")

  return(figure)

}
