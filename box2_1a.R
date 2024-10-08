## 'seq' can be used to make a vector with
## linearly equally spaced values
x <- seq(0, 1, length.out=101)

## now we choose a value of m to be investigated
m <- 5

## calculate how y depends on x
y <- m*x / (m*x + 1 - x)

xyplot(y ~ x, type="l",
       xlab="Proportion of A males in the population",
       ylab="Proportion of A males among mating males")
