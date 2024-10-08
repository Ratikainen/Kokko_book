x <- seq(0, 1, length.out=101)
m1 <- 1.5
m2 <- 5
m3 <- 10
y1 <- m1 * x / (m1*x + 1 - x)
y2 <- m2 * x / (m2*x + 1 - x)
y3 <- m3 * x / (m3*x + 1 - x)
xyplot(y1 + y2 + y3 ~ x, type="l",
       xlab="Proportion of A males in the population",
       ylab="Proportion of A males among mating males")
