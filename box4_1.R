T <- seq(0, 2, length.out=101)
b <- T
cL <- 2*T^2
cH <- T^2

(figure1 <- xyplot(b + cL + cH ~ T, xlab="Trait", ylab="Costs and benefits", type="l"))

WL <- b - cL
WH <- b - cH

(figure2 <- xyplot(WL + WH ~ T, xlab="Trait", ylab="Fitness", type="l"))

## this draws helpful lines to see where the maxima are
(figure2 <- update(figure2, type=c("l", "g")))
## and we can also zoom in to have a closer look
(figure2 <- update(figure2, xlim=c(0, 1), ylim=c(0, 0.5)))
