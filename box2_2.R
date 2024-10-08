## make a vector of 101 different values of x, ranging from 0 to 1
x <- seq(0, 1, length.out=101)

## choose a value of m to be investigated
m <- 5

## calculate how y depends on x
y <- m*x / (m*x + 1 - x)

## calculate mating proportions
pAA <- x * y
pAa <- x * (1-y)
paA <- (1-x) * y
paa <- (1-x) * (1-y)

## let's check that these sum up to 1!
pAA + pAa + paA + paa

## now to nA and na. We must also choose values for N, B, and b
N <- 1000
B <- 20
b <- 0.8
nA <- N * (pAA*b*B + 0.5*pAa*B + 0.5*paA*b*B)
na <- N * (0.5*pAa*B + 0.5*paA*b*B + paa*B)

## then we get xnew, and can plot it against x
xnew <- nA / (nA+na)

## then plot things: first xnew against x, and then x against x
## - the latter will produce a diagonal
xyplot(xnew + x ~ x, type="l",
       xlab="Frequency of A in generation 0",
       ylab="Frequency of A males among mating males")
