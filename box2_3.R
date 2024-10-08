## This script uses function sexconflict() in file sexconflict.R
## (make it available with 'source("sexconflict.R")').

N_case1 <- 1
xnew_case1 <- sexconflict(5, 0.5, N_case1, 20, 101)
N_case2 <- 100000
xnew_case2 <- sexconflict(5, 0.5, N_case2, 20, 101)

max(abs(xnew_case1$xnew - xnew_case2$xnew))
