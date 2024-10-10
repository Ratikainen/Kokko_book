## this script is used to test the
## ported Matlab files in one single go

## all R script files

files <- c("box2_1a.R",
           "box2_1b.R",
           "box2_2.R",
           "sexconflict.R",
           "box2_3.R",
           "distr_shift.R",
           "barnacle.R",
           "box4_1.R",
           "maledisplay.R",
           "forage.R",
           "plantheight.R",
           "plantheight_advanced.R",
           "migrate.R",
           "pip.R",
           "dispersal.R",
           "disploop.R")

## parse all files and check whether there are any warnings
## (hit enter to see the next graph)

require(lattice)
require(grid)
require(ggplot2)
grid.prompt(TRUE)
lapply(files, source)
warnings()

## test run of each function
## (hit enter to see the next graph)

barnacle(c(2, 1.5, 1.45, 1), 0.1)
dispersal(10, 1, 0.01, 0.01, 2, 0.1)
grid.prompt(FALSE)
disploop(10, 1, 0.01, 0.01, 2)
grid.prompt(TRUE)
distr_shift(0.1, 15, 1, 15)
forage(0, 0.01, 0.4, 0.8, 5, 6)
maledisplay(c(0, 0.2))
migrate(c(0.5, 0.38), 2, 0.0002)
pip(1:4, 1:4, matrix(rep(c(NaN, -1:1), 4), nrow=4))
plantheight_advanced(c(501, 501), c(3, 3), 0.5, 1, 0.5)
plantheight(c(0, 1/3, 2/3, 1), 3, 0.25, 1)
sexconflict(5, 0.9, 200, 20, 101)

