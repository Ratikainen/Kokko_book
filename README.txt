These files are R ports of the Matlab files accompanying the book:

Kokko, H. 2007. Modelling for Field Biologists (and Other Interesting People)
Cambridge University Press


They are available from:

http://evolution.unibas.ch/people/zumbrunn/Modelling-Kokko-R.zip


The author's original Matlab files are available from:

http://www.helsinki.fi/~hmkokko/modelling/index.html


More information on R can be obtained at:

http://www.r-project.org/


The R ports of the Matlab files were fabricated by Thomas Zumbrunn - please
don't bug the author but rather address your questions about these files to:

t.zumbrunn@unibas.ch


The R scripts/functions make use of the lattice graphics package instead of
the traditional graphics system. So before you produce any plots, you should
load the lattice package:

> require(lattice)

Some scripts/functions depend on each other. To make them available to R,
either copy the source code to the R terminal or load the corresponding file
with 'source':

> source("sexconflict.R")
