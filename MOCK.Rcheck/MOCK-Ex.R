pkgname <- "MOCK"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "MOCK-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('MOCK')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("binSearchC")
### * binSearchC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: binSearchC
### Title: Find an integer in an ordered vector using binary search.
### Aliases: binSearchC

### ** Examples

i = 42; v = 0:41
binSearchC(i,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("binSearchC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("decodeC")
### * decodeC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: decodeC
### Title: Decode the genetic representation of the edge assignment.
### Aliases: decodeC

### ** Examples

clusters = c(2,3,1,5,4)
decodeC(clusters)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("decodeC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("deviationC")
### * deviationC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: deviationC
### Title: Calculate the deviation within a matrix. This is defined as the
###   sum of deviations of each point from the center vector of the matrix.
### Aliases: deviationC

### ** Examples

v = matrix(1:25,ncol=5)
deviationC(v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("deviationC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("euclideanDistanceC")
### * euclideanDistanceC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: euclideanDistanceC
### Title: Rcpp implementation of euclidean distance between two vectors.
### Aliases: euclideanDistanceC

### ** Examples

x = c(1, 3, 5); y = c(4, 3, 1)
euclideanDistanceC(x, y)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("euclideanDistanceC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getNeighborhoodMutationMatrixC")
### * getNeighborhoodMutationMatrixC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getNeighborhoodMutationMatrixC
### Title: Zero-based Rcpp implementation of 'which' for integer vectors.
### Aliases: getNeighborhoodMutationMatrixC

### ** Examples

x = 42; v = 40:44
whichC(x,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getNeighborhoodMutationMatrixC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("meanVectorOfMatrixC")
### * meanVectorOfMatrixC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: meanVectorOfMatrixC
### Title: Calculate the mean vector for a matrix by computing the mean for
###   each column. This is an alternative to apply(m,2,mean).
### Aliases: meanVectorOfMatrixC

### ** Examples

m = matrix(1:25,ncol=5)
meanVectorOfMatrixC(m)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("meanVectorOfMatrixC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mockFunctionC")
### * mockFunctionC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mockFunctionC
### Title: Objective MOCK function calculating the deviation and
###   connectivity of a given genetic representation of a solution.
### Aliases: mockFunctionC

### ** Examples

data = matrix(c(1:10,91:100),ncol=2); genes = c(2,3,4,5,1,7,8,9,10,6); decoded = FALSE
lNN = matrix(c(2,1,2,3,4,7,8,9,10,9,3,3,4,5,6,5,6,7,8,8),ncol=2) #nearest neighbors of data, see \code{nn2} in RANN package
mockFunctionC(data,genes,lNN,decoded)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mockFunctionC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rcpp_hello_world")
### * rcpp_hello_world

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rcpp_hello_world
### Title: Simple function using Rcpp
### Aliases: rcpp_hello_world

### ** Examples

## Not run: 
##D rcpp_hello_world()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rcpp_hello_world", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("selectRowsOfDatasetC")
### * selectRowsOfDatasetC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: selectRowsOfDatasetC
### Title: Select rows of a matrix based on a (one-based) index-vector.
### Aliases: selectRowsOfDatasetC

### ** Examples

data = matrix(1:25,ncol=5); v = c(1,4)
selectRowsOfDatasetC(data,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("selectRowsOfDatasetC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("selectRowsOfDatasetCInteger")
### * selectRowsOfDatasetCInteger

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: selectRowsOfDatasetCInteger
### Title: Select rows of a matrix based on a vector.
### Aliases: selectRowsOfDatasetCInteger

### ** Examples

data = matrix(1:25,ncol=5); v = c(1,4)
selectRowsOfDatasetC(data,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("selectRowsOfDatasetCInteger", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("whichC")
### * whichC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: whichC
### Title: Zero-based Rcpp implementation of 'which' for integer vectors.
### Aliases: whichC

### ** Examples

x = 42; v = 40:44
whichC(x,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("whichC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("whichCList")
### * whichCList

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: whichCList
### Title: Zero-based Rcpp implementation of 'which' for integer vectors.
### Aliases: whichCList

### ** Examples

x = 42; v = 40:44
whichC(x,v)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("whichCList", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
