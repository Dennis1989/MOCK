# This file contains the imports required for mock-execution
# ToDo: Include this in the namespace file
# It also includes  code in order to benchmark mock
# ...commented out as long as it takes for us to figure out how to include it in the package
require(microbenchmark)
require(RANN)
require(ggplot2)
# require(PESAII) #For the moment this is loaded using Build&Reload
# require(MOCK)
require(profvis) #devtools::install_github("rstudio/profvis") , requires digest package
require(mlbench)
require(gridExtra)
require(vegan)
require(ggthemes)
require(nsga2R)

# a1 = read.csv("..\\MOCK\\data\\a1.txt")
spiral = read.csv("..\\MOCK\\data\\spiral.txt",header = F, sep = "\t")[,1:2]
#flame = read.csv("..\\MOCK\\data\\flame.txt",header = F, sep = "\t")[,1:2]
# r15 = read.csv("..\\MOCK\\data\\R15.txt",sep="\t")[,1:2]
#cassini = mlbench.cassini(1000)$x

long1 = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = long1
 
# plot(clusterData)
mockSol = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 100)
length(mockSol$sol) #amount of solutions
max(sapply(mockSol$sol,function(x){return(x$clusters)})) #maxium amount of clusters

printParetoFront(mockSol)
printCurrentSolution(mockSol)
 
printClusterSolution(mockSol, c(1:9), clusterData)

profvis({mockSol = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000)}, 0.005)
# microbenchmark(mockSol = mock(data = clusterData,L = 10,crossRate = 0.1,nGrid = 10,gens = 1000),times=1)