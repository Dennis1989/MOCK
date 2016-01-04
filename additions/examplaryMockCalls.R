#install.packages("stringi"); devtools::install_github("rstudio/profvis")
require(profvis)

#This file contains some examplary calls of mock as well as some performance benchmarks
#Load different datasets from mock package
clusterData = read.csv("..\\MOCK\\data\\a1.txt")
clusterData = read.csv("..\\MOCK\\data\\spiral.txt",header = F, sep = "\t")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\flame.txt",header = F, sep = "\t")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\R15.txt",sep="\t")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\square1.data",header = F, sep = " ")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\bridge.txt",header = F, sep = "")[,1:2]
clusterData = read.csv("..\\MOCK\\data\\spiral.data",header = F, sep = " ")[,1:2]
clusterData = iris[,-5]

#Call mock with different EMOAs and parameters
set.seed(42)
mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=1000,epMax = 1000,method="pesa",printN=10,controlFronts=1)
tempSol=mockSol
mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=100,method="nsga2",printN=NULL,tournamentN=2,controlFronts = 1,maxCluster = 25)
tempSol2=mockSol
mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=10000,method="smsemoa",printN=NULL,tournamentN=2,controlFronts = 1)
tempSol3=mockSol
length(mockSol$sol) #amount of solutions
max(sapply(mockSol$sol,function(x){return(x$clusters)})) #maxium amount of clusters

#Print pareto front and plot some characteristics of solution
printParetoFront(pesa,"benchmark",maxClusters = 25,labelType = "none")
printParetoFront(mockSol,"fronts",labelType = "index",maxClusters = 100)
plot(t(sapply(controlFronts[[j]]$sol,function(x){return(x$solution)}))[,2],t(sapply(controlFronts[[j]]$sol,function(x){return(x$solution)}))[,1])
plot(sapply(sol$controlFronts[[1]]$sol,function(x){return(x$clusters)}))


#Performance Benchmarks
profvis({mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=100,method="nsga2",controlFronts = 1)},0.005)
profvis({mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=1000,method="pesa",printN=10,controlFronts=3)},0.005)
profvis({mockSol=mock(data=clusterData,L=10,crossRate=0.7,nGrid=10,gens=10000,method="smsemoa",printN=5,tournamentN=2)},0.005)
