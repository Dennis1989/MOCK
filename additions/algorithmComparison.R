#comparing smsemoa, nsgaII and PESAII
#long data
set.seed(123)
long1 = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = long1

pesa = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 100,ipMax = 10,maxCluster=50)
smsemoa = mock(data = clusterData,L = 10,crossRate = 0.7,gens = 10000,method="smsemoa",controlFronts = 1,maxCluster=50)
nsga2 = mock(data = clusterData,L = 10,crossRate = 0.7,gens = 100,method="nsga2",controlFronts = 1,maxCluster=50)

square1 = read.csv("..\\MOCK\\data\\square1.data",header = F, sep = " ")[,1:2]
clusterData = square1

pesa_1 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 100,ipMax = 10,maxCluster=50)
smsemoa1_1 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 10000,method="smsemoa",controlFronts = 1,maxCluster=50)
nsga2_1 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 100,method="nsga2",controlFronts = 1,maxCluster=50)


spiral = read.csv("..\\MOCK\\data\\spiral.data",header = F, sep = " ")[,1:2]
clusterData = spiral
pesa_2 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 100,ipMax = 10,maxCluster=50)
smsemoa1_2 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 10000,method="smsemoa",controlFronts = 1,maxCluster=50)
nsga2_2 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 100,method="nsga2",controlFronts = 1,maxCluster=50)





#----Visualization----#

#generate graphs
graphLong   = printMultipleParetoFronts(list(pesa,smsemoa,nsga2),dataset="Long1")
graphLong_not   = printMultipleParetoFronts(list(pesa,smsemoa,nsga2),"F",dataset="Long1")

graphSquare = printMultipleParetoFronts(list(pesa_1,smsemoa1_1,nsga2_1),dataset="Square1")
graphSquare_not = printMultipleParetoFronts(list(pesa_1,smsemoa1_1,nsga2_1),"F",dataset="Square1")

graphSpiral = printMultipleParetoFronts(list(pesa_2,smsemoa1_2,nsga2_2),dataset="Spiral")
graphSpiral_not = printMultipleParetoFronts(list(pesa_2,smsemoa1_2,nsga2_2),"F",dataset="Spiral")

#merge all three plots within one grid (and visualize this)

pdf("Comparison2.pdf",width = 8, height = 8,pointsize = 2) 
grid.arrange(graphLong, graphLong_not, graphSquare, graphSquare_not,graphSpiral, graphSpiral_not, nrow=3) #arranges plots within grid
dev.off()

#show best solutions for MOCK and long1 dataset

p1=printClusterSolution(pesa, pesa$best, long1,1)
p2=printClusterSolution(nsga2, nsga2$best[1], long1,1)
p3=printClusterSolution(smsemoa, smsemoa$best, long1,1)
pdf("bestLong.pdf",pointsize = 0) 
grid.arrange(p1,p2,p3,layout_matrix=rbind(c(1,2),c(3,3)))
dev.off()
#show best solutions for MOCK and square dataset

p1=printClusterSolution(pesa_1, pesa_1$best, square1,1)
p2=printClusterSolution(nsga2_1, nsga2_1$best, square1,1)
p3=printClusterSolution(smsemoa1_1, smsemoa1_1$best, square1,1)
pdf("bestsquare.pdf",pointsize = 0) 
grid.arrange(p1,p2,p3,layout_matrix=rbind(c(1,2),c(3,3)))
dev.off()
#show best solutions for MOCK and spiral dataset

p1=printClusterSolution(pesa_2, pesa_1$best, spiral,1)
p2=printClusterSolution(nsga2_2, nsga2_1$best, spiral,1)
p3=printClusterSolution(smsemoa1_2, smsemoa1_2$best, spiral,1)
pdf("bestsprial.pdf",pointsize = 0) 
grid.arrange(p1,p2,p3,layout_matrix=rbind(c(1,2),c(3,3)))
dev.off()
#save
g <- arrangeGrob(graphLong, graphLong_not, graphSquare, graphSquare_not,graphSpiral, graphSpiral_not, nrow=3) #generates g


ggsave(file="whatever.pdf", g) #saves g
grid.arrange()


#-----ANALYSIS------#
#Analyse long dataset

#add method to matrix
comppesa=as.data.frame(solutionToMatrix(pesa))
comppesa=cbind(comppesa,method=rep(pesa$method,nrow(comppesa)))

#add method to matrix
compsms=as.data.frame(solutionToMatrix(smsemoa))
compsms=cbind(compsms,method=rep(smsemoa$method,nrow(compsms)))

#add method to matrix
compnsga2 =as.data.frame(solutionToMatrix(nsga2))
compnsga2=cbind(compnsga2,method=rep(nsga2$method,nrow(compnsga2)))

#add all algo solutions to global matrix
result = rbind(comppesa,compsms,compnsga2)

#transorm to matrix
res = as.matrix(sapply(result[,c("V1","V2")], as.numeric))  

#only get nondominated indices
nonDominated = PESAII:::nonDominatedSolutionsIndex(res[,1:2])
result=cbind(result,dominated=nonDominated)

#filter out dominated solutions
filterNondominated = result[result[,"dominated"],]


#get solutionb amount of global paretro front
pesashare=nrow(filterNondominated[filterNondominated[,"method"]=="PESA-II",])/nrow(filterNondominated)
nsga2share=nrow(filterNondominated[filterNondominated[,"method"]=="NSGA-II",])/nrow(filterNondominated)
smsemoashare=nrow(filterNondominated[filterNondominated[,"method"]=="SMS-EMOA",])/nrow(filterNondominated)


#--------------------------------------------------------------------------------------------------
#Analyse square dataset

#add method to matrix
comppesa=as.data.frame(solutionToMatrix(pesa_1))
comppesa=cbind(comppesa,method=rep(pesa$method,nrow(comppesa)))

#add method to matrix
compsms=as.data.frame(solutionToMatrix(smsemoa1_1))
compsms=cbind(compsms,method=rep(smsemoa$method,nrow(compsms)))

#add method to matrix
compnsga2 =as.data.frame(solutionToMatrix(nsga2_1))
compnsga2=cbind(compnsga2,method=rep(nsga2$method,nrow(compnsga2)))

#add all algo solutions to global matrix
result = rbind(comppesa,compsms,compnsga2)

#transorm to matrix
res = as.matrix(sapply(result[,c("V1","V2")], as.numeric))  

#only get nondominated indices
nonDominated = PESAII:::nonDominatedSolutionsIndex(res[,1:2])
result=cbind(result,dominated=nonDominated)

#filter out dominated solutions
filterNondominated = result[result[,"dominated"],]


#get solutionb amount of global paretro front
pesashare_1=nrow(filterNondominated[filterNondominated[,"method"]=="PESA-II",])/nrow(filterNondominated)
nsga2share_1=nrow(filterNondominated[filterNondominated[,"method"]=="NSGA-II",])/nrow(filterNondominated)
smsemoashare_1=nrow(filterNondominated[filterNondominated[,"method"]=="SMS-EMOA",])/nrow(filterNondominated)


#--------------------------------------------------------------------------------------------------
#Analyse spiral dataset

#add method to matrix
comppesa=as.data.frame(solutionToMatrix(pesa_2))
comppesa=cbind(comppesa,method=rep(pesa$method,nrow(comppesa)))

#add method to matrix
compsms=as.data.frame(solutionToMatrix(smsemoa1_2))
compsms=cbind(compsms,method=rep(smsemoa$method,nrow(compsms)))

#add method to matrix
compnsga2 =as.data.frame(solutionToMatrix(nsga2_2))
compnsga2=cbind(compnsga2,method=rep(nsga2$method,nrow(compnsga2)))

#add all algo solutions to global matrix
result = rbind(comppesa,compsms,compnsga2)

#transorm to matrix
res = as.matrix(sapply(result[,c("V1","V2")], as.numeric))  

#only get nondominated indices
nonDominated = PESAII:::nonDominatedSolutionsIndex(res[,1:2])
result=cbind(result,dominated=nonDominated)

#filter out dominated solutions
filterNondominated = result[result[,"dominated"],]


#get solutionb amount of global paretro front
pesashare_2=nrow(filterNondominated[filterNondominated[,"method"]=="PESA-II",])/nrow(filterNondominated)
nsga2share_2=nrow(filterNondominated[filterNondominated[,"method"]=="NSGA-II",])/nrow(filterNondominated)
smsemoashare_2=nrow(filterNondominated[filterNondominated[,"method"]=="SMS-EMOA",])/nrow(filterNondominated)





#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Compare MOCK with kmeans, single, and average linkage

#Conduct pesa on long1 and square1
set.seed(123)

long1 = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = long1
pesa = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 100,ipMax = 10,maxCluster=50)

square1 = read.csv("..\\MOCK\\data\\square1.data",header = F, sep = " ")[,1:2]
clusterData = square1
pesa_1 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 100,ipMax = 10,maxCluster=50)


#append benchmarks to pesa and pesa_1
set.seed(123)
long1 = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = long1
pesa = appendBenchmarks(clusterData,pesa,c("kmeans","single","average"),25,10)

square1 = read.csv("..\\MOCK\\data\\square1.data",header = F, sep = " ")[,1:2]
clusterData = square1
pesa_1 = appendBenchmarks(clusterData,pesa_1,c("kmeans","single","average"),25,10)

#print pareto fronts
long1benchmark = printParetoFront(pesa,"benchmark",maxClusters = 25,labelType = "none",dataset = "Long1",markBestsolutions = F)
square1benchmark = printParetoFront(pesa_1,"benchmark",maxClusters = 25,labelType = "none",dataset = "Square1",markBestsolutions = F)

#create pdf
pdf("Long1Square1Benchmark.pdf",width = 10, height = 4,pointsize = 2) 
grid.arrange(long1benchmark, square1benchmark,ncol=2)
dev.off()

#--------------------------------------------------------------------------------------------------
#Compare MOCK with solutions from original MOCK tool
set.seed(123)

long1 = read.csv("..\\MOCK\\data\\long1.data",header = F, sep = " ")[,1:2]
clusterData = long1
pesa = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 1000,ipMax = 10,maxCluster=50)
pesaMT = convertMockSolutionFileToMatrix(clusterData = clusterData,L=10,path="C:\\MOCK\\",solutionIndex=1)

square1 = read.csv("..\\MOCK\\data\\square1.data",header = F, sep = " ")[,1:2]
clusterData = square1
pesa_1 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 1000,ipMax = 10,maxCluster=50)
pesaMT_1 = convertMockSolutionFileToMatrix(clusterData = clusterData,L=10,path="C:\\MOCK\\",solutionIndex=2)

spiral = read.csv("..\\MOCK\\data\\spiral.data",header = F, sep = " ")[,1:2]
clusterData = spiral
pesa_2 = mock(data = clusterData,L = 10,crossRate = 0.7,nGrid = 10,gens = 1000,method="pesa",controlFronts = 1,epMax = 1000,ipMax = 10,maxCluster=50)
pesaMT_2 = convertMockSolutionFileToMatrix(clusterData = clusterData,L=10,path="C:\\MOCK\\",solutionIndex=3)



#----Visualization----#

graphLong   = printMockRAndMocktoolFronts(pesa,pesaMT,dataset="Long1")
graphLong_not   = printMockRAndMocktoolFronts(pesa,pesaMT,"F",dataset="Long1")

graphSquare = printMockRAndMocktoolFronts(pesa_1,pesaMT_1,dataset="Square1")
graphSquare_not = printMockRAndMocktoolFronts(pesa_1,pesaMT_1,"F",dataset="Square1")

graphSpiral = printMockRAndMocktoolFronts(pesa_2,pesaMT_2,dataset="Spiral")
graphSpiral_not = printMockRAndMocktoolFronts(pesa_2,pesaMT_2,"F",dataset="Spiral")

#merge all three plots within one grid (and visualize this)

pdf("Comparison3.pdf",width = 8, height = 8,pointsize = 2) 
grid.arrange(graphLong, graphLong_not, graphSquare, graphSquare_not,graphSpiral, graphSpiral_not, nrow=3) #arranges plots within grid
dev.off()
