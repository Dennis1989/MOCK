require(RANN)
appendBenchmarks <- function(data,mockSolution,benchmarks,maxCluster,L){
  nn = nn2(data,k=L)$nn.idx[,-1]
  data = as.matrix(data)
  if(length(which(benchmarks=="kmeans"))>0){
    #creating reference kmeans solutions
    km=t(sapply(1:maxCluster,function(x){
      return(mockFunctionC(data = data,geneOrClusterVector = kmeans(data,x)$cluster,lNN = nn,decoded = T))
    }))
    mockSolution$kmeans = matrix(unlist(km),ncol=2)
  }
  if(length(which(benchmarks=="single"))>0 || length(which(benchmarks=="average"))>0){
    #Obtain one cluster solution. Required because single and average linkage can't produce one cluster solutions
    oneClusterSolution = mockFunctionC(data = data,geneOrClusterVector = rep(1,nrow(data)),lNN = nn,decoded = T)
    
    dissimilarity = dist(data)
    if(length(which(benchmarks=="single"))>0){
      sl=rbind(oneClusterSolution,t(sapply(1:(maxCluster - 1),function(x){
        return(mockFunctionC(data = data,geneOrClusterVector = cutree(hclust(dissimilarity,"single"),x),lNN = nn,decoded = T))
      })))
      mockSolution$single = matrix(unlist(sl),ncol=2)
    }
    if(length(which(benchmarks=="average"))>0){
      al=rbind(oneClusterSolution,t(sapply(1:(maxCluster - 1),function(x){
        return(mockFunctionC(data = data,geneOrClusterVector = cutree(hclust(dissimilarity,"average"),x),lNN = nn,decoded = T))
      })))
      mockSolution$average = matrix(unlist(al),ncol=2)
    }  
  }
  return(mockSolution)
}