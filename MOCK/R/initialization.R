#This file contains functions for the PESA-II and MOCK initialization
#' @export
#' @importFrom vegan spantree
initializeMST <- function(adjacencyMatrix){
  adjacencyMatrix = as.matrix(adjacencyMatrix)
  
  # adjacencyMatrix[upper.tri(adjacencyMatrix)] <- NA
  
  # mst = minimum.spanning.tree(graph_from_adjacency_matrix(adjacencyMatrix,weighted = T),algorithm = "prim")
  # edges = matrix(as.numeric(get.edgelist(mst)),ncol=2)
  mst = spantree(adjacencyMatrix)
  edges = cbind(2:(length(mst$kid)+1),mst$kid)
  setdiff(1:nrow(edges), edges[,1])
  mstRoot = 1
  edges = rbind(c(mstRoot,mstRoot), edges)
  edges = edges[order(edges[,1]),]
  
  return(edges)
}


pruneMST <- function(edges, edgeDistances, x){
  edges[edgeDistances[1:x,1],2] = edgeDistances[1:x,1]
  
  return(edges[,2])
}

#Prunes MST according to interestingness. 
pruneMSTInterestingness <- function(edges, x, removedLinks, interestingness,nn,L){
  if(x > 1){
    for(i in 1:(x-1))
    {
      edges[interestingness[i,1],2] = removedLinks[i]
    }  
  }
  
  sampled = sample(nn[interestingness[x,1],1:L],1)
  edges[interestingness[x,1],2]= sampled
  removedLinks = c(removedLinks, sampled)
  return(list(solution = edges[,2],removedLinks = removedLinks))
}

# Calculates interestingness
# General idea: A link between A and B can be considered interesting, iff they are not 
# within each other L nearest neighbors.
calculateInterestingness <- function(mst,nn,L){
 res= apply(mst,MARGIN = 1,FUN = function(x){
    nni=0
    nnj=0
    ## get index for l'th nearest neigbour of the first entry
    for(i in 1:length(nn[x[1],]))
    {
      if(x[2]==nn[x[1],i]){
         nni=i
         break
      }
    }
    ##get lth
    for(j in 1:length(nn[x[2],]))
    {
      if(x[1]==nn[x[2],j]){
        nnj=j
        break
      }
        
    }
    
    if((nni >L) && (nnj >L))
      return (min(nni,nnj))
    else 
      return (0)
    
  })
 
 erg=cbind(mst,interestingness=res)
 erg = erg[order(erg[,3],decreasing=TRUE),]
 return(erg)
}


# Generation of the k-means solutions
generateKMeansSolution = function(edges,nn,k,L,data){
  #generate kmeans solution
  km=kmeans(data,k)
  
  for(i in 1:nrow(edges))
  {
    #if two connected data points in mst do not fall into the same km-cluster
    if(km$cluster[edges[i,1]]!= km$cluster[edges[i,2]])
    {
      #delete the edge and replace by a link from i to a random chosen neighbor nnil where l<L
      edges[i,2]=sample(nn[edges[i,1],1:L],1)
    }
  }
  return (edges[,2])
  
}

calculateInterestingnessC <- function(mst,nn,L){
  res=interestingnessC(mst$edges,nn,L)
  erg=cbind(mst$edges,interestingness=res)
  erg = erg[order(erg[,3],decreasing=TRUE),]
  return(erg)
}

#' Calculates a neighborhood-matrix (square, but not symmetric) that contains likelihood for an edge (from row to column) to be mutated in PESA
#'  @param nearestNeighbors Matrix that contains the nearest neighbors of each point ordered by neighborhood index
getNeighborhoodMutationMatrix <- function(nearestNeighbors){
  neighborhoodMutationMatrix = matrix(nrow = nrow(nearestNeighbors), ncol = ncol(nearestNeighbors) + 1)
  for (row in 1:nrow(nearestNeighbors)){
    for (column in 1:ncol(nearestNeighbors)){
      neighborhoodMutationMatrix[row,nearestNeighbors[row,column]] = (1 / nrow(nearestNeighbors)) + (column / nrow(nearestNeighbors))^2 #Formula for neighborhood-Biased Mutation
    }
  }
  diag(neighborhoodMutationMatrix) = 0
  return(neighborhoodMutationMatrix)
}


addSolutionDistinct <- function(initialPopulation,solution,param){
  addSolution = T
  if(length(initialPopulation) > 0){
    for(j in 1:length(initialPopulation)){
      if(isTRUE(all.equal(initialPopulation[[j]]$solution,solution))){
        addSolution = F
        break
      }
    }
  }
  if(addSolution == T){
    initialPopulation[[length(initialPopulation)+1]] = list(param=param,solution=solution)
  }
  return(initialPopulation)
}