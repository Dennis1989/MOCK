solutionToMatrix=function(x){
  #get the current solution as matrix
  matrix=t(sapply(x$sol,function(sol){
    return (sol$solution)
  }))
  
  #append amount of cluster 
  cluster = sapply(x$sol,function(sol){sol$clusters})
  
  #append score of cluster 
  score = sapply(x$sol,function(sol){sol$score})
  
  #cbind matrix and cluster
  matrix = cbind(matrix,cluster,score)
  
  return (matrix)
  
}