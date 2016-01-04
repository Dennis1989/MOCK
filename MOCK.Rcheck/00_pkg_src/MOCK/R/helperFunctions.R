#' @export
evalKmeans = function(data,ksol,nn){
  dev=0
  conn=0
  for(i in 1:(max(ksol$cluster)))
  {
    curdata = data[which(ksol$cluster==i),]
    dev=dev  + deviationC(as.matrix(curdata))
    conn=conn+ connectivity(which(ksol$cluster==i),nn)
  }
  return(c(dev=dev,occ=conn))
}

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

#' @export
filterAndNormalizeFront <- function(front,minimumdev,maximumdev,minimumconn,maximumconn){
  front = front[front[,1] >= minimumdev,]
  front = front[front[,2] >= minimumconn,]
  front[,1]=sqrt((front[,1] - minimumdev) /(maximumdev - minimumdev))
  front[,2]=sqrt((front[,2] - minimumconn) /(maximumconn - minimumconn))
  return(front)
}

#' @export
assignmentToClusterList <- function(clusterAssignment){
  clusterList = lapply(1:max(clusterAssignment), function(x){
    return(whichC(x,clusterAssignment))
  })
  return(clusterList)
}
