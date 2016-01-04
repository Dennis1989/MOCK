#'@export
calculateScores <- function(solutionFront,controlFronts){
  kSF = max(sapply(solutionFront$sol,function(x){return(x$clusters)}))
  kCF = Inf
  for (i in 1:length(controlFronts)){
    k = max(sapply(controlFronts[[i]]$sol,function(x){return(x$clusters)}))
    kCF = min(kCF,k)
  }
  kmax = min(kSF,kCF)
  solutionFront$sol = filter(solutionFront$sol,kmax)
  for(i in 1:length(controlFronts)){
    controlFronts[[i]]$sol = filter(controlFronts[[i]]$sol,kmax)
  }
  solutionFront$sol = normalizeParetoFront(solutionFront$sol)
  for(i in 1:length(controlFronts)){
    controlFronts[[i]]$sol = normalizeParetoFront(controlFronts[[i]]$sol)
  }
  #Initialize vector containing scores
  scores = vector(mode = "numeric",length = length(solutionFront$sol))
  #Iterate over solution fronts points
  for(i in 1:length(solutionFront$sol)){
    scores[i] = Inf
    #Iterate over control fronts
    for(j in 1:length(controlFronts)){
      #Get current control front
      currentCF = controlFronts[[j]]
      #Set score to current distance, if previous distances are lower
      scores[i] = min(scores[i],distance(solutionFront$sol[[i]],controlFronts[[j]]$sol))
    }
  }
  return(scores)
}

#Filter solutions from front with amount of clusters > kMax
filter <- function(solutionFront,kMax){
  filtered = list()
  #Get indices of solutions with amount of clusters > kMax
  keep = which(sapply(solutionFront,function(x){return(x$clusters)}) <= kMax )
  for(i in 1:length(keep)){
    #Remove solutions
    filtered[[i]] = solutionFront[[i]]
  }
  return(filtered)
}

#Normalize a whole front
#'@export
normalizeParetoFront <- function(front,...){
  maxDev = max(sapply(front,function(x){return(x$solution[1])}))
  maxConn = max(sapply(front,function(x){return(x$solution[2])}))
  minDev = min(sapply(front,function(x){return(x$solution[1])}))
  minConn = min(sapply(front,function(x){return(x$solution[2])}))
  for(i in 1:length(front)){
    front[[i]]$solution[1] = sqrt((front[[i]]$solution[1] - minDev) / (maxDev-minDev))
    front[[i]]$solution[2] = sqrt((front[[i]]$solution[2] - minConn) / (maxConn-minConn))
  }
  return(front)
}

#find shortest distance between solution point and control front
distance <- function(solution,controlFront){
  shortestDistance = Inf
  for(i in 1:length(controlFront)){
    shortestDistance = min(shortestDistance,euclideanDistanceC(as.vector(solution$solution),
                                                               as.vector(controlFront[[i]]$solution)))
  }
  return(shortestDistance)
}

#'@export
createControlFronts <- function(data=NULL,points=1000,n=3,dim=2,controlFrontMethod="eigen",
                                extremes=NULL,eigenVectors=NULL,...){
  controlData = NULL
  if(controlFrontMethod=="eigen"){
    if(is.null(eigenVectors)){
      eigenV = eigen(cov(data))
      eigenVectors = sapply(1:ncol(eigenV$vectors),function(x){
        return(eigenV$vectors[,x]*eigenV$values[x])
      })
    }
    #Generate control data in eigenspace
    #Each point is a linear combination of a uniform variable (between 0 an 1) times each eigenvector
    controlData = t(sapply(1:points,function(x){
      pointAsMatrix = apply(eigenVectors,2,function(y){
        return(runif(1)*y)
      })
      return(apply(pointAsMatrix,1,sum))
    }))
    #Backtransform control data to original data space
    #Get span for each dimension of control data
    controlDifferences = apply(controlData,2,function(x){
      return(max(x)-min(x))
    })
    #Get span for each dimension of data
    dataDifferences = apply(data,2,function(x){
      return(max(x)-min(x))
    })
    #Scale by dividing control data by control differences times data differences
    controlData = sapply(1:ncol(controlData),function(x){
      return((controlData[,x] / controlDifferences[x]) * dataDifferences[x])
    })
  }else if(controlFrontMethod=="extrema"){
    if(is.null(extremes)){
      #Get maxima and minima for each dimension of data
      extremes = apply(data,2,function(x){
        return(c(min(x),max(x)))
      })  
    }
    #Generate control data in hyperspace between respective extrma
    controlData = sapply(1:dim,function(y){
      return(runif(min = extremes[1,y],max = extremes[2,y],n = points))
    })
  }else{
    #Generate uniform control data in unit hypercube
    controlData = runif(points * dim)
    #Transform controlData to matrix
    controlData = matrix(controlData,ncol=dim)
  }
  #Finally, apply mock algorithm on control data
  controlFronts = lapply(1:n,function(x){
    return(mock(controlData,skipSearchBest=T,...))
  })
  return(controlFronts)
}