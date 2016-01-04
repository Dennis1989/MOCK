#' @title NSGA2 implementation for MOCK algorithm.
#' @param gens Number of generations.
#' @param fn Function to be applied.
#' @param data Data set on which to apply \code{NSGA2MOCK}.
#' @param nn Nearest neighbors matrix of \code{data}.
#' @param neighborhoodMutationMatrix Neighborhood matrix which mock mutation is based on.
#' @param initialPopulation Initial population provided by mock wrapper.
#' @param L Number of nearest neighbors to consider for mutation and mock function.
#' @param maxPop Maximum size of population.
#' @param tournamentN Size of tournament.
#' @param printN Generation interval in which to print progress of function. Progress bar is displayed, if null.
#' @param distinctSolutions Boolean indicating whether to filter duplicate solutions in decision space.
#' @return Population after \code{gens} generations. Matrix containing \code{maxPop} rows and \code{nrow(data) + 2} columns.
#' Last two columns contain connectivity and deviation.
#' @references 2002 Kalyanmoy Deb - A Fast Elitist Non-Dominated Sorting Genetic Algorithm for Multi-Objective Optimization: NSGA-II
#' @export
NSGA2MOCK = function(gens=100,fn,data,nn,neighborhoodMutationMatrix,population,L=10,maxPop=100,tournamentN=2,
                     printN=NULL,distinctSolutions=F)
{
  #amount of dimensions of parents and offspring matrix including two solution columns
  colDim = nrow(data) + 2
  
  #amount of rows of parents and offspring matrix; equal to initial solutions
  rowDim = length(population)
  
  #initialize parents matrix
  parents = matrix(data=NA,ncol=colDim,nrow = rowDim)
  
  #Initialize progress bar
  pb = txtProgressBar(min = 0,max = gens,title = "Progress")
  
  #save paramspace as matrix
  parents = t(sapply(population,function(x){
    return (x$param)
  }))
 
  #add sol space to same matrix
  parents = cbind(parents,t(sapply(population,function(x){
    return (x$solution)
  })))
  
  #initialize offspring matrix
  offspring = matrix(data=NA,ncol=colDim,nrow = rowDim)
  
  parents = sortAndTruncateParents(parents, colDim, maxPop,distinctSolutions)
  
  
  #Iterate over gens
  if(gens > 0){
    for(gen in 1 : gens)
    {
      #determine mating pool
      #create random candidates; maxPop rows and tournamentN columns
      candidates <- sapply(1:maxPop, function(x){sample(maxPop,tournamentN)})
      
      #conduct tournament on paramsolution
      matingPool <- tournamentSelectionC(parents,maxPop,candidates,order)[,1:(colDim-2)]
      
      j=1
      while(j < maxPop)
      {
        ##do MOCK crossover
        c1=crossoverMock(matingPool[j,],matingPool[j+1,])
        c2=crossoverMock(matingPool[j,],matingPool[j+1,])
        
        ##do mutation
        c1=mutateMock(c1,nn,neighborhoodMutationMatrix,L)
        
        ##do mutation for second child
        c2=mutateMock(c2,nn,neighborhoodMutationMatrix,L)
        
        ## update offspring parameters
        offspring[j,1:(colDim-2)] = c1
        offspring[j+1,1:(colDim-2)] = c2
        
        
        ## update offspring solutions
        offspring[j,c(colDim-1,colDim)]=mockFunction(data,c1,nn,L)
        offspring[j+1,c(colDim-1,colDim)]=mockFunction(data,c2,nn,L)
        
        j=j+2
      }
      
      #merge parent and offspring, leave rank and crowdmeasure out of parent
      parents=rbind(parents[,-c(colDim + 1, colDim + 2)],offspring)
        
      parents = sortAndTruncateParents(parents, colDim, maxPop,distinctSolutions)
      
      if(!is.null(printN)){
        #Update progress
        if (gen %% printN == 0){
          print(paste(gen, " of ", gens, " generations created"))
        }  
      }else{
        #Update progress bar
        setTxtProgressBar(pb=pb,value=gen)
      }
    }
    
    return(parents)
    
  }
}

#Sort parents by front rank and crowdedness and ommitt last nrow(parents) - maxPop solutions.
#Optionally also filter duplicate solutions.
#'@importFrom nsga2R crowdingDist4frnt
sortAndTruncateParents <- function(parents, colDim, maxPop, distinctSolutions){
    
  #do a fast Nondominated sort
  ranking=fastNonDominatedSortingR(parents[,c(colDim-1,colDim)])
  
  #make rank
  rnkIndex <- 1:nrow(parents)
  for(i in 1:length(ranking)){
    rnkIndex[ranking[[i]]] <- i
  }
  #append rank to parents
  parents=cbind(parents,rnkIndex)
  
  #range
  objRange <- apply(parents[,c(colDim-1,colDim)], 2, max) -
    apply(parents[,c(colDim-1,colDim)], 2, min)
  
  #crowding distance
  cd = crowdingDist4frnt(pop = parents[,c(colDim-1,colDim,colDim+1)],rnk = ranking,rng = objRange)
  cd = ifelse(is.infinite(cd),1,cd)
  parents <- cbind(parents,apply(cd,1,sum))
  
  if(distinctSolutions == T){
    #sort by using the connectedness and compactness
    parents <- parents[order(parents[, colDim - 1], -parents[,colDim]),]
    
    indices = numeric()
    i=1
    while(i < nrow(parents))
    {
      ref = parents[i,(colDim-1):colDim]
      for(j in (i+1):nrow(parents))
      {
        i = j +1
        if(isTRUE(all.equal(ref, parents[j,(colDim-1):colDim])))
        {
          indices = c(indices,j)
        }
        else{
          break
        }
      }
    }
    
    #Only remove nrow(parents) - maxPop solutions so that there will always be enough solutions
    if(length(indices)>0){
      if(nrow(parents) - length(indices) < maxPop){
        indices = sample(indices,nrow(parents) - maxPop)
      }
      if(length(indices)>0){
        parents = parents[-c(indices),]  
      }
    }
  }
  
  
  #sort by using the rank index and crowded distance
  parents <- parents[order(parents[, colDim+1], -parents[,colDim+2]),]
  
  
  #truncate parents after maxpopsize rows
  parents=parents[1:maxPop,] 
  
  return(parents)
}