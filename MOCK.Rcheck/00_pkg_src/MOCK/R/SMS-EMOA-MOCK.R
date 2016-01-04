
#'@importFrom emoa hypervolume_contribution
SMSEMOAMOCK = function(gens=100,fn,data,nn,neighborhoodMutationMatrix,population,L=10,maxPop=100,tournamentN=2,
                        printN=NULL,distinctSolutions=F){
  lastLayer=matrix()
  parents = list()
  
  #Initialize progress bar
  pb = txtProgressBar(min = 0,max = gens,title = "Progress")
  
  #save paramspace as matrix
  parents$param = t(sapply(population,function(x){
    return (x$param)
  }))
  
  #save paramspace as matrix
  parents$objective = t(sapply(population,function(x){
    return (x$solution)
  }))
  

  for(gen in 1 : gens)
  {
    #get two parents
    p1 = parents$param[sample(1:nrow(parents$param),1),]
    p2 = parents$param[sample(1:nrow(parents$param),1),]
    
    #do crossover
    child = crossoverMock(p1,p2)
    
    #mutate the child
    mutatedChild = mutateMock(child,nn,neighborhoodMutationMatrix,L)
    
    #append child to param
    parents$param = rbind(parents$param,mutatedChild)
    
    #append child to solution
    parents$objective = rbind(parents$objective,mockFunction(data,mutatedChild,nn,L))
    
    #do a fast Nondominated sort
    ranking=fastNonDominatedSortingR(parents$objective)
    
    #make rank
    rnkIndex <- 1:nrow(parents$objective)
    for(i in 1:length(ranking)){
      rnkIndex[ranking[[i]]] <- i
    }
    #append rank to parents
    parents$objective=cbind(parents$objective,rnkIndex,index=1:nrow(parents$objective))
    
    #range
    lastLayer = parents$objective[parents$objective[,"rnkIndex"]==max(parents$objective[,"rnkIndex"]),,drop = FALSE]
    
    #if last layer contains only one solution. Delete it
    if(nrow(lastLayer)==1){
      deleteIndex=parents$objective[,"index"]!=lastLayer[,"index"]
      parents$objective=parents$objective[deleteIndex,1:2]
      parents$param=parents$param[deleteIndex,]
    }
    
    else{
      contribution = hypervolume_contribution(t(lastLayer[,1:2]))
      index = match(min(contribution),contribution)
      elementTofilter = lastLayer[index,, drop = FALSE]
      deleteIndex=parents$objective[,"index"]!=elementTofilter[,"index"]
      parents$objective=parents$objective[deleteIndex,1:2]
      parents$param=parents$param[deleteIndex,]
    }
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