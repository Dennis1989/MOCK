#'  @export
NSGA2MOCK = function(gens,fn,data,nn,NeighborhoodMutationMatrix,initialPopulation,crossRate=0.7,sigma=0.5,mock=T,L=10)
{
  #creating a random initial parameter space population and corresponding fitness
  if(!is.null(initialPopulation)){
    population = initialPopulation
    length = nrow(data)
    
  }else{
    population = lapply(1:ipMax,FUN = function(x){
      param= runif(nvar,vmin,vmax)  
      solution = fn(param)
      return(list(param=param,solution=solution))
    }
    )
  }
  
  #create initial parents and offspring
  
  #save paramspace as matrix
  parents$param = lapply(population,function(x){
    return (x$param)
  })
  
  parents$param=do.call(rbind,parents$param)
  
  #save sol space as matrix
  parents$solution = lapply(population,function(x){
    return (x$solution)
  })
  
  parents$solution=do.call(rbind,parents$solution)
  
  offspring = list()
  
  #Generations
  if(gens > 0){
    for(i in 1: gens)
    {
      #merge parent and offspring
      parents$param=rbind(parents$param,offspring$param)
      parents$solution=rbind(parents$solution,offspring$solution)
      pop=parents$solution
      
      #do a fast Nondominated sort
      ranking=fastNonDominatedSorting(pop)
      
      #make rank
      rnkIndex <- integer(nrow(pop))
      i <- 1
      while (i <= length(ranking)) {
        rnkIndex[ranking[[i]]] <- i
        i <- i + 1
      }
      
      #append rank to pop
      pop=cbind(pop,rnkIndex)
      
      #range
      objRange <- apply(pop[,1:2], 2, max) -
        apply(pop[,1:2], 2, min)
      
      #crowding distance
      cd = crowdingDist4frnt(pop = pop,rnk = ranking,rng = objRange)
      pop <- cbind(pop,apply(cd,1,sum))
      
      #sort by using the rank index and crowded distance
      pop.sort <- pop[order(pop[, 3], -pop[,4]),] 
      parents$param <- parents$param[order(pop[, 3], -pop[,4]),]
      #only use maxpopsize solutions
      pop=pop.sort[1:maxPop,]
      
      parents$solution=pop
      parents$param=parents$param[1:maxPop,] 
                                    
      #determine mating pool
      matingPool <- tournamentSelection(parents$param, maxPop, maxPop)
      
      for(i in 1:popSize)
      {
        ##do MOCK crossover
        c1=MOCK:::crossoverC(matingPool[i,],matingPool[i+1,])
        c2=MOCK:::crossoverC(matingPool[i,],matingPool[i+1,])
        
        ##do mutation
        randomVectorMutation = runif(length(c1))
        randomVectorNeighborSelection = sample(L,length(c1),T)
        c1=MOCK:::mutateC(c1,nn,neigborhood,randomVectorMutation,randomVectorNeighborSelection)
        
        ##do mutation for second child
        randomVectorMutation = runif(length(c2))
        randomVectorNeighborSelection = sample(L,length(c2),T)
        c2=MOCK:::mutateC(c1,nn,neigborhood,randomVectorMutation,randomVectorNeighborSelection)
        
        ## update param
        offspring$param=rbind(c1,c2)
        
        ## update solution
        offspring$solution=rbind(MOCK:::mockFunction(data,c1,nn,L),MOCK:::mockFunction(data,c2,nn,L))
        i=i+2
      }
      
        
    }
    
    return(offspring)
  
  }
}