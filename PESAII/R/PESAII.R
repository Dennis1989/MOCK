#' @author Dennis Assenmacher
#' @title PESAII
#' @description Calculates a list of solutions of a given multiobjective optimisation problem by using the PESA-II algorithm.
#'  @param ipMax Maximum size of internal solutions used to explore new potential solutions.
#'  @param epMax Maximum size of external population.
#'  @param gens Number of generations.
#'  @param nvar Dimensions of paramaterspace.
#'  @param fn Multiobjective function that should be optimised.
#'  @param data Data set as matrix on which to apply \code{PESAII}.
#'  @param nn Nearest neighbors matrix of \code{data}.
#'  @param neighborhoodMutationMatrix Neighborhood matrix which mock mutation is based on.
#'  @param initialPopulation Initial population provided by mock wrapper.
#'  @param L Number of nearest neighbors to consider for mutation and mock function.
#'  @param crossRate Probability for parenting.
#'  @param sigma Standard deviation for mutate function.
#'  @param nGrid Number of grids.
#'  @param vmin minimum of parameter space variable.
#'  @param vmax maximum of parameter space variable.
#'  @param printN Generation interval in which to print progress of function. Progress bar is displayed, if null.
#'  @export
PESAII = function(ipMax,epMax,gens,vmin=0,vmax=1,nvar=6,fn,data,nn,neighborhoodMutationMatrix,
                  initialPopulation = NULL,crossRate=0.7,sigma=0.5,nGrid=20,mock=F,L=10,printN=NULL)
{
  #representing internal population
  IP = list()
  #representing external population
  EP = list()
  #represents the initial population
  population = list()
  
  #Initialize progress bar
  pb = txtProgressBar(min = 0,max = gens,title = "Progress")
  
  #---INITIAL PHASE---
  
  #creating a random initial parameter space population and corresponding fitness
  if(!is.null(initialPopulation)){
    population = initialPopulation
  }else{
    population = lapply(1:ipMax,FUN = function(x){
      param= runif(nvar,vmin,vmax)  
      solution = fn(param)
      return(list(param=param,solution=solution))
    }
    )
  }
  
  #---EVOLUTIONARY PHASE---
  
  for(i in 1:length(population))
  {
    EP=updateEP(EP,population[[i]],epMax,nGrid)
  }
  
  
  #Do the evolutionary steps
  if(gens > 0){
    for(gen in 1: gens)
    {
      #creates grid over n-dimensional space, using nGrids for each dimension
      grid = createGrid(EP,nGrid)
      # assigns the current external population to a specific grid
      assignments = updateGrid(EP,grid,nGrid)
      
      #Empty IP
      IP = list()
      
      #Within this block we are selecting potential candidates from our paretro front (niches)
      #remark: with replacement
      
      assignmentTable = table(assignments)
      for(i in 1:ipMax)
      {  
        #select a random populated grid 
        x=assignmentTable
        niche = x[sample(length(x),1)]
        #niche = sample(table(assignments),1 )
        
        #from that niche select a random solution
        x = which(assignments==as.integer(names(niche[1])))
        nicheSolution = x[sample(length(x),1)]
        
        sol = EP[nicheSolution]
        #Store in IP
        IP = c(IP,sol)
      }
      i=1
      
      #within this block we do apply parenting and our mutation steps
      while(i<ipMax){
        if(runif(1, min = 0, max = 1)<crossRate)
        {
          parent1  = IP[[i]] 
          parent2  = IP[[i+1]]
          IP[[i]]$param  = crossoverMock(parent1$param,parent2$param)
          IP[[i+1]]$param = crossoverMock(parent1$param,parent2$param)
        }
        
        if(mock){
          #Mutate params using mock specific function
          IP[[i]]$param = mutateMock(IP[[i]]$param,nn,neighborhoodMutationMatrix,L)
          IP[[i+1]]$param = mutateMock(IP[[i+1]]$param,nn,neighborhoodMutationMatrix,L)
          #Calculate objective function mock for mutated params
          #This could (probably) be solved independent from mock using ellipses
          IP[[i]]$solution=fn(data,IP[[i]]$param,nn,L)
          IP[[i+1]]$solution= fn(data,IP[[i+1]]$param,nn,L)
        }else{
          #Mutate params
          IP[[i]]$param=mutate(IP[[i]]$param,vmin,vmax,sigma)
          IP[[i+1]]$param= mutate(IP[[i+1]]$param,vmin,vmax,sigma)  
          #Calculate objective function for mutated params
          IP[[i]]$solution=fn(IP[[i]]$param)
          IP[[i+1]]$solution= fn(IP[[i+1]]$param)
        }
        i=i+2
      }
      #now we update our external population EP
      for(i in 1:ipMax)
      {
        EP=updateEP(EP,IP[[i]],epMax,nGrid)      
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
  }
  return (list(nGrid=nGrid,ipMax=ipMax,epMax=epMax,sol=EP,gens=gens,fn=fn))
}

# updates external population based on three rules
#' @export
updateEP = function(ep,sol,epMax,nGrid)
{ 

  #Check whether there is any solution in ep that is dominated by sol
  domination = sapply(ep,FUN=function(x){
    dominatesC(x = sol$solution,y=x$solution)
  })
  
  #if this is the case, replace their parameter and solution space as well
  if(any(domination)){
    
    ep=ep[(which(!domination))]
    
    #save EP
    ep[[length(ep)+1]]=list(solution=sol$solution,param=sol$param)
  } 
  #if no point dominates sol -> sol is nondominated or ep is empty
  else{
    if((isNonDominated(ep,sol$solution)||length(ep)==0)
       &&!any(sapply(ep,FUN=function(x){
                return (all(x$solution==sol$solution))
              }))
    )
  {
    #if EP < maximum size of EP: add to EP
    if(length(ep)<epMax){
      #save ep
      ep[[length(ep)+1]]=list(solution=sol$solution,param=sol$param)
    }
    
    #if there is a solution x in EP that sol is in a less crowded niche than x 
    else{
      #we create a local ep that contains our new solution because our new solution could
      #be greater than max or smaller than min
      localep = ep
      localep[[length(localep)+1]]=list(solution=sol$solution,param=sol$param)
      # creates grid over n-dimensional space, using nGrids for each dimension
      grid = createGrid(localep,nGrid)
      
      # assigns the current external population (current sol not included) to a specific grid
      assignments = updateGrid(localep,grid,nGrid)
      
      #now we get the potential grid index of our current solution in order to check
      #whether there exist another soltion in a more crowded region
      #currentAssignment = updateGrid(list(sol),grid) last entry is current solution
      currentAssignment = assignments[length(assignments)]
      
      #now we get the number of solutions in that grid
      #count - 1 because currentAssignment is already inside the grid
      count = table(assignments)[names(table(assignments)) == currentAssignment]-1
      # if there exist another grid that is more crowded then we delete a solution within
      # that grid randomly
      if(any(table(assignments))>count)
      {
        #we select the niche which contains the maximum number of elements
        x=table(assignments)[(table(assignments))==max(table(assignments))]
        niche=x[sample(length(x),1)]
        
        #out of that grid, we choose a random solution for replacement
        nicheSolution = sample(which(assignments==as.integer(names(niche[1]))),1)
        
        #replace that solution with our current solution
        ep[[nicheSolution]]=sol
      }
      
    }
    }
    
  }
  
  #grid = createGrid(EP,nGrid)
  return(ep)
}

