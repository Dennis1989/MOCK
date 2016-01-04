#library(igraph)
#' @export
#This file contains the MOCK function passed to the PESA-II algorithm as well as a wrapper for the C implementation of the mock function

#Wrapper for mockFunctionC
mockFunction <- function(data,param,nn,L){
  
  solution = mockFunctionC(data,param,nn[,1:L],F)
  solution <- c(V1 = solution$dev, V2 = solution$occ)
  
  return(solution)
}

#' @export
#' @title Multiobjective clustering with automatic determination of the number of clusters.
#' @description Implementation of MOCK as proposed by Handl.
#' @param data Dataset on which to apply the MOCK algorithm.
#' @param L Amount of nearest neighbors to consider for initialization, mutation, and connectivity objective function.
#' Defaults to 10.
#' @param ipMax Maximum size of internal solutions used to explore new potential solutions. Defaults to 10.
#' @param epMax Maximum size of external population. Defaults to 1000.
#' @param gens Number of generations. Defaults to 1000.
#' @param crossRate Probability for parenting. Defaults to 0.7.
#' @param sigma Standard deviation for mutate function. Defaults to 0.5.
#' @param nGrid Number of grids. Defaults to 10.
#' @param maxCluster Basis for initialization. Corresponds to k_user of original MOCK parameters. Default is dependent on
#' chosen EMOA.
#' @param method EMOA to apply. Defaults to "pesa". Other EMOAs are "nsga2" and "smsemoa".
#' @param tournamentN Size of tournament when applying tournament selection in NSGA-II. Defaults to 2.
#' @param printN Intervals (in generations) in which to update the progress updates when running the EMOAs.
#' Simple one line progress bar, if NULL. Defaults to 10.
#' @param distinctSolutions Boolean indicating whether NSGA-II and SMS-EMOA should omit duplicate solutions.
#' Duplicates are removed within objective space. PESA-II doesn't add duplicate solutions to pareto front. Defaults to false.
#' @param controlFronts Amount of control fronts to create after applying the EMOA. Defaults to 3.
#' @param controlFrontMethod Method for creating the control data on which control fronts are based. Defaults to the
#' method of creating uniform data in eigenspace of the original dataset proposed in 2007 by Handl. Other methods are
#' "extrema" which creates points uniformly in original space of dataset and "" which creates points uniformly in unit hypercube.
#' @param skipSearchBest Boolean indicating whether search for best MOCK solution should be skipped. Defaults to false and is set
#' to true when control fronts are generated.
#' @return List of 9 items. $sol allows to access the list of cluster solutions. Each cluster solution vector can be accessed with $solution,
#' the encoded clustering with $param (can be decoded using decodeC), the amount of clusters with clusters, and the attainment score with $score.
#' The index of the best solution can be accessed via $best. Also, some of the paramaters of the mock function call are included.
#' @references 2004 Julia Handl - Multiobjective clustering with automatic 
#' determination of the number of clusters.
#' @references 2007 Julia Handl - An Evolutionary Approach to
#' Multiobjective Clustering
#' @importFrom RANN nn2
mock <- function(data,L=10,ipMax=10,epMax=1000,gens=1000,crossRate=0.7,sigma=0.5,nGrid=10,maxCluster=NULL,
                 method="pesa",tournamentN=2,printN=10,distinctSolutions=F,controlFronts=3,controlFrontMethod="eigen",
                 skipSearchBest = F){
  if(!is.matrix(data)){
    data = as.matrix(data) #Data required as matrix
  }
  
  if(is.null(maxCluster)){
    #If maxCluster was not provided choose a value dependent on method
    if(method=="pesa"){
      maxCluster=25
    }else{
      maxCluster=50
    }
  }
  
  #Calculate initialization parameters for PESA-II
  #nearestNeighbors = nn2(data, k  = L + 1)$nn.idx[,-1] #[,-1] because point itself should not be first neighbour
  nearestNeighbors = nn2(data, k=nrow(data) )$nn.idx[,-1] #
  
  neighborhoodMutationMatrix = getNeighborhoodMutationMatrixC(nearestNeighbors)
  
  #distance Matrix
  distanceMatrix = dist(data)
  
  #minimum parameter space
  vmin = 1
  #maximum parameter space
  vmax = nrow(data)
  
  #maximum amount of initial MST-based solutions
  maxMST=maxCluster
  maxkMeans=maxCluster
  
  print("(1|6) START INITIALIZING MST-SOLUTIONS")
  
  #Create minimum spanning tree (MST) and interestingness.
  minimumSpanningTree = initializeMST(distanceMatrix)
  interestingness = calculateInterestingness(minimumSpanningTree,nearestNeighbors,L)
  
  #initial population connectivity based
  removedLinks = NULL
  initialPopulation = list()
  mstPopulation = list()
  
  if(distinctSolutions == F){
  #other initial MST solutions based on interestingness
  for(x in 1:maxMST){
    callbackSolution = pruneMSTInterestingness(edges=minimumSpanningTree,x, removedLinks,
                                    interestingness = interestingness,nn = nearestNeighbors,L = L)
    solution = mockFunction(data,callbackSolution$solution,nearestNeighbors, L)
    removedLinks = callbackSolution$removedLinks
    mstSolution = list(param=callbackSolution$solution,solution=solution)
    mstPopulation[[x]] = mstSolution
  }
  
  #initial MST population KMEANS based
  print("(2|6) BEGIN KMEANS INITIALIZATION")
  
  kmeansPopulation = lapply(1:maxkMeans,FUN=function(x){
    #generates all the Kmeans Solutions from k = 2
    param = generateKMeansSolution(edges = minimumSpanningTree,
                                   nn = nearestNeighbors,k = x,L = 10,data=data )
    solution = mockFunction(data,param,nearestNeighbors,L)
    return(list(param=param,solution=solution))
  }
    )
  
  #connecting the MST based and kMeans based solutions
  initialPopulation = append(mstPopulation,kmeansPopulation)
  }else{
    #other initial MST solutions based on interestingness
    k= 1
    while(length(initialPopulation) < maxMST && k < nrow(data)){
      callbackSolution = pruneMSTInterestingness(edges=minimumSpanningTree,k, removedLinks,
                                                 interestingness = interestingness,nn = nearestNeighbors,L = L)
      solution = mockFunction(data,callbackSolution$solution,nearestNeighbors, L)
      removedLinks = callbackSolution$removedLinks
      param = callbackSolution$solution
      initialPopulation = addSolutionDistinct(initialPopulation,solution,param)
      k = k + 1
    }
    
    #initial MST population KMEANS based
    print("(2|6) BEGIN KMEANS INITIALIZATION")
    
    k = 1
    index = length(initialPopulation) + 1
    while(length(initialPopulation) < maxMST + maxkMeans && k < nrow(data)){
      param = generateKMeansSolution(edges = minimumSpanningTree,
                                     nn = nearestNeighbors,k = k,L = 10,data=data )
      solution = mockFunction(data,param,nearestNeighbors,L)
      initialPopulation = addSolutionDistinct(initialPopulation,solution,param)
      k = k + 1
    }
  }
  
  mockSolution = list()
  
  #run PESAII with the initial population
  if(method == "pesa"){
    print("(3|6) PESA BEGINS")
    mockSolution = PESAII(ipMax = ipMax, epMax = epMax, gens = gens, vmin = vmin, vmax = vmax, fn = mockFunction, 
                        data = data, nn = nearestNeighbors, neighborhoodMutationMatrix = neighborhoodMutationMatrix,                        
                        initialPopulation = initialPopulation, sigma = sigma, nGrid = nGrid, 
                        mock=T, crossRate = crossRate, L = L,printN = printN)
    mockSolution$method ="PESA-II"
  
  }else if(method == "nsga2"){
    print("(3|6) NSGA2 BEGINS")
    nsga2sol = NSGA2MOCK( gens = gens, fn = mockFunction, data = data, nn = nearestNeighbors, 
                          neighborhoodMutationMatrix = neighborhoodMutationMatrix,population = initialPopulation,
                          L = L,maxPop = length(initialPopulation),tournamentN,printN,distinctSolutions)
    mockSolution$nGrid = nGrid
    mockSolution$ipMax = ipMax
    mockSolution$epMax = epMax
    mockSolution$gens = gens
    mockSolution$fn = mockFunction
    mockSolution$method ="NSGA-II"
    dimCol = nrow(data)
    mockSolution$sol = lapply(1:nrow(nsga2sol), function(x){
      return(list(solution = c(nsga2sol[x,dimCol + 1],nsga2sol[x,dimCol + 2]),param=nsga2sol[x,1:dimCol]))
    })
    
      
  }else if(method =="smsemoa"){
    print("(3|6) SMS-EMOA BEGINS")
    smsemoaSolution = SMSEMOAMOCK(gens = gens, fn = mockFunction, data = data, nn = nearestNeighbors, 
                                 neighborhoodMutationMatrix = neighborhoodMutationMatrix,population = initialPopulation,
                                 L = L,maxPop = length(initialPopulation),tournamentN,printN,distinctSolutions)
    mockSolution$nGrid = nGrid
    mockSolution$ipMax = ipMax
    mockSolution$epMax = epMax
    mockSolution$gens = gens
    mockSolution$fn = mockFunction
    mockSolution$method ="SMS-EMOA"
    mockSolution$sol = lapply(1:nrow(smsemoaSolution$param), function(x){
      return(list(solution = smsemoaSolution$objective[x,],param=smsemoaSolution$param[x,]))
    })
  }else{
    stop("MOCK method not found. Available methods: 'pesa', 'nsga2', 'smsemoa'")
  }
  
  #order mockSolution by deviation
  #first get list of deviations only
  deviations <- lapply(mockSolution$sol,function(x){return(x$solution["V1"])})
  #unlist and order deviations
  deviations <- order(sapply(deviations,"[[","V1"),decreasing = T)
  #apply ordering to mockSolution
  mockSolution$sol = mockSolution$sol[deviations]
  #add amount of clusters to mockSolution
  mockSolution$sol <- lapply(mockSolution$sol, function(x){
    clusterSolution <- decodeC(x$param)
    x$clusters = max(clusterSolution)
    return(x)
  })
  
  #If control fronts are generated, search for best solution shall be skipped.
  #Also no benchmarking reference fronts are created
  if(skipSearchBest == F){
    #Identify best solution
    if(typeof(controlFronts)=="double"){
      #controlFronts determines amount of fronts
      if(controlFronts>0){
      print("(5|6) GENERATE CONTORL FRONTS")
      #Generate control fronts using the same parameters for mock as for the pareto front
      controlFronts=createControlFronts(data,method=method,n=controlFronts,L=L,ipMax=ipMax,epMax=epMax,gens=gens,
                                        crossRate=crossRate,sigma=sigma,nGrid=nGrid,maxCluster=maxCluster,
                                        tournamentN=tournamentN,printN=printN,distinctSolutions=distinctSolutions)
      }else if(controlFronts==0){
        print("(5|6) NO CONTROL FRONTS REQUIRED - FINISHED MOCK")
        #Don't create control fronts but only return solution.
        return(mockSolution)
      }
    }else if(typeof(controlFronts)!="list"){
      #If controlFronts is neither list nor double greater/equal zero: Invalid input.
      print("Invalid parameter 'controlFronts'. Must be list of control fronts or a positive integer determining 
           # amount of control fronts.")
      return(mockSolution)
    }
    print("(6|6) IDENTIFY BEST SOLUTION")
    scores = calculateScores(mockSolution,controlFronts)
    mockSolution$best = which(scores == max(scores))
    mockSolution$sol = lapply(1:length(mockSolution$sol),function(x){
      mockSolution$sol[[x]]$score = scores[x]
      return(mockSolution$sol[[x]])
    })
    mockSolution$controlFronts = controlFronts
    print("FINISHED MOCK")
  }
  return(mockSolution)
}