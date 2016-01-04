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
#' @importFrom RANN nn2
#Actual MOCK function
mock <- function(data,L=10,ipMax=10,epMax=1000,gens=100,crossRate=0.7,sigma=0.5,nGrid=10,maxCluster=NULL,
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