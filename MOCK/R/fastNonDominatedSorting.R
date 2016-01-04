fastNonDominatedSortingR <- function(inputData){
  popSize = nrow(inputData)
  
  idxDom = fastNonDominatedC(inputData,c)
  idxDominators = sapply(idxDom$idxDominators,function(x){if(length(x)==0)return(NULL)else return(x)})
  idxDominatees = sapply(idxDom$idxDominatees,function(x){if(length(x)==0)return(NULL)else return(x)})
  
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    for (i in 1:noSolInCurrFrnt) {
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (j in hisDominatees) {
        noDominators[[j]] <- noDominators[[j]] - 1
        if (noDominators[[j]] == 0) {
          Q <- c(Q, j)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
  }
  return(rnkList)
}