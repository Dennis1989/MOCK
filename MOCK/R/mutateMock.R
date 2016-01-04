#' @title mutateMock Mutation operator for the MOCK algorithm
#' @param param Representation of one cluster solution
#' @param nn Nearest neighbour matrix
#' @param neighborhoodMutationMatrix Matrix which contains the "costs" for each link i->j
#' @return mutatedParam A mutated representatipn of one cluster solution
#' @export
mutateMock <- function(param,nn,neighborhoodMutationMatrix,L){
  #Generate a random vector which can be used in mutateC
  #This is faster than generating a random number for each mutation
  randomVectorMutation = runif(length(param))
  #Generate a vector containing one of L nearest neighbors for each point
  #This is faster than sampling for each mutation
  randomVectorNeighborSelection = sample(L,length(param),T)
  #Mutate param
  mutatedParam = mutateC(param,nn,neighborhoodMutationMatrix,randomVectorMutation,randomVectorNeighborSelection)
  return(mutatedParam)
}