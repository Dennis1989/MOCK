#include <Rcpp.h>
using namespace Rcpp;

//' Mutation of individuals in nearest neighbour space
//' @param sol: Solution to mutate
//' @param nn: Nearest neighbours matrix
//' @param neighborhood: Vector containing likelihood that an edge is mutated
//' @param randomVectorMutation: Comparison vector for neighborhood
//' @param randomVectorNeighborSelection: One of L nearest neighbors that is selected in case edge is mutated
//'@export
// [[Rcpp::export]]
IntegerVector mutateC(IntegerVector sol, IntegerMatrix nn, NumericMatrix neighborhood, NumericVector randomVectorMutation,IntegerVector randomVectorNeighborSelection) {
  
  IntegerVector newSol = clone(sol); //not cloning leads to inexplicable side-effects
  for(int i = 0; i<sol.size();i++)
  {
    double lookupNeighbour = neighborhood(i,sol[i] - 1);
    double lookupRandom = randomVectorMutation[i];
    if(lookupRandom < lookupNeighbour)
    {
      newSol[i]=nn(i,randomVectorNeighborSelection[i] - 1);
    }
  }
  return(newSol);
}
