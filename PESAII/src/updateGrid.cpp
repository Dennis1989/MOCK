#include <Rcpp.h>
using namespace Rcpp;

//'@export
// [[Rcpp::export]]
IntegerVector getAssignmentC(List grid, int nGrid, NumericMatrix matEP){
  IntegerVector assignment(matEP.nrow());
  for(int i = 0; i < matEP.nrow(); i++){
    NumericVector sol = matEP(i,_);
    IntegerVector bucketIndex(sol.size());
    for(int j = 0; j < sol.size(); j++){
      int gridi = 0;
      NumericVector currentGrid = grid[j];
      while(!(sol[j]>=currentGrid[gridi]&&(sol[j]<=currentGrid[gridi+1]))){
        gridi += 1;
      }
      bucketIndex[j] = gridi;
    }
    int help=0;
    for(int j = bucketIndex.size() - 1; j > 1; j--){
      help=nGrid*(help+bucketIndex[j]-1);
    }
    int index=bucketIndex[1]+help;
    assignment[i] = index;
  }
  return(assignment);
}