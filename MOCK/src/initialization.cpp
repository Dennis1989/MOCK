#include <Rcpp.h>
using namespace Rcpp;


//'@title Zero-based Rcpp implementation of \code{which} for integer vectors.
//'@param x Integer to be found.
//'@param v IntegerVector in which to find \code{x}.
//'@return indexVector IntegerVector containing all zero-based indexes of 
//'appearences of \code{x} in \code{v}.
//'
//'@examples
//'x = 42; v = 40:44
//'whichC(x,v)
//'
//'@seealso
//'which
//'
//'@export
// [[Rcpp::export]]
NumericMatrix getNeighborhoodMutationMatrixC(IntegerMatrix nearestNeighbors){
  double rows = (double)nearestNeighbors.nrow();
  double cols = (double) nearestNeighbors.ncol();
  
  //Create a square matrix of the same size as nearestNeighbors rows and set diagonal to 0
  NumericMatrix neighborhoodMutationMatrix = NumericMatrix::diag(rows, 0);
  for (int row = 0; row < rows; row++){
    for (int column = 0; column < cols; column++){
      //Formula for neighborhood-Biased Mutation
      neighborhoodMutationMatrix(row,nearestNeighbors(row,column) - 1) = (1 / rows) + pow((column / rows),2);
    }
  }
  
  return(neighborhoodMutationMatrix);
}