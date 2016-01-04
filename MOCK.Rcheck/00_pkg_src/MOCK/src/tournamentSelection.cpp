#include <Rcpp.h>
using namespace Rcpp;

//'@title Select rows of a matrix based on a vector.
//'@param data A NumericMatrix on which a vector is to be applied.
//'@param v A vector that is applied on \code{data}.
//'@return dataSelection NumericMatrx containing only the selected 
//'rows of \code{data}.
//'
//'@examples
//'data = matrix(1:25,ncol=5); v = c(1,4)
//'selectRowsOfDatasetC(data,v)
//'
//'@seealso
//'base::Extract
//'
//'@export
// [[Rcpp::export]]
IntegerMatrix selectRowsOfDatasetCInteger(IntegerMatrix data, IntegerVector v) {
  //Create a new NumericMatrix in which the selection is stored
  //Initially this matrix has the same size and is later resized
  IntegerMatrix dataSelection(v.length(),data.cols());
  for(int i = 0; i < v.length(); i++){
    dataSelection(i,_) = data(v(i)-1,_);
  }
  return(dataSelection);
}

//'@export
// [[Rcpp::export]]
IntegerMatrix tournamentSelectionC(IntegerMatrix pop, int pool_size, IntegerMatrix candidates, Function order) 
{
  int popSize = pop.nrow(); //50
  int dim = pop.ncol();     //1000
  IntegerMatrix f(pool_size,dim);  //50x1000
  for(int i = 0; i < pool_size; i++){ //0 to 49
    IntegerVector candidate = candidates(_,i); //50x50 -> 50
    IntegerMatrix tmp = selectRowsOfDatasetCInteger(pop, candidate);
    IntegerVector ordered = order(tmp(_, dim - 2), -tmp(_, dim - 1));
    f(i,_) = tmp(ordered[0]-1,_);
  }
  return(f);
}
