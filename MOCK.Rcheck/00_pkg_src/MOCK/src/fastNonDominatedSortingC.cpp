#include <Rcpp.h>
#include "dominates.h"
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


//'@title Resize an integervector to a given size.
//'@param v IntegerVector to be resized. All elements with an index greather than 
//'\code{size} are omitted.
//'@param size Integer determining the new seize of \code{v}.
//'@return returnVector Resized IntegerVector \code{v}.
// [[Rcpp::export]]
List resizeList(List v, int size) {
  List returnVector(size);
  for(int i = 0; i < size; i++){
    returnVector(i) = v(i);
  }
  return returnVector;
}


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
List whichCList(int x, IntegerVector v) {
  //Initially this vector has the same size as v and is later resized
  List indexVector(v.length());
  int index = 0;
  for(int i = 0; i < v.length(); i++){
    if(v(i) == x){
      indexVector[index] = i + 1;
      index += 1;
    }
  }
  indexVector = resizeList(indexVector, index);
  return(indexVector);
}



//'@export
// [[Rcpp::export]]
List fastNonDominatedC (NumericMatrix m, Function c) 
{
  int popSize = m.nrow();
  List idxDominators(popSize);
  List idxDominatees(popSize);
  for (int i = 0; i < popSize - 1; i++) {
    NumericVector xi = m(i,_);
    for (int j = i + 1; j < popSize; j++){
      NumericVector xj = m(j,_);
      if (dominatesCM(xi,xj)) {
        idxDominators[j] = c(idxDominators[j], 
                               i + 1);
        idxDominatees[i] = c(idxDominatees[i], 
                               j + 1);
      }
      else if (dominatesCM(xj,xi)) {
        idxDominators[i] = c(idxDominators[i], 
                               j + 1);
        idxDominatees[j] = c(idxDominatees[j], 
                               i + 1);
      }
    }
  }
  return(List::create(Rcpp::Named("idxDominators")=idxDominators,
                      Rcpp::Named("idxDominatees")=idxDominatees));
}
