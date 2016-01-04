#include <Rcpp.h>
using namespace Rcpp;

//'@title Resize an integervector to a given size.
//'@param v IntegerVector to be resized. All elements with an index greather than 
//'\code{size} are omitted.
//'@param size Integer determining the new seize of \code{v}.
//'@return Resized IntegerVector \code{v}.
// [[Rcpp::export]]
IntegerVector resizeVector(IntegerVector v, int size) {
  IntegerVector returnVector(size);
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
//'\code{\link{which}}
//'
//'@export
// [[Rcpp::export]]
IntegerVector whichC(int x, IntegerVector v) {
  //Initially this vector has the same size as v and is later resized
  IntegerVector indexVector(v.length());
  int index = 0;
  for(int i = 0; i < v.length(); i++){
    if(v(i) == x){
      indexVector(index) = i + 1;
      index += 1;
    }
  }
  indexVector = resizeVector(indexVector, index);
  return(indexVector);
}