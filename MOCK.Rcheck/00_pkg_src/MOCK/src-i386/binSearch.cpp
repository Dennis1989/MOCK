#include <Rcpp.h>
using namespace Rcpp;

//'@title Calculate the (floor) mid of two integers.
//'@param a First integer.
//'@param b Second integer.
//'@return Floor mid of \code{a} and \code{b}.
// [[Rcpp::export]]
int midpoint(int a, int b){
  int difference = b - a;
  if(difference % 2 == 0){
    return ((b+a) / 2);
  }else{
    return ((b+a-1) /2);
  }
}



//'@title Find an integer in an ordered vector using binary search.
//'@param i Integer to be found in vector.
//'@param v IntegerVector ordered increasingly in which \code{i} is to be found.
//'@return boolean indicating whether \code{v} contains \code{i}.
//'
//'@examples
//'i = 42; v = 0:41
//'binSearchC(i,v)
//'
//'@seealso
//'\code{\link{match}}
//'
//'@export
// [[Rcpp::export]]
bool binSearchC(int i,  IntegerVector v){
  int lowerIndex = 0;
  int upperIndex = v.length() - 1;
  //each iteration either lowerindex is increased or upperindex decreased
  while (lowerIndex <= upperIndex)
  {
    //calculate mid point such
    int midIndex = midpoint(lowerIndex, upperIndex);
    if (v[midIndex] == i){
      //i is found in v
      return (true);
    }
    //narrow down the search space to one half of v
    else if (v[midIndex] < i)
    {
      lowerIndex = midIndex + 1;
    }
    else        
    {
      upperIndex = midIndex - 1;
    }
  }
  // v doesn't contain i
  return (false);
}
