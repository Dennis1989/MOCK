#include <Rcpp.h>
#include "euclideanDistance.h"
using namespace Rcpp;




//'@title Calculate the mean vector for a matrix by computing the mean 
//'for each column. This is an alternative to apply(m,2,mean).
//'@param m NumericMatrix for which to calculate the mean vector.
//'@return mean NumericVector containing the coordinates of the mean point of \code{m}.
//'
//'@examples
//'m = matrix(1:25,ncol=5)
//'meanVectorOfMatrixC(m)
//'
//'@seealso
//'\code{\link{mean}}
//'
//'@export
// [[Rcpp::export]]
NumericVector meanVectorOfMatrixC(NumericMatrix m){
  NumericVector mean(m.ncol());
  for(int i = 0;i<m.ncol();i++)
  {
    double columnSum = 0;
    for(int j = 0 ; j<m.nrow();j++)
    {
      columnSum += m(j,i);
    }
    mean(i)=columnSum/m.nrow();
  }
  return mean;
}


//'@title Calculate the deviation within a matrix. This is defined as the sum 
//'of deviations of each point from the center vector of the matrix.
//'@param m NumericMatrix for which the deviation is to be calculated.
//'@return deviation The sum of distances of each point from the center of \code{m}
//'
//'@examples
//'v = matrix(1:25,ncol=5)
//'deviationC(v)
//'
//'@export
// [[Rcpp::export]]
double deviationC(NumericMatrix m) {   
  // Calculate means
  NumericVector mean= meanVectorOfMatrixC(m);
  
  //Calculate sum of distances
  double deviation=0;
  for(int i = 0;i<m.nrow();i++)
  {
    deviation += euclideanDistanceC(m(i,_),mean);
  }
  return(deviation);
}

