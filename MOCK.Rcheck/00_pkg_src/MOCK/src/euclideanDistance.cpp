#include <Rcpp.h>
using namespace Rcpp;

//'@title Rcpp implementation of euclidean distance between two vectors.
//' @param x First vector.
//' @param y Second vector.
//' @return Euclidean distance between input vectors \code{x} and \code{y}.
//'
//' @examples
//' x = c(1, 3, 5); y = c(4, 3, 1)
//' euclideanDistanceC(x, y)
//'
//' @export
// [[Rcpp::export]]
double euclideanDistanceC(NumericVector x, NumericVector y){
  double sum=0;
  for(int i = 0; i<x.size();i++)
  {
    sum=sum+(pow((x(i)-y(i)),2));
  }
  return(sqrt(sum));
}
