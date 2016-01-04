#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector minVector(NumericVector x, int y) {
    for(int i = 0; i < x.length(); i++){
      if(x(i) > y){
        x(i) = y;
      }  
    }
    return x;
}


// [[Rcpp::export]]
NumericVector applySelectionToNeighbours(NumericVector x, NumericMatrix y) {
  for(int i = 0; i < x.length(); i++){
    x(i) = y(i,x(i)-1);
    }  
  return x;
}
