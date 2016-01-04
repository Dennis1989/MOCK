#include <Rcpp.h>
using namespace Rcpp;

//'@export
// [[Rcpp::export]]
NumericVector crossoverMock(NumericVector parent1, NumericVector parent2) {
  NumericVector crossed(parent1.size());
  int cr=0;
  
  for(int i =0;i<parent1.size();i++){
    cr=rbinom(1,1,0.5)(0);
    if(cr==0){
      crossed[i]=parent1[i];}
      else{
      crossed[i]=parent2[i];}
  }
  
  return (crossed);
}

