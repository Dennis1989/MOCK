#include <Rcpp.h>
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
RNGScope scope;
//'@export
// [[Rcpp::export]]
NumericVector crossoverC(NumericVector parent1, NumericVector parent2) {
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

