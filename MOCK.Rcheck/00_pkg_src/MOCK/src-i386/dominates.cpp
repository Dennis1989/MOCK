#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

//' @export
// [[Rcpp::export]]
bool dominatesCM(NumericVector x, NumericVector y) {
  int trigger = 0;
  int length = x.size();
  for(int i=0;i<length;i++)
  {
    if(x[i]>y[i])
      return false;
    if(x[i]<y[i] && trigger==0)
      trigger =1;
  }
  if(trigger==1)
    return true;
  else return false;
}
