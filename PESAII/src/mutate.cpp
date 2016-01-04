#include <Rcpp.h>
#include <math.h>
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

// [[Rcpp::export]]
Rcpp::NumericVector mutateCR(Rcpp::NumericVector sol, Rcpp::NumericMatrix nn,int L, Rcpp::NumericMatrix neighborhood, Rcpp::NumericVector randomVectorMutation, NumericVector randomVectorSelection) {
  Rcpp::NumericVector newsol = sol;
  for(int i = 0; i<sol.size(); i++)
  {
    if(randomVectorMutation[i] < neighborhood(i,sol[i]-1))
    {
      newsol[i]=nn(i,randomVectorSelection[i]-1);
    }  
  }
  return(newsol);
}
  