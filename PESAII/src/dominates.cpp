#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

//' @export
// [[Rcpp::export]]
bool dominatesC(NumericVector x, NumericVector y) {
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

// [[Rcpp::export]]
NumericMatrix nonDominatedSolutionsCimp(NumericMatrix m) {
  
  int counter=0;
  int length = m.nrow();
  int colLength = m.ncol();
  bool  indices[length];
  bool trigger = false;
  
  for(int i =0; i < length;i++)
  {
    for(int j=0; j<length;j++)
    {
      //if there is a entry j that dominates i, i should be set to false
      if(dominatesC(m(j,_),m(i,_))&& (i !=j)){
        indices[i]=false;
        trigger=true;
        break;
      } 
    }
    // Trigger is false if there is no other point that dominates the current one
    if(trigger==false){
      indices[i]=true;
      counter++;
    }
    trigger=false;
  }
  NumericMatrix solution(counter,colLength);
  // creating the solution matrix
  int c = 0;
  for(int i =0;i<length;i++)
  {
    if(indices[i]==true){
    solution(c,_)=m(i,_);
    c++;
    }
  } 
  
  return solution; 
}
//'@export
// [[Rcpp::export]]
bool isNonDominated(Rcpp::List ep,NumericVector sol) {
  for(int i = 0;i<ep.length();i++)
  {
    Rcpp::List help=ep[i];
    if(dominatesC(help["solution"],sol)==true)
      return false;
  }
  return (true); 
}

