#include <Rcpp.h>
#include "binSearch.h"
using namespace Rcpp;

//'@title Calculate connectivity measure for a cluster and a corresponding 
//'neighborhood matrix.
//'@param cluster IntegerVector containing indexes of all points in currently 
//'considered cluster.
//'@param lNN IntegerMatrix containing the L nearest neighbors.
//'@references 2004 Julia Handl - Multiobjective clustering with automatic 
//'determination of the number of clusters.
//'@return connectivity measure according to definition in [Julia Handl 2004].
// [[Rcpp::export]]
double connectivity(IntegerVector cluster, IntegerMatrix lNN) {
  double connectivity = 0;
  
  //for each point in cluster i, we check, whether the n'th nearest neighbor is 
  //in that cluster or not
  for(int j=0;j<cluster.size();j++)
  {
    int rowIndex = cluster[j]-1;
    //iterate over all neigbors of point i
    for(int k = 0; k<lNN.ncol(); k++)
    {
      if(binSearchC(lNN(rowIndex,k),cluster) == false){
        connectivity += (1 / double(k+1));    
      }
    }
  }
  return (connectivity);
}