#include <Rcpp.h>
#include "connectivity.h"
#include "decode.h"
#include "deviation.h"
#include "which.h"

using namespace Rcpp;

//'@title Select rows of a matrix based on a (one-based) index-vector.
//'@param data A NumericMatrix on which a vector is to be applied.
//'@param v A one-based index-vector that is applied on \code{data}.
//'@return dataSelection NumericMatrx containing only the selected 
//'rows of \code{data}.
//'
//'@examples
//'data = matrix(1:25,ncol=5); v = c(1,4)
//'selectRowsOfDatasetC(data,v)
//'
//'@seealso
//'\code{\link{Extract}}
//'
//'@export
// [[Rcpp::export]]
NumericMatrix selectRowsOfDatasetC(NumericMatrix data, IntegerVector v) {
   //Create a new NumericMatrix in which the selection is stored
   NumericMatrix dataSelection(v.length(),data.cols());
   for(int i = 0; i < v.length(); i++){
     dataSelection(i,_) = data(v(i)-1,_);
   }
  return(dataSelection);
}


//'@title Objective MOCK function calculating the deviation and connectivity of a given 
//'genetic representation of a solution.
//'@param data NumericMatrix containing the whole dataset.
//'@param geneOrClusterVector IntegerVector containing either the genetic representation of edges
//'       or the assignment of points to clusters.
//'@param lNN IntegerMatrix containing the L nearest neighbors.
//'@param decoded boolean indicating whether \code{geneOrClusterVector} is already decoded. True if
//'\code{geneOrClusterVector} contains assignment of clusters to points, false if \code{geneOrClusterVector}
//'is genetic representation of edges.
//'@return List consisting of the overall deviation and connectivity of the genes.
//'
//'@examples
//'data = matrix(c(1:10,91:100),ncol=2); genes = c(2,3,4,5,1,7,8,9,10,6); decoded = FALSE
//'lNN = matrix(c(2,1,2,3,4,7,8,9,10,9,3,3,4,5,6,5,6,7,8,8),ncol=2) #nearest neighbors of data, see \code{nn2} in RANN package
//'mockFunctionC(data,genes,lNN,decoded)
//'
//'@export
// [[Rcpp::export]]
List mockFunctionC(NumericMatrix data, IntegerVector geneOrClusterVector, IntegerMatrix lNN, bool decoded) {

  IntegerVector clusterAssignment;
  if(decoded==false){
    //geneOrClusterVector in genetic representation, get corresponding cluster for each point
    clusterAssignment = decodeC(geneOrClusterVector); 
  }else{
    //geneOrClusterVector already cluster assignment to points
    clusterAssignment = geneOrClusterVector;
  }
    
  IntegerVector currentCluster;
  double dev = 0;
  double conn = 0;
  
  // Iterate over each cluster
  for(int i = 0; i < max(clusterAssignment); i++){
    
    // Identify the edges in the current cluster
    currentCluster = whichC(i + 1, clusterAssignment);
    
    //Create a matrix for each solution in a cluster
    NumericMatrix dataClustered(currentCluster.length(),data.cols());
    
    //Add the data of the solution to the matrix
    dataClustered = selectRowsOfDatasetC(data,currentCluster);
    
    //Calculate deviation of our new generated matrix
    dev += deviationC(dataClustered);
    
    //Calculate connectivity out of our new generated matrix
    conn += connectivity(currentCluster,lNN);
  }
  
  //return solution with deviation and connectivity
  return(List::create(Rcpp::Named("dev") = dev,
                     Rcpp::Named("occ") = conn));
  
}