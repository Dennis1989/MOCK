#include <Rcpp.h>
using namespace Rcpp;

//'@title Decode the genetic representation of the edge assignment. 
//'@description Implementation is based on pseudo-code provided by [Julia Handl 2004].
//'@param clusters Integervector containing genetic representation of edges.
//'@return Integervector containing the assigned cluster for each point.
//'@references 2004 Julia Handl - Multiobjective clustering with automatic 
//'determination of the number of clusters.
//'
//'@examples
//'clusters = c(2,3,1,5,4)
//'decodeC(clusters)
//'
//'@export
// [[Rcpp::export]]
IntegerVector decodeC(IntegerVector clusters) {
  int currentCluster = 1;
  IntegerVector clusterAssignment(clusters.length());
  IntegerVector previous(clusters.length());
  for(int i = 0; i < clusters.length(); i++){
    clusterAssignment(i) = -1;
  }
  for(int i = 0; i < clusters.length(); i++){
    int ctr = 0;
    if(clusterAssignment(i) == -1){
      clusterAssignment(i) = currentCluster;
      int neighbour = clusters(i) - 1;
      previous(ctr) = i;
      ctr += 1;
      while(clusterAssignment(neighbour) == -1){
        previous(ctr) = neighbour;
        clusterAssignment(neighbour) = currentCluster;
        neighbour = clusters(neighbour) - 1;
        ctr += 1;
      }
      if(clusterAssignment(neighbour) != currentCluster){
        ctr -= 1;
        while(ctr >= 0){
          clusterAssignment(previous(ctr)) = clusterAssignment(neighbour);
          ctr -= 1;
        }
      }else{
        currentCluster += 1;
      }
    }
  }
  return(clusterAssignment);
}