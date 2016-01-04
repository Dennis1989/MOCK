#' @author Dennis Assenmacher
#' @title Dominated points
#' @description This function is responsible for creating a multidimensional grid
#' @param ep, ngrid
#' @return hypergrid that contains all intervals
#' @export
createGrid=function(ep,ngrid){
  
  #first we need all solutions as a matrix
  matEP=lapply(ep,function(x){
    return (x$solution)
  })
  matEP=do.call(rbind,matEP)
  
  #get minimum value for each dimension
  min = apply(matEP,2,function(x){
    return(min(x))
  })
  
  #get maximum value for each dimension
  max = apply(matEP,2,function(x){
    return(max(x))
  })
  
  #create grid structure
  grid = lapply(1:length(min),FUN = function(x){
    seq(min[x], max[x], length.out = ngrid+1)
  })
  
}