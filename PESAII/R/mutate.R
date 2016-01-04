require(Rcpp)
#' @author Dennis Assenmacher
#' @title PESA-II Mutate
#' @description Gaussian mutation of one individual
#' @param min Minimum value after mutation
#' @param max Maximum value after mutation
#' @param sigma Variance
#' @export
mutate = function(sol,min,max,sigma=0.3){
  x=sol+rnorm(0,n=length(sol),sd=sigma)
  
  x=pmax(min,pmin(max,x))
  return(x)
  
}