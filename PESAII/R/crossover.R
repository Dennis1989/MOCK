#' @author Dennis Assenmacher
#' @title PESA-II Crossover
#' @description Uniform crossover between two individuals
#' @export
crossover = function(v1,v2)
{
  length= length(v1)
  solution = sapply(1:length,function(x){
    crossover=rbinom(1,1,0.5)
    if(crossover==0)
      return(v1[x])
    else
      return(v2[x])
    
  })
  return (solution)
  
}

