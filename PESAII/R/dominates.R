#' @author Dennis Assenmacher
#' @examples dominates(c(3,2),c(5,6)) == TRUE
#' @title Dominated points
#' @description Calculates whether a vector x domonates another vector y
#' @param x, y Vector that holds fitness values.
#' @return TRUE if x dominates y, FALSE if x does not dominate y
#' @export
dominates = function(x, y) {
  return(all(x <= y) && any(x < y))
}

#' @author Dennis Assenmacher
#' @examples nonDominatedSolutions(mtcars[,c("mpg","hp")])
#' @title Non-dominated solutions
#' @description Calculates all entries which are not dominated by any other entry.
#' @param m Matrix or data.frame that contains rowwise entries.
#' @return Matrix which contains all non-dominated points
#' @export
nonDominatedSolutions = function(obj)
{
  indicator=apply(obj,1,function(x){
    for(i in 1:nrow(obj))
    {
      if(dominates(obj[i,],x)){
        return (FALSE)
      }
    }
    return (TRUE)   
  })
  
  return (obj[indicator,])
}

#' @author Dennis Assenmacher
#' @examples nonDominatedSolutionsC(mtcars[,c("mpg","hp")])
#' @title Non-dominated solutions
#' @description Calculates all entries which are not dominated by any other entry.
#' @param m Matrix or data.frame that contains rowwise entries.
#' @return Matrix which contains all non-dominated points
#' @export
nonDominatedSolutionsC = function(obj){
  if(is.data.frame(obj))
  {
    nonDominatedSolutionsCimp(as.matrix(obj))
  }
  else{
    nonDominatedSolutionsCimp(obj)
  }
    
}

testtime = function(){
  set.seed(1234)
  for(i in 1:10000)
  {
    vector1 <- sample(1:1000,500,replace=T)/100
    vector2 <- sample(1:1000,500,replace=T)/100
    dominates(vector1,vector2)
  }
}

nonDominatedSolutionsIndex = function(obj)
{
  indicator=apply(obj,1,function(x){
    for(i in 1:nrow(obj))
    {
      if(dominates(obj[i,],x)){
        return (FALSE)
      }
    }
    return (TRUE)   
  })
  
  return (indicator)
}
