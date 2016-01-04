#' @export
updateGrid = function (ep,grid,nGrid){
  #first we again need our solutions as a matrix
  matEP=lapply(ep,function(x){
    return (x$solution)
  })
  matEP=do.call(rbind,matEP)
  
  #we take each solution
  assignments = getAssignmentC(grid, nGrid, matEP)

return (assignments)

}

