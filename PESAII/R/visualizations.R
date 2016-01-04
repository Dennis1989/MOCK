#'@importFrom ggplot2 ggplot
#'@author Dennis Assenmacher
#'@title PrintCurrentSolutions
#'@description Visualizes a 2 dimensional solution of PESA-II
#'@param ep External population returned by PESA-II
#'@export 
printParetroFrontPESA= function(sol)
{
  grid = createGrid(sol$sol,sol$nGrid)
  # z=lapply(population,function(x){
  #   return (x$solution)
  # })
  # z=as.data.frame(do.call(rbind,ep))
  
  p<- ggplot()
  #p <-p+ geom_point(data = z, aes(x=V1,y=V2))
  
  k=lapply(sol$sol,function(x){
    return (x$solution)
  })
  k=as.data.frame(do.call(rbind,k))
  
  p<-p+ geom_point(data = k, aes(x=V1,y=V2),color="red")
  
  for(i in 1:length(grid[[1]]))
  {
    p<-p+geom_vline(xintercept=grid[[1]][i],colour="green", linetype = "longdash")
  }
  
  for(i in 1:length(grid[[2]]))
  {
    p<-p+geom_hline(yintercept=grid[[2]][i],colour="green", linetype = "longdash")
  }
  return(p)
  # points(k,col="red")
}