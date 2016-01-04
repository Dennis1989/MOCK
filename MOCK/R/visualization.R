colorsMOCK <- c("#000000","#a93226","#58d68d","#2471a3","#17a589", "#ca6f1e","#839192", "#A30059", 
            "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87","#5A0007", "#809693", 
            "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80","#bfc9ca")

#' #'@importFrom ggplot2 ggplot
#'@author Dennis Assenmacher
#'@title printClusterSolution
#'@description Draws a set of MOCK-Solutions in searchspace.
#'@param solution Solution returned by MOCK function.
#'@param index Index or vector of indices of MOCK solution/s.
#'@param data Original input data as a matrix or dataframe.
#'@param columns Determines the column count of the plot. 
#'@importFrom gridExtra grid.arrange
#'@export 
printClusterSolution <- function(solution,index,data,columns){
  plots =list()
  
  data =as.data.frame(data)
  colnames(data)<-c("V1","V2")
  plots = lapply(index,function(x){
    ass = decodeC(unlist(solution$sol[[x]]$param))
    data=cbind(data,cluster=ass)
    atCL = assignmentToClusterList(ass)
    p = ggplot()
    p<-p+ theme_bw()+theme(panel.background = element_rect(fill="#F0F8FF"))

    p<-p+ geom_point(data = data, aes(x=V1,y=V2,colour=factor(cluster)))
    
    p<-p+ scale_colour_manual(name="Methods",values=colorsMOCK)+guides(colour=FALSE)
    
    p<-p+xlab("x")+ylab("y") + ggtitle(paste("Dev=",round(solution$sol[[x]]$sol[1]),"Conn=",
                                             round(solution$sol[[x]]$sol[2]),"#Clust=",solution$sol[[x]]$clusters))
    
    
    return (p)
  })
  
  do.call(grid.arrange, c(plots,ncol=columns))
 
}

#' #'@importFrom ggplot2 ggplot
#'@author Dennis Assenmacher
#'@title printClusterSolution
#'@description Plots the attainment score for a distinct solution returned by the mock function.
#'@param sol Solution returned by MOCK function.
#'@return plot Attainment plot 
#'@export 
printAttainmentScore <- function(sol)
{
  m=solutionToMatrix(sol)
  m=cbind(m,index=1:nrow(m))
  p<- ggplot()+ labs(title = "Attainment Score")
  p<-p+ theme_tufte()+theme(panel.background = element_rect(fill="#F0F8FF"))
  p<-p+xlab("#Cluster")+ylab("Attainment Score")
  p<-p+geom_point(data=as.data.frame(m[,3:4]),aes(x=cluster,y=score))
  candidates = getCandidates(m)
  localoptima = which(diff(sign(diff(candidates[,2])))==-2)+1
  p<-p+geom_point(data=as.data.frame(candidates[localoptima,]),aes(x=V1,y=V2),color="red")
 p<- p+geom_text(data=as.data.frame(candidates[localoptima,]),aes(x=V1,y=V2,label=V3),hjust=0, vjust=0)
return(p)
 }

getCandidates = function(m){
candidates = matrix(ncol=3)
for(i in 1:max(m[,"cluster"]))
{
  h=m[m[,"cluster"]==i,c("cluster","score","index"),drop=F]
  if(length(h)>0)
  candidates= rbind(candidates,h[h[,"score",drop=F]==max(h[,"score"])])
}
candidates=candidates[2:nrow(candidates),]
return(candidates)
}


#'@importFrom ggplot2 ggplot
#'@title PrintCurrentSolutions
#'@description Visualizes a 2-dimensional solution of PESA-II.
#'@param sol Solution returned by MOCK function.
#'@param method Determines how the graph should be plotted. Options are: "benchmark","fronts","normalized","unnormalized".
#'@param maxClusters Determines which solutions should be visualized based on the cluster count.
#'@param labelType Determines whether the index of the cluster solution should be displayed within the plot.
#'@param dataset Name of the dataset used in order to print the title.
#'@param markBestsolutions Determines whether the best soultion should be displayed or not.
#'@return ggplot to be created
#'@export 
printParetoFront = function(sol,method="benchmark",maxClusters=25,labelType="index",gridIntervals=0.1,dataset="",markBestsolutions=T)
{
  #Initialize plot
  p<- ggplot()+ labs(title = dataset)
  p<-p+ theme_tufte()+theme(panel.background = element_rect(fill="#F0F8FF"))
  p<-p+xlab("Connectivity")+ylab("Deviation")
  if(method!="unnormalized"){
    p<-p + scale_x_continuous(breaks = round(seq(0, 1.4, by = 0.2),1.4)) +
      scale_y_continuous(breaks = round(seq(0, 1, by = 0.2),1))
  }
  #Get amount of clusters for each solution
  sol$sol = sol$sol[sapply(sol$sol,function(x){x$clusters}) <= maxClusters]
  
  for(i in 1:length(sol$controlFronts)){
    j = 1
    while(j <=length(sol$controlFronts[[i]]$sol)){
      if(sol$controlFronts[[i]]$sol[[j]]$clusters>maxClusters){
        sol$controlFronts[[i]]$sol[[j]] = NULL
        j = j - 1 #index hasn't moved
      }
      j = j + 1
    }
  }
  
  #Normalize solutions
  if(method == "benchmark"){
    #Get matrix from solution
    k=t(sapply(sol$sol,function(x){
      return (x$solution)
    }))
    
    #Find maxima and minima over all points on pareto front
    maximumdev= max(k[,1])
    maximumconn = max(k[,2])
    minimumdev= min(k[,1])
    minimumconn = min(k[,2])
    
    #Normalize based on identified maxima and minima
    k[,1]=sqrt((k[,1] - minimumdev) /(maximumdev - minimumdev))
    k[,2]=sqrt((k[,2] - minimumconn) /(maximumconn - minimumconn))
    
    #Remove solutions with deviation/connectivity < minimumdev, since these would produce NaNs
    #and normalize front
    if(!is.null(sol$kmeans)){
      sol$kmeans = filterAndNormalizeFront(sol$kmeans,minimumdev,maximumdev,minimumconn,maximumconn)
      sol$kmeans = as.data.frame(sol$kmeans)
      sol$kmeans = cbind(sol$kmeans,method=rep("k-Means",nrow(sol$kmeans)))
    }
    if(!is.null(sol$single)){
      sol$single = filterAndNormalizeFront(sol$single,minimumdev,maximumdev,minimumconn,maximumconn)
      sol$single = as.data.frame(sol$single)
      sol$single = cbind(sol$single,method=rep("Single-linkage",nrow(sol$single)))
    }
    if(!is.null(sol$average)){
      sol$average = filterAndNormalizeFront(sol$average,minimumdev,maximumdev,minimumconn,maximumconn)
      sol$average = as.data.frame(sol$average)
      sol$average = cbind(sol$average,method=rep("Average-linkage",nrow(sol$average)))
    }
  }else if(method == "fronts"){
    #Normalize all fronts based on maximum deviation and connectivity of each front
    controlFronts = lapply(sol$controlFronts,function(x){return(normalizeParetoFront(x$sol))})
    #Normalize solution
    k = normalizeParetoFront(sol$sol)
    #Get solution matrix from k
    k=t(sapply(k,function(x){
      return (x$solution)
    }))
    p<-p+guides(color=F)
  }else if(method=="normalized"){
    #Normalize solution
    k = normalizeParetoFront(sol$sol)
    #Get solution matrix from k
    k=t(sapply(k,function(x){
      return (x$solution)
    }))
  }else if(method=="unnormalized"){
    #Get solution matrix from k
    k=t(sapply(sol$sol,function(x){
      return (x$solution)
    }))
  }
  
  #Data frame needed for ggplot2
  k = as.data.frame(k)
  k = cbind(k,method=rep("PESA-II",nrow(k)))
  
  if(labelType=="index"){
    #Add indices to k
    k=cbind(k,label=1:nrow(k))    
  }else if(labelType=="clusters"){
    #Add amount of clusters to k
    k=cbind(k,label=sapply(sol$sol,function(x){x$clusters}))  
  }
  
  if(markBestsolutions==T){
    m=solutionToMatrix(sol)
    m=cbind(m,index=1:nrow(m))
    candidates=getCandidates(m)
    localoptima = candidates[which(diff(sign(diff(candidates[,2])))==-2)+1,]
    k=cbind(k,bestSolution=F)
    k[localoptima[,3],"bestSolution"]=T
  }
  
  #Plot solution as step plot
  p<-p+geom_step(data = k, aes(x=V2,y=V1),color="red")
  if(labelType!='none'){
    p<-p+geom_text(data=k,aes(x=V2,y=V1,label=label),hjust=0, vjust=0)
  }
  
  #Add point plot of solution
  if(markBestsolutions){
    #ToDo: Check why this doesn't work with aesthetics here
    p<-p+geom_point(data = k, aes(x=V2,y=V1),color=c("red","orange")[unlist(k["bestSolution"]+1)],
                                             shape=c(1,22)[unlist(k["bestSolution"]+1)],
                                             size=c(2,4)[unlist(k["bestSolution"]+1)],
                                             bg=c("red","orange")[unlist(k["bestSolution"]+1)])
  }else{
    p<-p+geom_point(data = k, aes(x=V2,y=V1,color=method))
  }
  
  if(method == "benchmark"){
    #Add all benchmark solutions
    if(!is.null(sol$kmeans)){
      p<-p+geom_point(data = as.data.frame(sol$kmeans),aes(x=V2,y=V1,colour=method,shape=method))
    }
    if(!is.null(sol$single)){
      p<-p+geom_point(data = as.data.frame(sol$single),aes(x=V2,y=V1,colour=method,shape=method))
    }
    if(!is.null(sol$average)){
      p<-p+geom_point(data = as.data.frame(sol$average),aes(x=V2,y=V1,colour=method,shape=method))
    }  
    #Plot solution as step plot
    p<-p+geom_step(data = as.data.frame(k), aes(x=V2,y=V1,colour=method,shape=method))
  }else if(method == "fronts"){
    #Add control fronts
    for(i in 1:length(controlFronts)){
      p<-p+geom_step(data = as.data.frame(t(sapply(controlFronts[[i]],function(x){return(x$solution)}))), 
                     aes(x=V2,y=V1),color="grey",lty=2)
    }
    #Plot solution as step plot
    p<-p+geom_step(data = as.data.frame(k), aes(x=V2,y=V1,colour=method))
  }else{
    #Currently do nothing, pareto front is already painted
  }
  cols <- c("PESA-II"="red","k-Means"="darkblue","Single-linkage"="darkgreen","Average-linkage"="orange")
  shapes <- c("PESA-II"=1,"k-Means"=0,"Single-linkage"=2,"Average-linkage"=3)
  
  p<-p+ scale_colour_manual(name="Methods",values=cols)
  p<-p+ scale_shape_manual(name="Methods",values=shapes)
  return(p)
}

#'@title PrintMultipleParetoFronts
#'@description Visualizes a 2-dimensional solution of PESA-II.
#'@param sol Solution returned by MOCK function.
#'@export 
printMultipleParetoFronts = function(solutionList,absolute="T",dataset="")
{
  #Initialize plot
  p<- ggplot()+ labs(title = paste(dataset," - ", ifelse(absolute, "normalized", "unnormalized"),sep = ""))
  p<-p+ theme_tufte()+theme(panel.background = element_rect(fill="#F0F8FF"))
  p<-p+xlab("Connectivity")+ylab("Deviation")
  if(absolute=="T"){
    p<-p + scale_x_continuous(breaks = round(seq(0, 1.4, by = 0.2),1.4)) +
      scale_y_continuous(breaks = round(seq(0, 1, by = 0.2),1))
  }
  #Get all solutions as matrices
  solutions = lapply(solutionList,FUN=function(x)
  {
    m=as.data.frame(solutionToMatrix(x))
    m=cbind(m,method=x$method)
    return(m)
  })
  
  #Normalize solutions
  # if(method == "benchmark"){
  if(absolute == "T")
  {
    index=0
    maximumdev=0
    #Find maxima and minima over all points on all pareto front
    for(i in 1:length(solutions))
    {
      if(max(solutions[[i]][,1])>maximumdev){
        index = i
        maximumdev = max(solutions[[i]][,1])
      }
      
    }
    maximumdev= max(solutions[[index]][,1])#Normalize based on identified maxima and minima
    maximumconn= max(solutions[[index]][,2])
    minimumdev= min(solutions[[index]][,1])#Normalize based on identified maxima and minima
    minimumconn= min(solutions[[index]][,2])
    
    for(i in 1:length(solutions)){
      solutions[[i]][,1]=sqrt((solutions[[i]][,1] - minimumdev) /(maximumdev - minimumdev))
      solutions[[i]][,2]=sqrt((solutions[[i]][,2] - minimumconn) /(maximumconn - minimumconn))
    }
  } 
  cols <- c("PESA-II"="#000000","SMS-EMOA"="#3591d1","NSGA-II"="darkgreen")
  #Plot solution as step plot
  for(i in 1:length(solutions)){
    
    p<-p+geom_step(data = as.data.frame(solutions[[i]]), aes(x=V2,y=V1,colour=method))
    
    #Add point plot of solution
    p<-p+geom_point(data = as.data.frame(solutions[[i]]), aes(x=V2,y=V1,colour=method),size=0.1)
  }
  p<-p+ scale_colour_manual(name="Methods",values=cols)
  return(p)
}

#'@title PrintCurrentSolutions
#'@description Visualizes a 2-dimensional solution of PESA-II.
#'@param sol Solution returned by MOCK function.
#'@export 
printMockRAndMocktoolFronts = function(mockSolution,mockToolSolution,absolute="T",dataset="")
{
  #Initialize plot
  p<- ggplot()+ labs(title = paste(dataset," - ", ifelse(absolute, "normalized", "unnormalized"),sep = ""))
  p<-p+ theme_tufte()+theme(panel.background = element_rect(fill="#F0F8FF"))
  p<-p+xlab("Connectivity")+ylab("Deviation")
  if(absolute=="T"){
    p<-p + scale_x_continuous(breaks = round(seq(0, 1.4, by = 0.2),1.4)) +
      scale_y_continuous(breaks = round(seq(0, 1, by = 0.2),1))
  }
  #Get mockSolution as matrix and convert both to data frame
  mockSolution=as.data.frame(solutionToMatrix(mockSolution))
  mockSolution=cbind(mockSolution,method="Mock R")
  mockToolSolution=as.data.frame(mockToolSolution)
  mockToolSolution=cbind(mockToolSolution,method="Mock Tool")
  
  #Put both in list
  solutions = list(mockSolution,mockToolSolution)
  
  #Normalize solutions
  if(absolute == "T")
  {
    index=0
    minimumdev=Inf
    #Find maxima and minima over all points on all pareto front
    for(i in 1:length(solutions))
    {
      if(min(solutions[[i]][,1])<minimumdev){
        index = i
        minimumdev = min(solutions[[i]][,1])
      }
      
    }
    maximumdev= max(solutions[[index]][,1])#Normalize based on identified maxima and minima
    maximumconn= max(solutions[[index]][,2])
    minimumdev= min(solutions[[index]][,1])#Normalize based on identified maxima and minima
    minimumconn= min(solutions[[index]][,2])
    
    for(i in 1:length(solutions)){
      solutions[[i]][,1]=sqrt((solutions[[i]][,1] - minimumdev) /(maximumdev - minimumdev))
      solutions[[i]][,2]=sqrt((solutions[[i]][,2] - minimumconn) /(maximumconn - minimumconn))
    }
  } 
  cols <- c("Mock R"="#000000","Mock Tool"="orange")
  #Plot solution as step plot
  for(i in 1:length(solutions)){
    
    p<-p+geom_step(data = as.data.frame(solutions[[i]]), aes(x=V2,y=V1,colour=method))
    
    #Add point plot of solution
    p<-p+geom_point(data = as.data.frame(solutions[[i]]), aes(x=V2,y=V1,colour=method),size=0.1)
  }
  p<-p+ scale_colour_manual(name="Methods",values=cols)
  return(p)
}

