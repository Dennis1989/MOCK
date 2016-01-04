require(RANN)
require(MOCK)
convertMockSolutionFileToMatrix <- function(clusterData,L,path,solutionIndex,pattern="-[0-9]*.solution",mode="solution"){
  #Calculate l nearest neighbors
  lnn = nn2(clusterData,k=L)$nn.idx[,-1]
  clusterData = as.matrix(clusterData)
  #Get list of files 
  files = list.files(path = path,pattern = paste(solutionIndex,pattern,sep=""),full.names=T)
  
  if(mode=="solution"){
  #For all files: Read csv, apply mock function and append number of clusters
  mockImport = unname(t(sapply(files,function(file){
    currentFile = read.csv(file,header=F,sep = " ")
    #Apply mockFunctionC on cluster Data using fourth column of mock solution file which contains the
    #cluster assignment. Therefore, set decoded to true.
    solution = mockFunctionC(clusterData,currentFile[,4]+1,lnn,decoded=T)
    return(as.numeric(c(solution,max(currentFile[4]+1))))
  })))
  deviations <- mockImport[,1]
  #unlist and order deviations
  deviations <- order(deviations,decreasing = T)
  #apply ordering to mockSolution
  mockImport = mockImport[deviations,]
  return(mockImport)}else{
    return(unname(t(sapply(files,function(file){
      currentFile = read.csv(file,header=F,sep = " ")
      return(currentFile[,4]+1)
    }))))
  }
}