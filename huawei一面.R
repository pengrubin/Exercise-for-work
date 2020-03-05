rawdata<-matrix(c(1,1,1,1,0,1,1,0,1,0,1,1,0,0,0,0,0,0,0,0),ncol=4)
rawdata<-matrix(c(1,1,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,1),ncol=4)


myfunction <- function(rawdata,newdata,i,j){
  if(i<=0 || j<=0 || i>dim(rawdata)[1] || j>dim(rawdata)[2]) {
    return(newdata)
  }
  if(rawdata[i,j]==0) {
    return(newdata)
  }
  if(newdata[i,j]==1) {
    return(newdata)
  }
  newdata[i,j] <- 1
  newdata <- Recall(rawdata,newdata,i+1,j)
  newdata <- Recall(rawdata,newdata,i-1,j)
  newdata <- Recall(rawdata,newdata,i,j+1)
  newdata <- Recall(rawdata,newdata,i,j-1)
  return(newdata)
}

newdata <<- matrix(rep(0,(dim(rawdata)[2])*(dim(rawdata)[1])),ncol = (dim(rawdata)[2]))
island <- 0
for (i in 1:dim(rawdata)[1]) {for (j in 1:dim(rawdata)[2]) {
  if (rawdata[i,j] == 1 && newdata[i,j]==0) {
    newdata <- myfunction(rawdata,newdata,i,j) 
    island <- island +1}
  
}}

