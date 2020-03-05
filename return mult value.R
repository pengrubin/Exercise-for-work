rawdata<-matrix(c(1,1,1,1,0,1,1,0,1,0,1,1,0,0,0,0,0,0,0,0),ncol=4)
rawdata<-matrix(c(1,1,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,1),ncol=4)


myfunction <- function(rawdata,newdata,i,j){
  if(i<=0 || j<=0 || i>dim(rawdata)[1] || j>dim(rawdata)[2]) {
    return(list(rawdata,newdata))
  }
  if(rawdata[i,j]==0) {
    return(list(rawdata,newdata))
  }
  if(newdata[i,j]==1) {
    return(list(rawdata,newdata))
  }
  newdata[i,j] <- 1
  test <- Recall(rawdata,newdata,i+1,j)
  rawdata <- test[[1]]
  newdata <- test[[2]]
  test <- Recall(rawdata,newdata,i-1,j)
  rawdata <- test[[1]]
  newdata <- test[[2]]
  test <- Recall(rawdata,newdata,i,j+1)
  rawdata <- test[[1]]
  newdata <- test[[2]]
  test <- Recall(rawdata,newdata,i,j-1)
  rawdata <- test[[1]]
  newdata <- test[[2]]
  return(list(rawdata,newdata))
}

newdata <<- matrix(rep(0,(dim(rawdata)[2])*(dim(rawdata)[1])),ncol = (dim(rawdata)[2]))
island <- 0
for (i in 1:dim(rawdata)[1]) {for (j in 1:dim(rawdata)[2]) {
  if (rawdata[i,j] == 1 && newdata[i,j]==0) {
    test <- myfunction(rawdata,newdata,i,j) 
    rawdata <- test[[1]]
    newdata <- test[[2]]
    island <- island +1}
}}

