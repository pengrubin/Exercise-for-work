BitMatrix <- function(n){
  set <- 0:(2^n-1)
  rst <- matrix(0,ncol = n,nrow = 2^n)
  for (i in 1:n){
    rst[, i] = ifelse((set-rowSums(rst*rep(c(2^((n-1):0)), each=2^n)))/(2^(n-i))>=1, 1, 0)
  }
  rst
}

k <- c(1,2,3)
selectMatrix <- BitMatrix(length(k))
apply(selectMatrix, 1, function(x){ k[which(x==1)] })
