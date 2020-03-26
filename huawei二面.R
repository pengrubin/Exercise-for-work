data <- 'aaabAbccdddeefff'
data <- toupper(data)
new <- "0"
for (i in 1: nchar(data)) {
  for (j in 1: nchar(data)){
    if (i<=j) {
      if (j<=i+1) { if (nchar(substr(data,i,j))>nchar(new)){
        new <- substr(data,i,j)
      }}
      else if (str_count(substr(data,i,j),substr(data,j,j))<3){
        if (nchar(substr(data,i,j-1))>=nchar(new)){
          new <- substr(data,i,j-1)}
        break
      }
    }
  }
}
new 
