rawdata=readline("stdin")

for (i in 1:2) {
  repeat {
    
    if (nchar(rawdata[i])<=8) {
      a=paste0(rawdata[i],paste0(rep("0",8-nchar(rawdata[i])),collapse = ""),collapse = "")
      cat(a)
      cat('\n')
      break
    }
    cat(substr(rawdata[i],1,8))
    cat('\n')
    rawdata[i]=sub(substr(rawdata[i],1,8),"",rawdata[i])
  }
}



i = readLines("stdin")
i = as.numeric(i)
if (floor(i)==i) {
  cat(i*(2+(-1+i*3))/2)
}else{cat("-1")}


for(i in 1:5){ 
  if(i==3){
    next
  }
  print(i)
}
