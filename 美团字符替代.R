con <- file("stdin", "r")  
str = readLines(con, n = 1)
lines = readLines(con, n = 1)
new_str=""
for (i in 1:nchar(str)) {
  if(substr(str, i,i)=='-') {new_str = paste0(new_str,'-')
  }else if(substr(str, i,i)=='0') {new_str = paste0(new_str,'0')
  }else if(substr(str, i,i)=='1') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][1])
  }else if(substr(str, i,i)=='2') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][2])
  }else if(substr(str, i,i)=='3') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][3])
  }else if(substr(str, i,i)=='4') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][4])
  }else if(substr(str, i,i)=='5') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][5])
  }else if(substr(str, i,i)=='6') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][6])
  }else if(substr(str, i,i)=='7') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][7])
  }else if(substr(str, i,i)=='8') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][8])
  }else if(substr(str, i,i)=='9') {new_str = paste0(new_str,strsplit(lines, " ")[[1]][9])
  }else {break}
}
cat(new_str)
