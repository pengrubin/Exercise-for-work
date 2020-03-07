rawdata = "YUANzhi1987"
new <- NULL
for (i in 1:nchar(rawdata)) {substr(rawdata,i,i)
  if (utf8ToInt(substr(rawdata,i,i))>=97 && utf8ToInt(substr(rawdata,i,i))<=99){
    new <- paste0(new, 2)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=100 && utf8ToInt(substr(rawdata,i,i))<=102){
    new <- paste0(new, 3)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=103 && utf8ToInt(substr(rawdata,i,i))<=105){
    new <- paste0(new, 4)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=106 && utf8ToInt(substr(rawdata,i,i))<=108){
    new <- paste0(new, 5)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=109 && utf8ToInt(substr(rawdata,i,i))<=111){
    new <- paste0(new, 6)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=112 && utf8ToInt(substr(rawdata,i,i))<=115){
    new <- paste0(new, 7)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=116 && utf8ToInt(substr(rawdata,i,i))<=118){
    new <- paste0(new, 8)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=119 && utf8ToInt(substr(rawdata,i,i))<=122){
    new <- paste0(new, 9)
  }
  else if (utf8ToInt(substr(rawdata,i,i))>=65 && utf8ToInt(substr(rawdata,i,i))<=89){
    new <- paste0(new,intToUtf8(utf8ToInt(substr(rawdata,i,i))+33))
  }else if (utf8ToInt(substr(rawdata,i,i))==90){
    new <- paste0(new, "a")
  }
  else {new <- paste0(new,substr(rawdata,i,i))}
}
cat(new)
