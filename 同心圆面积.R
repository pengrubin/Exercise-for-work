num = readline('stdin')
r = readline('stdin')

r_num=strsplit(r,' ')[[1]]
r_order=order(r_num,decreasing = TRUE)
S=0

for (i in 1: num) {
S=S+(-1)^(i+1)*pi*r_order[i]^2
}

cat(round(S,5))
