# return x between a and b
# a and b, has no business with which one is big or small
# include can be lower, upper, both and none, any letter, default is l: lower
#set.seed(2019)
#d=round(abs(rnorm(100))*100)
#d2=d[order(d)]
#d2
#x_ab(d2,12,30)
#including the min 12, not including the max 30
x_ab <- function(x,a,b,include='l'){
    min.x=min(a,b)
    max.x=max(a,b)
    if (do::left('lower',nchar(include))==include) res=x[x >=min.x & x < max.x]
    if (do::left('upper',nchar(include))==include) res=x[x > min.x & x <=max.x]
    if (do::left('both',nchar(include))==include)  res=x[x >=min.x & x <=max.x]
    if (do::left('none',nchar(include))==include)  res=x[x > min.x & x < max.x]
    return(res)
}
