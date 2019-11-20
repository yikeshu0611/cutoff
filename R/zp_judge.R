judge_123 <- function(x){
    x=x[!is.na(x)]
    if (length(x)==1) return(TRUE)
    for (i in 1:(length(x)-1)) {
        if (x[i] > x[i+1]) return(FALSE)
    }
    return(TRUE)
}
judge_321 <- function(x){
    x=x[!is.na(x)]
    if (length(x)==1) return(TRUE)
    for (i in 1:(length(x)-1)) {
        if (x[i] < x[i+1]) return(FALSE)
    }
    return(TRUE)
}
