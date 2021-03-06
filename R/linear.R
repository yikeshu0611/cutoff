#' @title Significant Cutoff Value for Linear Regression
#'
#' @param data data
#' @param y name for y
#' @param x name for x
#' @param cut.numb number of cutoff points
#' @param n.per the least percentage of the smaller group comprised in all patients
#' @param p.cut cutoff of p value, default is 0.05
#' @param strict logical. TRUE means significant differences for each group
#'     combination were considered. FALSE means considering for any combination
#' @param include direction of cutoff point. Any left letter of lower or upper
#' @param round digital. Default is 2
#'
#' @return a dataframe contains cutoff points value, subject numbers in each group,
#'     dumb variable, beta of regression and p value.
#' @export
#'
#' @examples
#' linear(data=mtcars,y='qsec',x='disp',
#'        cut.numb=2,
#'        n.per=0.25)
#'
#' linear(data=mtcars,y='qsec',x='disp',
#'        cut.numb=2,
#'        n.per=0.25,
#'        p.cut=0.05,
#'        strict=TRUE,
#'        include='low',
#'        round=2)
#' linear(data=mtcars,y='qsec',x='disp',
#'        cut.numb=2,
#'        n.per=0.25,
#'        p.cut=0.05,
#'        strict=FALSE,
#'        include='low',
#'        round=2)
linear <- function(data,y,x,
                   cut.numb,
                   n.per,
                   p.cut=0.05,
                   strict=TRUE,
                   include='low',
                   round=2){
    data=delet_na_df(data)
    res.cut.1=get_cutoff(regress = 'linear',
                       data,x,cut.numb,n.per,include,round,
                       y)
    for (i in 1:nrow(res.cut.1)) {
        if (i==1){
            res.cut=res.cut.1
            pair.filt=NULL
        }
        res.cut.i=res.cut[i,1:cut.numb]
        bt=cutit(data[,x],res.cut.i,include)
        #all pairs
        for (j in 1:cut.numb){
            if (j==1) df=NULL
            b=to.refer(bt,j)
            j.coef=coef(summary(lm(data[,y]~b)))[-1,]
            if (!is.matrix(j.coef)){
                j.coef=data.frame(t(j.coef),check.names = F)
                rownames(j.coef)='b2'
            }
            name.j=paste0(rownames(j.coef),collapse = '/')
            beta.j=paste0(digital(j.coef[,1],round),collapse = '/')
            p.j=paste0(digital(j.coef[,4],round),collapse = '/')
            sum.j=sum(j.coef[,4] <= p.cut)
            df=rbind(df,data.frame(dump=name.j,beta=beta.j,
                                   pvalue=p.j,sum=sum.j))
        }
        #judge
        if (strict){
            if (max(df[,4])==cut.numb){
                pair.filt=rbind(pair.filt,df[df[,4]==cut.numb,][1,])
            }else{
                res.cut[i,]=NA
            }
        }else{
            if (any(df[,4] >0 )){
                pair.filt=rbind(pair.filt,df[df[,4]==max(df[,4]),][1,])
            }else{
                res.cut[i,]=NA
            }
        }
    }
    res=na.omit(res.cut)
    if (nrow(res)==0){
        message('No results. Please lower n.per, y.per or cut.numb')
        return(NULL)
    }
    r=cbind(na.omit(res.cut),pair.filt[,-4])
    rownames(r)=NULL
    message('\n3: last combination: ',nrow(r),'\n')
    return(r)
}
