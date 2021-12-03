#'cov4vec
#'
#'Gets different type of correlation, p-value, test statistics given two vectors
#'
#'@param x input vector
#'
#'@param y input vector, with the same dimension of x
#'
#'@param method a character string that indicates specific form of correlation, 
#'either pearson, kendall or spearman
#'
#'@param stat_test input of TRUE or FASLE, to indicate the program whether
#' to perform a test of significance and report p-value
#'
#'@return a list of combination of correlation information and value given x and y
#'
#'@examples
#'pearson_cor(c(1,2,3,4,5),c(2,1,0,3,4),method="pearson",stat_test=FALSE) # $correlation 0.6 
#'$method "pearson"
#'
#'@export
#'


pearson_cor<-function(x,y,method,stat_test){ #21
  n=length(x)
  mean_x=mean(x)
  mean_y=mean(y)
  num=sum((x-mean_x)*(y-mean_y))
  denom=sqrt(sum((x-mean_x)*(x-mean_x))*sum((y-mean_y)*(y-mean_y)))
  rho=num/denom
  if(stat_test==FALSE){
    RVAL<-list(correlation=rho,
               method="pearson"
    )
    return(RVAL)
  }
  else{
    df=n-2
    t_stat = sqrt(df) * rho / sqrt(1 - rho^2)
    p_value = 2*pt(-abs(t_stat),df)
    RVAL <- list(correlation = rho,
                 statistic = round(t_stat,4),
                 df = df,
                 p.value = round(as.numeric(p_value),2),
                 method ="pearson"
    )
    return(RVAL)
  }
}