#'cov4vec
#'
#'Gets different type of correlation, p-value, test statistics given two vectors
#'
#'@param x input value 
#'
#'@param y input value
#'
#'@param method input string
#'
#'@param stat_test input 
#'
#'@return the correlation of x and y
#'
#'@examples
#'pearson_cor(c(1,2),c(2,1),method="pearson",stat_test=TRUE)
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