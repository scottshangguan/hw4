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
#'pearson_cor(c(1,2,3,4,5),c(2,1,0,3,4),method="pearson",stat_test=FALSE) 
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

spearman_cor<-function(x,y,method,stat_test){#23
  
  n=length(x)
  rank_x=rank(x)
  rank_y=rank(y)
  d=rank_x-rank_y
  n=length(x)
  num=6*sum(d*d)
  denom=n*(n^2-1)
  rho=1-num/denom
  if(n<=1290){
    s=sum((rank(x) - rank(y))^2)
  }
  else{
    s=(n^3 - n) * (1 - rho) / 6
  }
  if(stat_test==FALSE){
    RVAL<-list(correlation=rho,
               method="spearman"
    )
    return(RVAL)
  }
  else{
    df=n-2
    p_s <- function(s, n, lower.tail = TRUE) {
      den <- (n*(n^2-1))/6
      r <- 1 - s/den
      pt(r / sqrt((1 - r^2)/(n-2)), df = n-2,
         lower.tail = !lower.tail)
    }
    p <- if(s > (n^3 - n) / 6){
      p_s(s, n, lower.tail = FALSE)
    }else{
      p_s(s, n, lower.tail = TRUE)
    }
    
    
    p_value =   min(2 * p, 1)
    RVAL <- list(correlation = rho,
                 s=s,
                 p.value = round(as.numeric(p_value),2),
                 method ="spearman"
    )
    return(RVAL)
  }
}

cor_fuction<-function(x,y, method = c("pearson", "kendall", "spearman"),stat_test=FALSE){
  
  word_vec=c("correlation","degree of freedom", "t-statistic", "p-value")
  if((is.vector(x)==FALSE) || (is.vector(y)==FALSE))
    stop("Please supply both 'x' and 'y' as vectors")
  if(length(x)!=length(y))
    stop("Please supply both 'x' and 'y' with the same dimension")
  if(length(x)< 3L)
    stop("not enough finite observations")
  if(method=="pearson"){
    return(pearson_cor(x,y,method,stat_test))
  }
  else if(method=="kendall"){
    return(kendall(x,y))
  }
  else if(method=="spearman"){
    return(spearman_cor(x,y,method,stat_test))
  }
}