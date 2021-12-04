#'pearson covariance
#'
#'Gets different type of correlation, p-value, test statistics given two vectors
#'
#'@param x input vector
#'
#'@param y input vector, with the same dimension of x
#'
#'@param method a character string that indicates specific form of correlation, 
#'either pearson, kendall or spearman

#'@return pearson covariance given x and y
#'
#'@examples
#'pearson_cov(c(1,2,3,4,5),c(2,1,0,3,4),method="pearson") 
#'@export


pearson_cov<-function(x,y,method){
  mean_x=mean(x)
  mean_y=mean(y)
  num=sum((x-mean_x)*(y-mean_y))
  denom=length(x)-1
  return(num/denom)
}

#'spearman covariance
#'
#'Gets different type of correlation, p-value, test statistics given two vectors
#'
#'@param x input vector
#'
#'@param y input vector, with the same dimension of x
#'
#'@param method a character string that indicates specific form of correlation, 
#'either pearson, kendall or spearman

#'@return spearman covariance given x and y
#'
#'@examples
#'spearman_cov(c(1,2,3,4,5),c(2,1,0,3,4),method="spearman") 
#'@export

spearman_cov<-function(x,y,method){
  rank_x=rank(x)
  rank_y=rank(y)
  mean_rank_x=mean(rank_x)
  mean_rank_y=mean(rank_y)
  num=sum((rank_x-mean_rank_x)*(rank_y-mean_rank_y))
  denom=length(x)-1
  return(num/denom)
}
