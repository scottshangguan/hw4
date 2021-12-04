#'pearson covariance
#'
#'Gets pearson covariance given two vectors
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
#'Gets spearman covariance given two vectors
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

#'cov_function c
#'
#'Gets pearson or spearman correlation given two vectors
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
#'cov_function(c(1,2,3,4,5),c(2,1,0,3,4),method="spearman") 
#'@export
#'
#'
#'
cov_fuction<-function(x,y,method = c("pearson", "spearman")){
  if(method=="pearson"){
    return(pearson_cov(x,y,method=="pearson"))
  }
  else if(method=="spearman"){
    return(spearman_cor(x,y,method=="spearman"))
  }
}
