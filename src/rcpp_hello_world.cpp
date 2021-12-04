#include <otherPackage.h>
#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>


using namespace Rcpp;


// [[Rcpp::export]]

double kendall(std::vector<double> x, std::vector<double> y){
  int sum=0;
  std::vector<int> ti(x.size(),1);
  std::vector<int> ui(x.size(),1);
  int c=0,d=0;
  for(int i=0;i<x.size();i++){
    for(int j=i+1;j<x.size();j++){
      if((x[i]>x[j] && y[i]>y[j]) || (x[i]<x[j] && y[i]<y[j])){
        c++;
      }else if((x[i]>x[j] && y[i]<y[j]) || (x[i]<x[j] && y[i]>y[j])){
        d++;
      }
      if(x[i]==x[j]){
        ti[i]++;
      }
      if(y[i]==y[j]){
        ui[i]++;
      }
    }
  }
  int n1=0;
  int n2=0;
  for(int i=0;i<ti.size();i++){
    n1+=ti[i]*(ti[i]-1)/2;
  }
  for(int i=0;i<ui.size();i++){
    n2+=ui[i]*(ui[i]-1)/2;
  }
  int nn=(x.size()*x.size()-x.size())/2;
  std::cout<<ti.size()<<std::endl;
  std::cout<<"n1="<<n1<<" n2="<<n2<<std::endl;
  int m=std::min(c,d);
  return (double)(c-d)/(std::sqrt(nn-n1)*std::sqrt(nn-n2));
  //return (double)(c-d)/(nn*nn*(m-1)/(double)m)*2;
}
