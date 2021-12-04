#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
using namespace Rcpp;


// [[Rcpp::export]]
int timesTwo(int x) {
  return x * 2;
}