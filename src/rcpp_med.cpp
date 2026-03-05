#include <Rcpp.h>
using namespace Rcpp;

// Source - https://stackoverflow.com/a/34771504
// Posted by nrussell, modified by community. See post 'Timeline' for change history
// Retrieved 2026-02-06, License - CC BY-SA 3.0

// [[Rcpp::interfaces(cpp)]]

//' @title cpp_med
//'
//' @export

// [[Rcpp::export]]
double cpp_med(Rcpp::NumericVector x){
  std::size_t size = x.size();
  std::sort(x.begin(), x.end());
  if (size  % 2 == 0) return (x[size / 2 - 1] + x[size / 2]) / 2.0;
  return x[size / 2];
}

