#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
using namespace Rcpp;

#define PUSH(i, j, x) i_vec->push_back(i); j_vec->push_back(j); x_vec->push_back(x);

// [[Rcpp::export]]
List predict_onehot_sparse(List onehot, DataFrame df) {

  int addNA = INTEGER(onehot.attr("addNA"))[0];

  CharacterVector type;
  int pos = 0;
  int _j = 0;
  double _val = 0.0;

  std::vector<int> * i_vec = new std::vector<int>;
  std::vector<int> * j_vec = new std::vector<int>;
  std::vector<double> * x_vec = new std::vector<double>;

  for (int i = 0; i < onehot.size(); i++) {
    type = as<CharacterVector>(as<List>(onehot[i])[1]);

    if (type[0] == "factor") {

      CharacterVector lvls = as<CharacterVector>(as<List>(onehot[i])[2]);
      IntegerVector v(df[i]);

      for (int row = 0; row < v.size(); row++) {
        _j = addNA ? ((v[row] == NA_INTEGER) ? lvls.size() + pos : v[row] + pos - 1) : v[row] + pos - 1;
        PUSH(row, _j, 1.0)
      }

      pos += lvls.size() + addNA;

    } if (type[0] == "integer") {

      IntegerVector v(df[i]);

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _j = addNA ? ((v[row] == NA_INTEGER) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_INTEGER) ? 1.0 : (double) v[row]) : (double) v[row];
          PUSH(row, _j, _val)
        }

      }
      pos += 1 + addNA;

    } if (type[0] == "numeric") {

      NumericVector v(df[i]);

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _j = addNA ? (ISNA(v[row]) ? pos+1 : pos) : pos;
          _val = addNA ? (ISNA(v[row]) ? 1.0 : (double) v[row]) : (double) v[row];
          PUSH(row, _j, _val)
        }

      }
      pos += 1 + addNA;
    } if (type[0] == "logical") {

      LogicalVector v(df[i]);

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _j = addNA ? ((v[row] == NA_LOGICAL) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_LOGICAL) ? 1.0 : v[row]) : v[row];
          PUSH(row, _j, _val)
        }

      }
      pos += 1 + addNA;
    }

  } // end loop over onehot

  // load into R vectors
  IntegerVector i_out(i_vec->begin(), i_vec->end());
  delete i_vec;

  IntegerVector j_out(j_vec->begin(), j_vec->end());
  delete j_vec;

  NumericVector x_out(x_vec->begin(), x_vec->end());
  delete x_vec;

  List ll(3);
  ll["i"] = i_out + 1;
  ll["j"] = j_out + 1;
  ll["x"] = x_out;

  return ll;
}
