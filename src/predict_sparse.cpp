#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
using namespace Rcpp;

int calculate_column_size(List onehot);

int calculate_column_size(List onehot) {

  int ncols = 0;

  int addNA = INTEGER(onehot.attr("addNA"))[0];

  CharacterVector type;
  for (int i = 0; i < onehot.size(); i++) {

    type = as<CharacterVector>(as<List>(onehot[i])[1]);

    if (type[0] == "factor") {
      CharacterVector lvls = as<CharacterVector>(as<List>(onehot[i])[2]);
      ncols += lvls.size() + addNA;
    } else if (type[0] == "character") {
      break;
    } else {
      ncols += 1 + addNA;
    }
  }

  return ncols;
}

typedef struct {
  int i;
  int j;
  double val;
} point;

// [[Rcpp::export]]
List predict_onehot_sparse(List onehot, DataFrame df) {

  //int ncols = calculate_column_size(onehot);
  int addNA = INTEGER(onehot.attr("addNA"))[0];

  std::vector<point> m = std::vector<point>();

  CharacterVector type;
  int pos = 0;
  int _i = 0;
  int _j = 0;
  double _val = 0.0;

  IntegerVector i_test;


  for (int i = 0; i < onehot.size(); i++) {
    type = as<CharacterVector>(as<List>(onehot[i])[1]);

    if (type[0] == "factor") {

      CharacterVector lvls = as<CharacterVector>(as<List>(onehot[i])[2]);
      IntegerVector v = df[i];

      for (int row = 0; row < v.size(); row++) {
        _i = row;
        _j = addNA ? ((v[row] == NA_INTEGER) ? lvls.size() + pos : v[row] + pos - 1) : v[row] + pos - 1;
        _val = 1.0;
        m.push_back((point){.i=_i, .j=_j, .val=_val});

        i_test.push_back(_i);
      }

      pos += lvls.size() + addNA;

    } if (type[0] == "integer") {

      IntegerVector v = df[i];

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _i = row;
          _j = addNA ? ((v[row] == NA_INTEGER) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_INTEGER) ? 1.0 : (double) v[row]) : (double) v[row];
          m.push_back((point){.i=_i, .j=_j, .val=_val});
        }

      }
      pos += 1 + addNA;

    } if (type[0] == "numeric") {

      NumericVector v = df[i];

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _i = row;
          _j = addNA ? (ISNA(v[row]) ? pos+1 : pos) : pos;
          _val = addNA ? (ISNA(v[row]) ? 1.0 : (double) v[row]) : (double) v[row];
          m.push_back((point){.i=_i, .j=_j, .val=_val});
        }

      }
      pos += 1 + addNA;
    } if (type[0] == "logical") {

      LogicalVector v = df[i];

      for (int row = 0; row < v.size(); row++) {

        if (v[row] != 0) {
          _i = row;
          _j = addNA ? ((v[row] == NA_LOGICAL) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_LOGICAL) ? 1.0 : v[row]) : v[row];
          m.push_back((point){.i=_i, .j=_j, .val=_val});
        }

      }
      pos += 1 + addNA;
    }

  } // end loop over onehot

  // load into R vectors
  IntegerVector i_out(m.size());
  IntegerVector j_out(m.size());
  NumericVector x_out(m.size());

  for (size_t i = 0; i < m.size(); i++) {
    i_out[i] = m[i].i + 1;
    j_out[i] = m[i].j + 1;
    x_out[i] = m[i].val;
  }

  List ll(3);
  ll["i"] = i_out;
  ll["j"] = j_out;
  ll["x"] = x_out;
  ll["test"] = i_test;

  return ll;
}