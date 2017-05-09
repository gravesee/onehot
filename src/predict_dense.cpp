#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
using namespace Rcpp;

//#define PUSH(i, j, x) m(i, j) = x;
#define PUSH(i, j, x) REAL(m)[i +  j * df.nrow()] = x;

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


// [[Rcpp::export]]
NumericMatrix predict_onehot_dense(List onehot, DataFrame df) {

  size_t ncols = calculate_column_size(onehot);
  // allocate numeric matrix
  //double * m = (double *) calloc(df.nrow() * ncols, sizeof m);
  SEXP m = PROTECT(Rf_allocMatrix(REALSXP, df.nrow(), ncols));
  //NumericMatrix m(df.nrow(), ncols);

  Rprintf("Dims of matrix (%d,%d)\n", df.nrow(), ncols);

  int addNA = INTEGER(onehot.attr("addNA"))[0];
  CharacterVector type;
  int pos = 0;
  int _j = 0;
  double _val = 0.0;

  // loop over and fill the matrix
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

  UNPROTECT(1);
  return m;
}