#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
using namespace Rcpp;

#define DEBUG

#define PUSH(r, j, x) REAL(m)[(r) +  (j) * nrows] = (x);

int calculate_column_size(List onehot, std::vector<std::string> types, int addNA) {

  int ncols = 0;
  for (unsigned i = 0; i < types.size(); i++) {

    if (types[i] == "factor") {
      CharacterVector lvls = as<CharacterVector>(as<List>(onehot[i])[2]);
      ncols += lvls.size() + addNA;
    } else if (types[i] == "numeric" || types[i] == "integer" || types[i] == "logical") {
      ncols += 1 + addNA;
    }
  }

  return ncols;
}


// [[Rcpp::export]]
NumericMatrix predict_onehot_dense(List onehot, DataFrame df) {

  int addNA = INTEGER(onehot.attr("addNA"))[0];

  // load encoder types in vector
  std::vector< std::string > types;
  for (unsigned i = 0; i < onehot.size(); i++) {
    types.push_back((char *) as<CharacterVector>(as<List>(onehot[i])[1])[0]);
  }

  // allocate space for matrix
  size_t ncols = calculate_column_size(onehot, types, addNA);
  size_t nrows = df.nrow();
  SEXP m = PROTECT(Rf_allocMatrix(REALSXP, nrows, ncols));

  if (m == NULL) {
    Rcpp::stop("Could not allocate matrix of size %d\n", nrows * ncols);
  }

  //Rprintf("Matrix dims: (%d,%d)\n", nrows, ncols);

  // fill with zeros
  #pragma omp parallel for
  for (size_t i = 0; i < nrows * ncols; i++) {
    REAL(m)[i] = 0;
  }

#ifdef DEBUG

  int I = 0; // column position
  int j = 0; // column to assign
  double x = 0.0; // value to assign

  // loop over and fill the matrix
  for (int i = 0; i < onehot.size(); i++) {

    if (types[i] == "factor") {

      int nl = LENGTH(as<CharacterVector>(as<List>(onehot[i])[2]));
      int * v = INTEGER(df[i]);

      #pragma omp parallel for
      for (size_t r = 0; r < nrows; r++) {
        if (addNA) {
          j = (v[r] == NA_INTEGER) ? nl + I : v[r] + I-1;
          PUSH(r, j, 1.0)
        } else if (v[r] != NA_INTEGER) {
          PUSH(r, v[r] + I-1, 1.0)
        }
      }

      I += nl + addNA;

    } if (types[i] == "integer") {

      int * v = INTEGER(df[i]);

      #pragma omp parallel for
      for (size_t r = 0; r < nrows; r++) {

        if (v[r] != 0) {
          j = addNA ? ((v[r] == NA_INTEGER) ? I+1 : I) : I;
          x = addNA ? ((v[r] == NA_INTEGER) ? 1.0 : v[r]) : v[r];
          PUSH(r, j, x)
        }

      }
      I += 1 + addNA;

    } if (types[i] == "numeric") {

      double * v = REAL(df[i]);

      #pragma omp parallel for
      for (size_t r = 0; r < nrows; r++) {

        if (v[r] != 0) {
          j = addNA ? (ISNA(v[r]) ? I+1 : I) : I;
          x = addNA ? (ISNA(v[r]) ? 1.0 : v[r]) : v[r];
          PUSH(r, j, x)
        }

      }
      I += 1 + addNA;
    } if (types[i] == "logical") {

      int * v = LOGICAL(df[i]);

      #pragma omp parallel for
      for (size_t r = 0; r < nrows; r++) {

        if (v[r] != 0) {
          j = addNA ? ((v[r] == NA_LOGICAL) ? I+1 : I) : I;
          x = addNA ? ((v[r] == NA_LOGICAL) ? 1.0 : v[r]) : v[r];
          PUSH(r, j, x)
        }

      }
      I += 1 + addNA;
    }

  } // end loop over onehot

#endif
  UNPROTECT(1);
  return m;
}