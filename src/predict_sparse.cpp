#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
using namespace Rcpp;

#define PUSH(i, j, x) i_vec.push_back(i+1); j_vec.push_back(j+1); x_vec.push_back(x);

// [[Rcpp::export]]
List predict_onehot_sparse(List onehot, DataFrame df) {

  int addNA = INTEGER(onehot.attr("addNA"))[0];
  size_t nrows = df.nrow();

  // load encoder types in vector
  std::vector< std::string > types;
  for (int i = 0; i < onehot.size(); i++) {
    types.push_back((char *) as<CharacterVector>(as<List>(onehot[i])[1])[0]);
  }

  int I = 0;
  int j = 0;
  double x = 0.0;

  std::vector<int> i_vec;
  std::vector<int> j_vec;
  std::vector<double> x_vec;

  for (size_t i = 0; i < onehot.size(); i++) {

    if (types[i] == "factor") {

      int nl = LENGTH(as<CharacterVector>(as<List>(onehot[i])[2]));
      int * v = INTEGER(df[i]);

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

      for (size_t r = 0; r < nrows; r++) {

        if (v[r] != 0) {
          j = addNA ? (ISNA(v[r]) ? I+1 : I) : I;
          x = addNA ? (ISNA(v[r]) ? 1.0 : v[r]) : v[r];
          PUSH(r, j, x)
        }
      }
      I += 1 + addNA;

    } if (types[i] == "logical") {

      int * v = INTEGER(df[i]);

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

  List ll(3);
  ll["i"] = wrap(i_vec);
  ll["j"] = wrap(j_vec);
  ll["x"] = wrap(x_vec);

  return ll;
}
