#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
using namespace Rcpp;


// [[Rcpp::export]]
SEXP predict_onehot_libsvm(List onehot, DataFrame df, IntegerVector y, CharacterVector outfile) {

  FILE *ofp;
  char * outputFilename = outfile(0);
  ofp = fopen(outputFilename, "w");

  if (ofp == NULL) {
    fprintf(stderr, "Can't open output file %s!\n",  outputFilename);
    exit(1);
  }

  std::setvbuf(ofp, NULL, _IOFBF, 1024 * 1024 * 50);

  int addNA = INTEGER(onehot.attr("addNA"))[0];


  // load types in vector
  std::vector< std::string > types;
  for (int i=0; i < onehot.length(); i++) {
    types.push_back((char *) as<CharacterVector>(as<List>(onehot[i])[1])[0]);
  }

  // loop over the rows
  for (int row = 0; row < df.nrow(); row++) {
    int pos = 0;
    int _j = 0;
    double _val = 0.0;

    fprintf(ofp, "%d", y[row]);

    // loop over the columns
    for (int i = 0; i < onehot.length(); i++) {

      if (types[i] == "factor") {

        CharacterVector lvls = as<CharacterVector>(as<List>(onehot[i])[2]);
        int * v = INTEGER(df[i]);

        _j = addNA ? ((v[row] == NA_INTEGER) ? lvls.size() + pos : v[row] + pos - 1) : v[row] + pos - 1;
        fprintf(ofp, " %d:%d", _j, 1);

        pos += lvls.size() + addNA;

      } if (types[i] == "integer") {

        int * v = INTEGER(df[i]);

        if (v[row] != 0) {
          _j = addNA ? ((v[row] == NA_INTEGER) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_INTEGER) ? 1 : v[row]) : v[row];

          fprintf(ofp, " %d:%d", _j , (int) _val);
        }
        pos += 1 + addNA;

      } if (types[i] == "numeric") {

        double * v = REAL(df[i]);

        if (v[row] != 0) {
          _j = addNA ? (ISNA(v[row]) ? pos+1 : pos) : pos;
          _val = addNA ? (ISNA(v[row]) ? 1.0 : v[row]) : v[row];

          fprintf(ofp, " %d:%0.3f", _j, _val);
        }
        pos += 1 + addNA;

      } if (types[i] == "logical") {

        int * v = LOGICAL(df[i]);

        if (v[row] != 0) {
          _j = addNA ? ((v[row] == NA_LOGICAL) ? pos+1 : pos) : pos;
          _val = addNA ? ((v[row] == NA_LOGICAL) ? 1 : v[row]) : v[row];

          fprintf(ofp, " %d:%d", _j, (int) _val);
         }

      pos += 1 + addNA;
    }
  } // looped over all columns

  fprintf(ofp, "\n");

} // looped over all rows

  fclose(ofp);
  return R_NilValue;
}
