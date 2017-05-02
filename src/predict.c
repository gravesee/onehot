#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

// onehot -- onehot object
// data -- data frame of predictors in same order

#define TYPE(onehot, i) STRING_VALUE(STRING_ELT(VECTOR_ELT(VECTOR_ELT(onehot, i), 1), 0))
#define LEVELS(onehot, i) VECTOR_ELT(VECTOR_ELT(onehot, i), 2)
#define IS_FACTOR(onehot, i) strncmp(TYPE(onehot, i), "factor", 6) == 0
#define NAME(onehot, i) VECTOR_ELT(VECTOR_ELT(onehot, i), 0)

void fill_factor(double * m, SEXP v, int * pos);
void fill_double(double * m, SEXP v, int * pos);
void fill_integer(double * m, SEXP v, int * pos);
void fill_logical(double * m, SEXP v, int * pos);

void fill_factor(double *m, SEXP v, int * pos) {
  for (int i = 0; i < LENGTH(v); i++) {
    if (INTEGER(v)[i] != R_NaInt) {
      m[i + (*pos + (INTEGER(v)[i] - 1)) * LENGTH(v)] = 1;
    }
  }
  *pos += LENGTH(GET_LEVELS(v));
}

void fill_double(double * m, SEXP v, int * pos) {
  for (int i = 0; i < LENGTH(v); i++){
    m[i + *pos * LENGTH(v)] = REAL(v)[i];
  }
  *pos += 1;
}
void fill_integer(double * m, SEXP v, int * pos) {
  for (int i = 0; i < LENGTH(v); i++){
    m[i + *pos * LENGTH(v)] = INTEGER(v)[i];
  }
  *pos += 1;
}
void fill_logical(double * m, SEXP v, int * pos) {
  for (int i = 0; i < LENGTH(v); i++){
    m[i + *pos * LENGTH(v)] = LOGICAL(v)[i];
  }
  *pos += 1;
}

SEXP predict_onehot(SEXP onehot, SEXP data) {

  int ncols = 0;
  int nrows = LENGTH(VECTOR_ELT(data, 0));

  for (int i = 0; i < LENGTH(onehot); i++) {
    if (Rf_isString(VECTOR_ELT(data, i))) break;
    ncols += IS_FACTOR(onehot, i) ? LENGTH(LEVELS(onehot, i)) : 1;
  }

  // allocate matrix and fill with zeros
  SEXP result = PROTECT(Rf_allocMatrix(REALSXP, nrows, ncols));
  for (int i = 0; i < nrows*ncols; i++) REAL(result)[i] = 0;

  double * m = REAL(result); // alias to output matrix

  // fill matrix with values
  int pos = 0;
  for (int i = 0; i < LENGTH(onehot); i++) {
    SEXP v = VECTOR_ELT(data, i);

    if (IS_FACTOR(onehot, i)) {
      fill_factor(m, v, &pos);

    } else if (Rf_isInteger(v)) {
      fill_integer(m, v, &pos);

    } else if (Rf_isReal(v)) {
      fill_double(m, v, &pos);

    } else if (Rf_isLogical(v)) {
      fill_logical(m, v, &pos);
    }
  }
  //Rprintf("Matrix cols: %d\n", ncols);

  UNPROTECT(1);
  return result;

}

#ifdef OS_WINDOWS
R_CallMethodDef callMethods[]  = {
  {"predict_onehot", (DL_FUNC) &predict_onehot, 2},
  {NULL, NULL, 0}
};

void R_init_onehot(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
#endif
