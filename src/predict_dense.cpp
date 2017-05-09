#include <Rcpp.h>
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
