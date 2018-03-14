# onehot 0.1.3

* Fixed examples in the README

# onehot 0.1.2

* Refactored how missing values are predicted.
* Factors can have NA indicator columns.
* Numerics instead use sentinel fields.

# onehot 0.1.1

* Added a `NEWS.md` file to track changes to the package.

* Initial Release

# onehot 0.1.1.9000

* Fixed a bug in onehot.summary switch statement. Using proper default now.
* Added support for predicting a sparse matrix.
* Added multicore support for predicting dense matrices using openMP.
* New argument, addNA, adds indicator columns for all input columns.