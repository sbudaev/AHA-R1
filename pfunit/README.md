# Unit tests

The `pfunit` directory contains Makefiles for unit tests of the model using
the pFUnit framework for Fortran.

* To use unit testing, delete/disable the "main" program from the model code.
  ("main" should be a separate small file.)
* pFUnit is straightforward to install and use on Linux; may be tricky on
  Windows (uses Cygwin).
* pFUnit is obtained [here](http://pfunit.sourceforge.net).
.

**Note:** Unit testing is now rudimentary for evaluation only, uses modified
"simple" examples from pFUnit distribution. Not used so far for anything except
evaluation and testing.
