################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 05.12.2013: Updated to thestthat 0.8


library(testthat)
library(strvalidator)

# test_check(package="strvalidator") # Does not work yet?
# test_package=("strvalidator") # No error message, but does not run (although R CDM says OK)
  test_dir("tests/testthat")