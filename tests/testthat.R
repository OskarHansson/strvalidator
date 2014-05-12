################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 24.12.2014: Second try, update to thestthat 0.8.1
# 05.12.2013: Updated to thestthat 0.8


# Load testthat package.
library(testthat)

# Run all tests.
test_check(package="strvalidator")

#  # Run manually:
# library(strvalidator)
# library(testthat)
# test_dir("tests/testthat")