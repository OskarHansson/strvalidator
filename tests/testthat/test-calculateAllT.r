context("calculateAllT")

################################################################################
# CHANGE LOG
# 11.08.2024: Expected values updated as a consequence of updated RNG.
# 10.08.2024: Code updated to correct errors, based on ChatGPT4o.
# 04.07.2023: Updated expected Hosmer-Lemeshow_p values due to a change in
#   the ResourceSelection package 0.3-6, which involves changes to the Hosmer-Lemeshov
#   test because the test degrees of freedom was incorrectly determined for
#   certain small data sets.
# 14.03.2019: Added temporary fix for changed random number generator.
# 13.07.2018: First version.
#
# require(strvalidator)
# require(testthat)
# NB! ResourceSelection is required for this test function.
# require(ResourceSelection)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateAllT.r")
# test_dir("tests/testthat")

test_that("calculateAllT", {
  # Get test data.
  data(set4)
  data(ref4)
  
  # Score dropout.
  kit <- "ESX17"
  
  set.seed(123) # Set random seed for reproducible result on method X.
  dropout <- suppressMessages(calculateDropout(
    data = set4, ref = ref4, kit = kit,
    ignore.case = TRUE
  ))
  # Calculate average peak height.
  dfH <- suppressMessages(calculateHeight(
    data = set4, ref = ref4, na.replace = 0,
    add = FALSE, exclude = "OL", sex.rm = TRUE,
    qs.rm = TRUE, kit = kit,
    ignore.case = TRUE, exact = FALSE
  ))
  
  # Add average peak height to dataset.
  dropout <- suppressMessages(addData(
    data = dropout, new.data = dfH,
    by.col = "Sample.Name", then.by.col = NULL,
    exact = TRUE, ignore.case = TRUE
  ))
  
  # TEST 01 -------------------------------------------------------------------
  # Test that the expected result is calculated.
  
  # Analyse dataframe.
  res <- suppressMessages(
    calculateAllT(
      data = dropout, kit = kit,
      p.dropout = 0.01, p.conservative = 0.05
    )
  )
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected Explanatory_variable was recorded.
  expected_explanatory_variable <- c(
    "Random (Ph)", "LMW (Ph)", "HMW (Ph)", "Locus (Ph)",
    "Random (H)", "LMW (H)", "HMW (H)", "Locus (H)",
    "Random log(Ph)", "LMW log(Ph)", "HMW log(Ph)", "Locus log(Ph)",
    "Random log(H)", "LMW log(H)", "HMW log(H)", "Locus log(H)"
  )
  expect_equal(res[1:16, 1], expected_explanatory_variable)
  
  # Check that expected P(dropout)=0.01@T was calculated.
  expected_p_dropout_01 <- c(
    734, 495, 784, 646,
    547, 517, 513, 533,
    1024, 627, 1080, 897,
    773, 696, 672, 766
  )
  expect_equal(res[1:16, 2], expected_p_dropout_01)
  
  # Check that expected P(dropout>0.01)<0.05@T was calculated.
  expected_p_dropout_01_lt_05 <- c(
    1183, 811, 1514, 901,
    772, 767, 777, 678,
    2398, 1378, 3423, 1597,
    1428, 1381, 1358, 1194
  )
  expect_equal(res[1:16, 3], expected_p_dropout_01_lt_05)
  
  # Check that expected Hosmer-Lemeshow_p was calculated.
  expected_hosmer_lemeshow_p <- c(
    0.9995, 0.9574, 0.9973, 0.9501,
    0.8294, 0.9546, 0.9895, 0.9752,
    0.9944, 0.8312, 0.8982, 0.8022,
    0.7114, 0.9245, 0.9650, 0.8995
  )
  expect_equal(res[1:16, 4], expected_hosmer_lemeshow_p)
  
  # Check that expected beta0 was calculated.
  expected_beta0 <- c(
    -0.2740, 0.2337, -0.9888, 0.9789,
    1.0485, 0.7341, 0.6129, 2.4478,
    7.3939, 9.6917, 5.2677, 11.5748,
    11.4643, 10.8486, 10.7495, 15.5977
  )
  expect_equal(res[1:16, 5], expected_beta0)
  
  # Check that expected beta1 was calculated.
  expected_beta1 <- c(
    -0.0059, -0.0097, -0.0046, -0.0086,
    -0.0103, -0.0103, -0.0102, -0.0132,
    -1.7296, -2.2182, -1.4120, -2.3783,
    -2.4149, -2.3593, -2.3572, -3.0403
  )
  expect_equal(res[1:16, 6], expected_beta1)
  
  # Check that expected observed was calculated.
  expected_observed <- rep(c(378, 382, 383, 367), 4)
  expect_equal(res[1:16, 7], expected_observed)
  
  # Check that expected dropout was calculated.
  expected_dropout <- rep(c(20, 16, 15, 31), 4)
  expect_equal(res[1:16, 8], expected_dropout)
  
  # TEST 02 -------------------------------------------------------------------
  # Test that input data is checked.
  
  # Dataframe with required column names.
  dfNames <- data.frame(
    MethodX = NA, Method1 = NA, Method2 = NA, 
    MethodL = NA, Height = NA, H = NA, MethodL.Ph = NA
  )
  
  # List of tests to ensure that missing columns generate errors.
  required_columns <- c("MethodX", "Method1", "Method2", "MethodL", "Height", "H", "MethodL.Ph")
  
  for (i in seq_along(required_columns)) {
    expect_error(calculateAllT(
      data = dfNames[, -i, drop = FALSE], kit = kit,
      p.dropout = 0.01, p.conservative = 0.05
    ))
  }
  
  # Additional input validation tests
  # kit not in kit definition file should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = "KitNotDefined",
    p.dropout = 0.01, p.conservative = 0.05
  ))
  
  # p.dropout > 1 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = 1.01, p.conservative = 0.05
  ))
  
  # p.dropout < 0 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = -0.01, p.conservative = 0.05
  ))
  
  # length(p.dropout) > 1 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = c(0.01, 0.01), p.conservative = 0.05
  ))
  
  # p.conservative > 1 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = 0.01, p.conservative = 1.05
  ))
  
  # p.conservative < 0 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = 0.01, p.conservative = -0.05
  ))
  
  # length(p.conservative) > 1 should generate an error.
  expect_error(calculateAllT(
    data = dropout, kit = kit,
    p.dropout = 0.01, p.conservative = c(0.05, 0.05)
  ))
})
