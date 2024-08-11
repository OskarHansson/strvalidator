context("addSize")

################################################################################
# CHANGE LOG
# 08.08.2024: Modernized the code.
# 22.03.2019: Changed deprecated 'matches' to 'expect_match'.
# 26.08.2014: Added test for scrambled markers (test 05 and test 06) [Issue#5].
# 07.05.2014: Added test for 'ESX17' (test 03 and test 04).
# 02.03.2014: First tests for 'addSize'.
#
# test_dir("inst/tests/")
# test_file("tests/testthat/test-addSize.r")
# test_dir("tests/testthat")

test_that("addSize", {
  # Load test data.
  data(set2)
  data(ref4)
  
  # Correct marker order.
  scrambled <- rbind(set2[7:8, ], set2[1:6, ], set2[9:16, ])
  
  # Get kit information for 'bins=TRUE'.
  kitBins <- getKit("SGMPlus", what = "Size")
  
  # Get kit information for 'bins=FALSE'.
  kitCalc <- getKit("SGMPlus", what = "Offset")
  
  # Extract one sample.
  ref4 <- ref4[ref4$Sample.Name == "A2", ]
  
  # Get kit information for 'bins=TRUE'.
  kitBins2 <- getKit("ESX17", what = "Size")
  
  # Get kit information for 'bins=FALSE'.
  kitCalc2 <- getKit("ESX17", what = "Offset")
  
  # TEST 01 -------------------------------------------------------------------
  # Test adding size using bins=TRUE.
  
  # Analyse dataframe.
  res <- addSize(data = set2, kit = kitBins, bins = TRUE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))
  expect_false(any(is.na(res$Dye)))
  expect_true(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_bins_true <- c(
    126, 138, 169, 258, 266, 305, 107, 113, 144, 211, NA, 301, 
    130, 173, 189, 247, 126, 138, 169, 258, 266, 305, 107, 113, 
    144, 211, NA, 301, 130, 173, 189, NA
  )
  expect_equal(res$Size, expected_sizes_bins_true, tolerance = 1e-5)
  
  # TEST 02 -------------------------------------------------------------------
  # Test adding size using bins=FALSE.
  
  # Analyse dataframe.
  res <- addSize(data = set2, kit = kitCalc, bins = FALSE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))
  expect_false(any(is.na(res$Dye)))
  expect_true(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_bins_false <- c(
    126, 138, 169, 258, 266, 305, 107, 116, 144, 211, 363, 301, 
    130, 173, 189, 247, 126, 138, 169, 258, 266, 305, 107, 116, 
    144, 211, 363, 301, 130, 173, 189, NA
  )
  expect_equal(res$Size, expected_sizes_bins_false, tolerance = 1e-5)
  
  # TEST 03 -------------------------------------------------------------------
  # Test adding size using bins=TRUE with ref4 data.
  
  # Analyse dataframe.
  res <- addSize(data = ref4, kit = kitBins2, bins = TRUE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_ref4_bins_true <- c(
    81.79, 81.79, 122.88, 126.97, 168.62, 168.62, 215.03, 219.03, 
    311.67, 315.56, 101.28, 105.42, 146.92, 159.18, 227.63, 247.60, 
    289.13, 301.08, 90.14, 105.15, 153.06, 161.12, 215.57, 227.59, 
    291.31, 306.69, 103.29, 112.67, 152.82, 169.06, 218.83, 226.77, 
    321.53, 368.23
  )
  expect_equal(res$Size, expected_sizes_ref4_bins_true, tolerance = 1e-5)
  
  # TEST 04 -------------------------------------------------------------------
  # Test adding size using bins=FALSE with ref4 data.
  
  # Analyse dataframe.
  res <- addSize(data = ref4, kit = kitCalc2, bins = FALSE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_ref4_bins_false <- c(
    82, 82, 122, 126, 169, 169, 215, 219, 
    313, 317, 101, 105, 147, 159, 228, 248, 
    289, 301, 90, 105, 153, 161, 216, 228, 
    292, 308, 103, 112, 153, 169, 219, 227, 
    321, 367
  )
  expect_equal(res$Size, expected_sizes_ref4_bins_false, tolerance = 1e-5)
  
  # TEST 05 -------------------------------------------------------------------
  # Test adding size when marker order is not correct and bins=TRUE.
  
  # Analyse dataframe.
  res <- addSize(data = scrambled, kit = kitBins, bins = TRUE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Dye)))
  expect_true(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_scrambled_bins_true <- c(
    107, 113, 126, 138, 169, 258, 266, 305, 
    144, 211, NA, 301, 130, 173, 189, 247
  )
  expect_equal(res$Size, expected_sizes_scrambled_bins_true, tolerance = 1e-5)
  
  # TEST 06 -------------------------------------------------------------------
  # Test adding size when marker order is not correct and bins=FALSE.
  
  # Analyse dataframe.
  res <- addSize(data = scrambled, kit = kitCalc, bins = FALSE)
  
  # Check return class.
  expect_s3_class(res, "data.frame")
  
  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Size" %in% names(res))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Size)))
  
  # Check result.
  expected_sizes_scrambled_bins_false <- c(
    107, 116, 126, 138, 169, 258, 266, 305, 
    144, 211, 363, 301, 130, 173, 189, 247
  )
  expect_equal(res$Size, expected_sizes_scrambled_bins_false, tolerance = 1e-5)
})
