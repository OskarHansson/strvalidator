context("EPG generation")

################################################################################
# CHANGE LOG
# 22.03.2019: Changed deprecated 'matches' to 'expect_match'.
# 21.04.2016: Fixed old argument name 'keepNA' -> 'keep.na'.
#
# require(strvalidator)
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-heightToPeak.r")
# test_dir("tests/testthat")

test_that("heightToPeak", {
  # Three alleles with sizes and heights.
  peakw <- 1
  mydf <- data.frame(Size = c(100, 120, 130), Height = c(5000, 4500, 4000))
  res <- heightToPeak(data = mydf, width = peakw, keep.na = TRUE)

  # TEST 01--------------------------------------------------------------------
  # Normal data.

  # Test some faulty arguments.
  expect_that(heightToPeak(data = mydf, width = "String"), throws_error())
  expect_that(heightToPeak(data = c(12, 14)), throws_error())
  expect_that(heightToPeak(data = mydf, keep.na = "String"), throws_error())

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check length.
  expect_that(nrow(res), equals(9))

  # Check peak width.
  expect_that(res$Size[3] - res$Size[1], is_equivalent_to(peakw))

  # Check that expected columns exist.
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))


  # TEST 02--------------------------------------------------------------------
  # Data with a missing height and an additional column for sample name.

  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with text as strings (stringsAsFactors=FALSE).
  # Wider peaks.
  name <- "TEST"
  peakw <- 2.5
  mydf <- data.frame(
    Sample.Name = name, Size = c(100, 120, 130),
    Height = c(5000, NA, 4000), stringsAsFactors = FALSE
  )
  res <- heightToPeak(data = mydf, width = peakw, keep.na = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check length.
  expect_that(nrow(res), equals(9))

  # Check sample name (error if returned as factor level).
  expect_that(res$Sample.Name[1], is_equivalent_to(name))

  # Check peak width.
  expect_that(res$Size[3] - res$Size[1], is_equivalent_to(peakw))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))

  # TEST 03--------------------------------------------------------------------
  # keep.na=FALSE and an additional column for numerical sample name.

  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with numerical names (stringsAsFactors=TRUE).
  name <- 123456
  peakw <- 1 / 3
  mydf <- data.frame(Sample.Name = name, Size = c(100, 120, 130), Height = c(5000, NA, NA))
  res <- heightToPeak(data = mydf, width = peakw, keep.na = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check length.
  expect_that(nrow(res), equals(3))

  # Check sample name (error if returned as factor level).
  expect_that(res$Sample.Name[1], is_equivalent_to(name))

  # Check peak width.
  expect_that(res$Size[3] - res$Size[1], is_equivalent_to(peakw))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))


  # TEST 04--------------------------------------------------------------------
  # keep.na=FALSE and an additional column for numerical sample name.
  # Completely negative sample.

  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with numerical names (stringsAsFactors=TRUE).
  name <- 123456
  mydf <- data.frame(Sample.Name = name, Size = c(100, 120, 130), Height = c(NA, NA, NA))
  res <- heightToPeak(data = mydf, keep.na = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check length.
  expect_that(nrow(res), equals(0))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))

  # NEW DATA.
  res <- heightToPeak(data = mydf, keep.na = TRUE)

  # Check length.
  expect_that(nrow(res), equals(9))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
})
