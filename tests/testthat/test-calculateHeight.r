context("calculateHeight")

################################################################################
# CHANGE LOG
# 21.11.2021: Fixed unique sample names in df7.
# 21.11.2021: Changed partial match argument "na" to full name "na.replace".
# 22.03.2019: Changed deprecated 'matches' to 'expect_match'.
# 16.09.2016: Updated with test 21-22 for complete negative samples, updated test 16.
# 15.09.2016: Updated with test 20 for homozygous double notation.
# 19.08.2016: Updated as a consequence of a bug fix.
# 15.08.2016: Updated to match re-written function. Added additional tests.
# 12.10.2014: Added test for NA in Allele column (test 14 and 15).
# 10.09.2014: Added test for parameter 'exclude' (test 12 and 13).
# 10.09.2014: Changed test for column name from grepl to %in% to handle partial matches.
# 10.09.2014: Added test for new functionality (TPH - total peak height).
# 10.09.2014: Function changed name from calculateH to calculateHeight.
# 04.03.2014: Added test for no NA and na!=NULL.
# 25.02.2014: Updated test to change in 'calculateH'. Added more tests.
#
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateHeight.r")
# test_dir("tests/testthat")

test_that("calculateHeight", {

  # Generate test data.
  markers <- c("D3", "vWA", "D16", "D2", "AMEL", "D8", "D21", "D18", "D19", "TH01", "FGA")
  height.1 <- c(3349, 1273, 627, 77, 7189, 3303, 582, 175, 2854, 1217, 460)
  height.2 <- c(2296, 1470, 377, NA, NA, 3026, 737, 174, 2547, NA, 355)

  df1 <- data.frame(
    Sample.Name = "MySample",
    Marker = markers,
    Height.1 = height.1,
    Height.2 = height.2,
    stringsAsFactors = FALSE
  )

  df1 <- slim(
    data = df1,
    fix = c("Sample.Name", "Marker"),
    stack = c("Height")
  )

  # Generate test data.
  markers <- c("D3", "vWA", "D16", "D2", "AMEL", "D8", "D21", "D18", "D19", "TH01", "FGA")
  height.1 <- c(3349, 1273, 627, 77, 7189, 3303, 582, 175, 3416, 1217, 460)
  height.2 <- c(2296, 1470, 377, NA, NA, 3026, 737, 174, NA, NA, 355)

  df2 <- data.frame(
    Sample.Name = "AnotherSample",
    Marker = markers,
    Height.1 = height.1,
    Height.2 = height.2,
    stringsAsFactors = FALSE
  )

  df2 <- slim(
    data = df2,
    fix = c("Sample.Name", "Marker"),
    stack = c("Height")
  )

  df2 <- rbind(df1, df2)

  # One negative sample.
  df3 <- df2
  df3[df3$Sample.Name == "AnotherSample", ]$Height <- as.numeric(NA)


  # Off-ladder alleles "OL".
  df4 <- df1
  df4$Allele <- c(
    "10", "11", "12", "13", "14", "15", "OL", "X", "18", "19", "20",
    "21", "22", "OL", "24", "25", "26", "27", "28"
  )

  # Create reference dataset.
  ref4 <- df4[df4$Allele != "OL", ]

  # Create reference dataset with double notation for homozygotes.
  ref5 <- rbind(ref4[1:7, ], ref4[7:12, ], ref4[12:15, ], ref4[15:17, ])

  # Introduce an NA allele.
  df5 <- df4
  df5$Allele[17] <- NA
  df5$Height[17] <- NA

  # A single negative sample.
  df6 <- df1
  df6$Height <- NA
  df6$Allele <- NA

  # One result and one negative sample.
  df7 <- df6
  df7$Sample.Name <- "AnotherSample"
  df7 <- rbind(df7, df4)

  # Check correct formula, division by expected alleles.
  df8 <- ref4
  df8$Sample.Name <- "MySample2"
  df8 <- rbind(ref4, df8)
  df8$Height <- NA
  df8[df8$Sample.Name == "MySample2" & df8$Marker == "AMEL", ]$Height <- 200
  df8[df8$Sample.Name == "MySample", ]$Height <- 100
  df8[df8$Sample.Name == "MySample" & df8$Marker == "AMEL", ]$Height <- 200
  df8[df8$Sample.Name == "MySample" & df8$Marker == "D18", ]$Height <- 200
  df8[df8$Sample.Name == "MySample" & df8$Marker == "TH01", ]$Height <- 200

  # TEST 01 -------------------------------------------------------------------
  # Test that analysis of one sample works.

  # Analyse dataframe.
  res <- calculateHeight(data = df1, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH, equals(32088))
  expect_that(res$Peaks, equals(19))

  # TEST 02 -------------------------------------------------------------------
  # Test that analysis of a dataset with multiple samples works.

  # Analyse dataframe.
  res <- calculateHeight(data = df2, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH[1], equals(32088))
  expect_that(res$TPH[2], equals(30103))
  expect_that(res$Peaks[1], equals(19))
  expect_that(res$Peaks[2], equals(18))

  # TEST 03 -------------------------------------------------------------------
  # Test that analysis of a dataset including negative samples work.

  # Analyse dataframe.
  res <- calculateHeight(data = df3, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH[1], equals(32088))
  expect_that(res$TPH[2], equals(0))
  expect_that(res$Peaks[1], equals(19))
  expect_that(res$Peaks[2], equals(0))

  # TEST 04 -------------------------------------------------------------------
  # Test that analysis of a dataset with negative samples work,
  # with replacement of NA.

  # Analyse dataframe.
  res <- calculateHeight(data = df3, na.replace = 0, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH[1], equals(32088))
  expect_that(res$TPH[2], equals(0))
  expect_that(res$Peaks[1], equals(19))
  expect_that(res$Peaks[2], equals(0))

  # TEST 05 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  # Add to dataframe.

  # Analyse dataframe.
  res <- calculateHeight(data = df1, na.replace = NULL, add = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df1$Sample.Name))
  expect_that(res$Marker, equals(df1$Marker))
  expect_that(res$Height, equals(as.numeric(df1$Height)))
  expect_that(unique(res$TPH), equals(32088))
  expect_that(unique(res$Peaks), equals(19))

  # TEST 06 -------------------------------------------------------------------
  # Test that analysis of a dataset with multiple samples works.
  # Add to dataframe.

  # Analyse dataframe.
  res <- calculateHeight(data = df2, na.replace = NULL, add = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df2$Sample.Name))
  expect_that(res$Marker, equals(df2$Marker))
  expect_that(res$Height, equals(as.numeric(df2$Height)))
  expect_that(unique(res$TPH)[1], equals(32088))
  expect_that(unique(res$TPH)[2], equals(30103))
  expect_that(unique(res$Peaks)[1], equals(19))
  expect_that(unique(res$Peaks)[2], equals(18))

  # TEST 07 -------------------------------------------------------------------
  # Test that analysis of a dataset including negative samples work.
  # Add to dataframe.

  # Analyse dataframe.
  res <- calculateHeight(data = df3, na.replace = NULL, add = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Height)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df3$Sample.Name))
  expect_that(res$Marker, equals(df3$Marker))
  expect_that(res$Height, equals(as.numeric(df3$Height)))
  expect_that(unique(res$TPH)[1], equals(32088))
  expect_that(unique(res$TPH)[2], equals(0))
  expect_that(unique(res$Peaks)[1], equals(19))
  expect_that(unique(res$Peaks)[2], equals(0))

  # TEST 08 -------------------------------------------------------------------
  # Test that analysis of a dataset including negative samples work,
  # with replacement of NA.
  # Add to dataframe.

  # Analyse dataframe.
  res <- calculateHeight(data = df3, na.replace = 0, add = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Height)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df3$Sample.Name))
  expect_that(res$Marker, equals(df3$Marker))
  expect_that(res$Height, equals(as.numeric(df3$Height)))
  expect_that(unique(res$TPH)[1], equals(32088))
  expect_that(unique(res$TPH)[2], equals(0))
  expect_that(unique(res$Peaks)[1], equals(19))
  expect_that(unique(res$Peaks)[2], equals(0))

  # TEST 09 -------------------------------------------------------------------
  # Test that analysis work when no NA and na.replace!=NULL.

  # Analyse dataframe.
  res <- calculateHeight(data = df1, na.replace = 0, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH, equals(32088))
  expect_that(res$Peaks, equals(19))

  # TEST 10 -------------------------------------------------------------------
  # Test that analysis of one sample with off-ladder alleles work.

  # Analyse dataframe.
  res <- calculateHeight(data = df4, na.replace = NULL, add = FALSE, exclude = "OL")

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH, equals(31837))
  expect_that(res$Peaks, equals(17))

  # TEST 11 -------------------------------------------------------------------
  # Test that analysis of one sample with vector as exclude work.

  # Analyse dataframe.
  res <- calculateHeight(data = df4, na.replace = NULL, add = TRUE, exclude = c("X", "OL"))

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df4$Sample.Name[c(-7, -8, -14)]))
  expect_that(res$Marker, equals(df4$Marker[c(-7, -8, -14)]))
  expect_that(res$Height, equals(as.numeric(df4$Height[c(-7, -8, -14)])))
  expect_that(unique(res$TPH), equals(24648))
  expect_that(unique(res$Peaks), equals(16))

  # TEST 12 -------------------------------------------------------------------
  # Test that analysis of one sample with off-ladder alleles work + NA allele.

  # Analyse dataframe.
  res <- calculateHeight(data = df5, na.replace = NULL, add = FALSE, exclude = "OL")

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$TPH, equals(30620))
  expect_that(res$Peaks, equals(16))

  # TEST 13 -------------------------------------------------------------------
  # Test that analysis of one sample with vector as exclude work + NA allele.

  # Analyse dataframe.
  res <- calculateHeight(data = df5, na.replace = NULL, add = TRUE, exclude = c("X", "OL"))

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("Peaks" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Height)))
  expect_true(any(is.na(res$Allele)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df5$Sample.Name[c(-7, -8, -14)]))
  expect_that(res$Marker, equals(df5$Marker[c(-7, -8, -14)]))
  expect_that(res$Height, equals(as.numeric(df5$Height[c(-7, -8, -14)])))
  expect_that(unique(res$TPH), equals(23431))
  expect_that(unique(res$Peaks), equals(15))

  # TEST 14 -------------------------------------------------------------------
  # Test that analysis of one sample with reference works.

  # Analyse dataframe.
  res <- calculateHeight(data = df4, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(31837))
  expect_that(round(res$H, 3), equals(1591.85))
  expect_that(res$Peaks, equals(17))
  expect_that(res$Expected, equals(17))
  expect_that(res$Proportion, equals(1))

  # TEST 15 -------------------------------------------------------------------
  # Test that analysis of a dataset with reference works.
  # Includes NA.

  # Analyse dataframe.
  res <- calculateHeight(data = df5, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(30620))
  expect_that(round(res$H, 3), equals(1701.111))
  expect_that(res$Peaks, equals(16))
  expect_that(res$Expected, equals(17))
  expect_that(round(res$Proportion, 3), equals(0.941))

  # TEST 16 -------------------------------------------------------------------
  # Test that analysis of a dataset with only a negative sample work.

  # Analyse dataframe.
  res <- calculateHeight(data = df6, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that an empty dataset was returned.
  # Updated 16.09.2016. No longer remove NA samples.
  expect_false(nrow(res) == 0)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_true(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(0))
  expect_that(round(res$H, 3), equals(NaN))
  expect_that(res$Peaks, equals(0))
  expect_that(res$Expected, equals(17))
  expect_that(round(res$Proportion, 3), equals(0))

  # TEST 17 -------------------------------------------------------------------
  # Test that analysis of a dataset with 1 result and 1 negative sample work.

  # Analyse dataframe.
  res <- calculateHeight(data = df7, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(31837))
  expect_that(round(res$H, 3), equals(1591.85))
  expect_that(res$Peaks, equals(17))
  expect_that(res$Expected, equals(17))
  expect_that(round(res$Proportion, 3), equals(1))

  # TEST 18 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  # Add to dataframe.

  # Analyse dataframe.
  res <- calculateHeight(data = df5, ref = ref4, na.replace = NULL, add = TRUE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(unique(res$TPH), equals(30620))
  expect_that(unique(round(res$H, 3)), equals(1701.111))
  expect_that(unique(res$Peaks), equals(16))
  expect_that(unique(res$Expected), equals(17))
  expect_that(unique(round(res$Proportion, 3)), equals(0.941))

  # TEST 19 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  # Add to dataframe.
  # Remove sex markers.

  # Analyse dataframe.
  res <- calculateHeight(
    data = df5, ref = ref4, na.replace = NULL, add = TRUE,
    sex.rm = TRUE, kit = "SGMPlus"
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(unique(res$TPH), equals(23431))
  expect_that(unique(round(res$H, 3)), equals(1464.438))
  expect_that(unique(res$Peaks), equals(15))
  expect_that(unique(res$Expected), equals(16))
  expect_that(unique(round(res$Proportion, 3)), equals(0.938))

  # TEST 20 -------------------------------------------------------------------
  # Test that analysis of one sample works,
  # with double notation for homozygotes in reference.

  # Analyse dataframe.
  res <- calculateHeight(data = df4, ref = ref5, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(31837))
  expect_that(round(res$H, 3), equals(1591.85))
  expect_that(res$Peaks, equals(17))
  expect_that(res$Expected, equals(17))
  expect_that(res$Proportion, equals(1))

  # TEST 21 -------------------------------------------------------------------
  # Test that analysis of a dataset with only a negative sample work,
  # with reference and replacement of NA.

  # Analyse dataframe.
  res <- calculateHeight(data = df6, ref = ref4, na.replace = 0, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(0))
  expect_that(res$H, equals(0))
  expect_that(res$Peaks, equals(0))
  expect_that(res$Expected, equals(17))
  expect_that(res$Proportion, equals(0))

  # TEST 22 -------------------------------------------------------------------
  # Test that analysis of a dataset with only a negative sample work,
  # with reference.

  # Analyse dataframe.
  res <- calculateHeight(data = df6, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_true(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH, equals(0))
  expect_that(res$H, equals(NaN))
  expect_that(res$Peaks, equals(0))
  expect_that(res$Expected, equals(17))
  expect_that(res$Proportion, equals(0))

  # TEST 23 -------------------------------------------------------------------
  # Test correctness of formula, division by expected alleles.
  # A full profile with H=100 and a sample with a single allele H=100/20.

  # Analyse dataframe.
  res <- calculateHeight(data = df8, ref = ref4, na.replace = NULL, add = FALSE)

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("TPH" %in% names(res))
  expect_true("H" %in% names(res))
  expect_true("Peaks" %in% names(res))
  expect_true("Expected" %in% names(res))
  expect_true("Proportion" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$TPH)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  expect_false(any(is.na(res$Expected)))
  expect_false(any(is.na(res$Proportion)))

  # Check result.
  expect_that(res$TPH[1], equals(2000))
  expect_that(res$TPH[2], equals(200))
  expect_that(res$H[1], equals(100))
  expect_that(res$H[2], equals(100))
  expect_that(res$Peaks[1], equals(17))
  expect_that(res$Peaks[2], equals(1))
  expect_that(res$Expected[1], equals(17))
  expect_that(res$Expected[2], equals(17))
  expect_that(res$Proportion[1], equals(17 / 17))
  expect_that(res$Proportion[2], equals(1 / 17))
})
