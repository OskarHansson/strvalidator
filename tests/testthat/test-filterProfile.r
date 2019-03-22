context("filterProfile")

################################################################################
# CHANGE LOG
# 22.03.2019: Changed deprecated 'matches' to 'expect_match'.
# 28.08.2016: Updated to work with improved filterProfile.
# 15.12.2015: Added test 10-12 to test new option 'exact'.
# 09.04.2015: Added test 09 to test new option 'invert'.
# 22.01.2014: Fixed test 2 in accordance with change in 'filterProfile'.
# 20.01.2014: All tests working.
#
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-filterProfile.r")
# test_dir("tests/testthat")

test_that("filterProfile", {

  # Load test data.
  data(set1)
  data(ref1)
  data(set2)
  data(ref2)
  data(set7)
  data(ref7)

  # Dataset with double notation (16/16 vs 16).
  set1d <- slim(set1, fix = c("Sample.Name", "Marker"), stack = c("Allele", "Height"))
  ref1d <- slim(ref1, fix = c("Sample.Name", "Marker"), stack = c("Allele"))
  sampleName1 <- ref1d$Sample.Name
  marker1 <- ref1d$Marker
  allele1 <- ref1d$Allele
  # Duplicate homozygous alleles.
  allele1 <- c(allele1[1:19], allele1[19], allele1[20:33])
  marker1 <- c(marker1[1:19], marker1[19], marker1[20:33])
  sampleName1 <- c(sampleName1[1], sampleName1)
  # Make new dataframe.
  ref1d <- data.frame(Sample.Name = sampleName1, Marker = marker1, Allele = allele1, stringsAsFactors = FALSE)

  # Dataset with missing marker.
  set3 <- set2[set2$Marker != "TH01", ]

  # Dataset with different case in sample name.
  set4 <- set2
  set4$Sample.Name[set4$Sample.Name == "SampleA02"] <- "sAmPLea02"

  # Dataset with dirty' data.
  set6 <- slim(set1,
    fix = c("Sample.Name", "Marker"),
    stack = c("Allele", "Height"),
    keep.na = TRUE
  )
  ref6 <- slim(ref1,
    fix = c("Sample.Name", "Marker"),
    stack = c("Allele"),
    keep.na = TRUE
  )

  # Extract a single samples.
  set7 <- set7[set7$Sample.Name == "H01", ]
  ref7 <- ref7[ref7$Sample.Name == "H", ]

  # Dataset without Sample.Name column.
  set8 <- slim(set1, fix = c("Sample.Name", "Marker"), stack = c("Allele", "Height"))
  ref8 <- slim(ref1, fix = c("Sample.Name", "Marker"), stack = c("Allele"))
  ref8 <- ref8[, c("Marker", "Allele")]

  # Dataset testing exact matching.
  # 'PC1' is in 'NOPC15'
  set9 <- slim(set1, fix = c("Sample.Name", "Marker"), stack = c("Allele", "Height"))
  set9 <- trim(data = set9, samples = "PC1|ladder")
  set9[set9$Sample.Name == "Ladder", ]$Sample.Name <- "NOPC15"
  ref9 <- suppressWarnings(guessProfile(data = set9, ol.rm = TRUE))
  ref9$Height <- as.character(ref9$Height) # To enable comparison using 'all.equal'.

  # TEST 01 -------------------------------------------------------------------
  # Test that filtering of two 'clean' samples work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set2, ref = ref2,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))
  expect_true(any(grepl("Dye", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Dye)))

  # Check result.
  expect_that(ncol(res), equals(5))
  expect_that(nrow(res), equals(31))

  # TEST 02 -------------------------------------------------------------------
  # Test that adding missing loci work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set3, ref = ref2,
    add.missing.loci = TRUE,
    keep.na = TRUE,
    ignore.case = FALSE,
    kit = "SGMPlus",
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(6))
  expect_that(nrow(res), equals(30))

  # TEST 03 -------------------------------------------------------------------
  # Test that keep NA loci work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set2, ref = ref2,
    add.missing.loci = FALSE,
    keep.na = TRUE,
    ignore.case = FALSE,
    kit = "SGMPlus",
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(6))
  expect_that(nrow(res), equals(32))

  # TEST 04 -------------------------------------------------------------------
  # Test that ignore case = FALSE work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set2, ref = ref2,
    add.missing.loci = FALSE,
    keep.na = TRUE,
    ignore.case = FALSE,
    kit = "SGMPlus",
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(6))
  expect_that(nrow(res), equals(32))

  # TEST 05 -------------------------------------------------------------------
  # Test that 'fat' data gives error.

  # Test using 'fat' data.
  expect_that(
    filterProfile(
      data = set1, ref = ref2,
      add.missing.loci = FALSE,
      keep.na = TRUE,
      ignore.case = FALSE,
      debug = FALSE
    ),
    throws_error()
  )

  # Test using 'fat' ref.
  expect_that(
    filterProfile(
      data = set2, ref = ref1,
      add.missing.loci = FALSE,
      keep.na = TRUE,
      ignore.case = FALSE,
      debug = FALSE
    ),
    throws_error()
  )

  # Test using 'fat' data and ref.
  expect_that(
    filterProfile(
      data = set1, ref = ref1,
      add.missing.loci = FALSE,
      keep.na = TRUE,
      ignore.case = FALSE,
      debug = FALSE
    ),
    throws_error()
  )

  # TEST 06 -------------------------------------------------------------------
  # Test that filtering of 'dirty' samples work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set6, ref = ref6,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(264))

  # Check success of filtering.
  expect_true(all(unique(res$Allele[res$Marker == "AMEL"]) == c("X", "Y")))
  expect_true(all(unique(res$Allele[res$Marker == "D3S1358"]) == c("17", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "TH01"]) == c("6", "9.3")))
  expect_true(all(unique(res$Allele[res$Marker == "D21S11"]) == c("29", "31.2")))
  expect_true(all(unique(res$Allele[res$Marker == "D18S51"]) == c("16", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "D10S1248"]) == c("13", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "D1S1656"]) == c("12", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S1338"]) == c("22", "25")))
  expect_true(all(unique(res$Allele[res$Marker == "D16S539"]) == c("9", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D22S1045"]) == c("16")))
  expect_true(all(unique(res$Allele[res$Marker == "vWA"]) == c("16", "19")))
  expect_true(all(unique(res$Allele[res$Marker == "D8S1179"]) == c("14", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "FGA"]) == c("20", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S441"]) == c("10", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "D12S391"]) == c("18", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D19S433"]) == c("13", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "SE33"]) == c("15", "16")))

  # TEST 07 -------------------------------------------------------------------
  # Test that filtering using double notation work (16/16 vs 16).

  # Analyse dataframe.
  res <- filterProfile(
    data = set1d, ref = ref1d,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(264))

  # Check success of filtering.
  expect_true(all(unique(res$Allele[res$Marker == "AMEL"]) == c("X", "Y")))
  expect_true(all(unique(res$Allele[res$Marker == "D3S1358"]) == c("17", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "TH01"]) == c("6", "9.3")))
  expect_true(all(unique(res$Allele[res$Marker == "D21S11"]) == c("29", "31.2")))
  expect_true(all(unique(res$Allele[res$Marker == "D18S51"]) == c("16", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "D10S1248"]) == c("13", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "D1S1656"]) == c("12", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S1338"]) == c("22", "25")))
  expect_true(all(unique(res$Allele[res$Marker == "D16S539"]) == c("9", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D22S1045"]) == c("16")))
  expect_true(all(unique(res$Allele[res$Marker == "vWA"]) == c("16", "19")))
  expect_true(all(unique(res$Allele[res$Marker == "D8S1179"]) == c("14", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "FGA"]) == c("20", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S441"]) == c("10", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "D12S391"]) == c("18", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D19S433"]) == c("13", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "SE33"]) == c("15", "16")))

  # TEST 08 -------------------------------------------------------------------
  # Test that filtering of 'dirty' samples without Sample.Name work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set8, ref = ref8,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(270))

  # Check that 'Ladder' is in result.
  expect_true("Ladder" %in% res$Sample.Name)

  # Check success of filtering.
  expect_true(all(unique(res$Allele[res$Marker == "AMEL"]) == c("X", "Y")))
  expect_true(all(unique(res$Allele[res$Marker == "D3S1358"]) == c("17", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "TH01"]) == c("6", "9.3")))
  expect_true(all(unique(res$Allele[res$Marker == "D21S11"]) == c("29", "31.2")))
  expect_true(all(unique(res$Allele[res$Marker == "D18S51"]) == c("16", "18")))
  expect_true(all(unique(res$Allele[res$Marker == "D10S1248"]) == c("13", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "D1S1656"]) == c("12", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S1338"]) == c("22", "25")))
  expect_true(all(unique(res$Allele[res$Marker == "D16S539"]) == c("9", "13")))
  expect_true(all(unique(res$Allele[res$Marker == "D22S1045"]) == c("16")))
  expect_true(all(unique(res$Allele[res$Marker == "vWA"]) == c("16", "19")))
  expect_true(all(unique(res$Allele[res$Marker == "D8S1179"]) == c("14", "15")))
  expect_true(all(unique(res$Allele[res$Marker == "FGA"]) == c("20", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D2S441"]) == c("10", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "D12S391"]) == c("18", "23")))
  expect_true(all(unique(res$Allele[res$Marker == "D19S433"]) == c("13", "14")))
  expect_true(all(unique(res$Allele[res$Marker == "SE33"]) == c("15", "16")))

  # TEST 09 -------------------------------------------------------------------
  # Test that invert filtering of 'dirty' samples work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set6, ref = ref6,
    add.missing.loci = TRUE,
    keep.na = TRUE,
    ignore.case = FALSE,
    invert = TRUE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(240))

  # Check success of filtering.
  expect_false(all(unique(res$Allele[res$Marker == "AMEL"]) %in% c("X", "Y")))
  expect_false(all(unique(res$Allele[res$Marker == "D3S1358"]) %in% c("17", "18")))
  expect_false(all(unique(res$Allele[res$Marker == "TH01"]) %in% c("6", "9.3")))
  expect_false(all(unique(res$Allele[res$Marker == "D21S11"]) %in% c("29", "31.2")))
  expect_false(all(unique(res$Allele[res$Marker == "D18S51"]) %in% c("16", "18")))
  expect_false(all(unique(res$Allele[res$Marker == "D10S1248"]) %in% c("13", "15")))
  expect_false(all(unique(res$Allele[res$Marker == "D1S1656"]) %in% c("12", "13")))
  expect_false(all(unique(res$Allele[res$Marker == "D2S1338"]) %in% c("22", "25")))
  expect_false(all(unique(res$Allele[res$Marker == "D16S539"]) %in% c("9", "13")))
  expect_false(all(unique(res$Allele[res$Marker == "D22S1045"]) %in% c("16")))
  expect_false(all(unique(res$Allele[res$Marker == "vWA"]) %in% c("16", "19")))
  expect_false(all(unique(res$Allele[res$Marker == "D8S1179"]) %in% c("14", "15")))
  expect_false(all(unique(res$Allele[res$Marker == "FGA"]) %in% c("20", "23")))
  expect_false(all(unique(res$Allele[res$Marker == "D2S441"]) %in% c("10", "14")))
  expect_false(all(unique(res$Allele[res$Marker == "D12S391"]) %in% c("18", "23")))
  expect_false(all(unique(res$Allele[res$Marker == "D19S433"]) %in% c("13", "14")))
  expect_false(all(unique(res$Allele[res$Marker == "SE33"]) %in% c("15", "16")))

  # TEST 10 -------------------------------------------------------------------
  # Test that filtering generates incorrect profile in 'fast' mode.

  # Analyse dataframe.
  res <- filterProfile(
    data = set9, ref = ref9,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    exact = FALSE,
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(71))

  # Check if filtered profile equals reference.
  expect_false(is.logical(all.equal(res[res$Sample.Name == "NOPC15", ],
    ref9[ref9$Sample.Name == "NOPC15", ],
    check.attributes = FALSE
  )))

  # TEST 11 -------------------------------------------------------------------
  # Test that filtering generates correct profile in 'fast' mode.

  # Analyse dataframe.
  res <- filterProfile(
    data = set9, ref = ref9,
    add.missing.loci = FALSE,
    keep.na = FALSE,
    ignore.case = FALSE,
    exact = TRUE,
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(66))

  # Check if filtered profile equals reference.
  expect_true(is.logical(all.equal(res[res$Sample.Name == "NOPC15", ],
    ref9[ref9$Sample.Name == "NOPC15", ],
    check.attributes = FALSE
  )))

  # TEST 12 -------------------------------------------------------------------
  # Test that filtering generates incorrect profile in 'slow' mode.

  # Analyse dataframe.
  res <- filterProfile(
    data = set9, ref = ref9,
    add.missing.loci = TRUE,
    keep.na = TRUE,
    ignore.case = FALSE,
    exact = FALSE,
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(105))

  # Check if filtered profile equals reference.
  expect_false(is.logical(all.equal(res[res$Sample.Name == "NOPC15", ],
    ref9[ref9$Sample.Name == "NOPC15", ],
    check.attributes = FALSE
  )))

  # TEST 12 -------------------------------------------------------------------
  # Test that filtering generates correct profile in 'slow' mode.

  # Analyse dataframe.
  res <- filterProfile(
    data = set9, ref = ref9,
    add.missing.loci = TRUE,
    keep.na = TRUE,
    ignore.case = FALSE,
    exact = TRUE,
    debug = FALSE
  )


  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Height", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Height)))

  # Check result.
  expect_that(ncol(res), equals(4))
  expect_that(nrow(res), equals(66))

  # Check if filtered profile equals reference.
  expect_true(is.logical(all.equal(res[res$Sample.Name == "NOPC15", ],
    ref9[ref9$Sample.Name == "NOPC15", ],
    check.attributes = FALSE
  )))


  # TEST 13 -------------------------------------------------------------------
  # Test that filtering of only quality sensors work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set7, ref = ref7, kit = "ESSplexSEQS",
    add.missing.loci = FALSE,
    keep.na = FALSE,
    qs.rm = TRUE,
    sex.rm = FALSE,
    ignore.case = FALSE,
    filter.allele = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Data.Point" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Data.Point)))

  # Check result.
  expect_that(ncol(res), equals(7))
  expect_that(nrow(res), equals(42))

  # Check markers.
  expect_false("QS1" %in% res$Marker)
  expect_true("AM" %in% res$Marker)
  expect_true("TH01" %in% res$Marker)
  expect_true("D3S1358" %in% res$Marker)
  expect_true("vWA" %in% res$Marker)
  expect_true("D21S11" %in% res$Marker)
  expect_false("QS2" %in% res$Marker)
  expect_true("D16S539" %in% res$Marker)
  expect_true("D1S1656" %in% res$Marker)
  expect_true("D19S433" %in% res$Marker)
  expect_true("SE33" %in% res$Marker)
  expect_true("D10S1248" %in% res$Marker)
  expect_true("D22S1045" %in% res$Marker)
  expect_true("D12S391" %in% res$Marker)
  expect_true("D8S1179" %in% res$Marker)
  expect_true("D2S1338" %in% res$Marker)
  expect_true("D2S441" %in% res$Marker)
  expect_true("D18S51" %in% res$Marker)
  expect_true("FGA" %in% res$Marker)

  # Check success of filtering.
  expect_false(all(c("Q") %in% res$Allele[res$Marker == "QS1"]))
  expect_true(all(c("X", "Y") %in% res$Allele[res$Marker == "AM"]))
  expect_true(all(c("6", "9.3") %in% res$Allele[res$Marker == "TH01"]))
  expect_true(all(c("16", "17", "18") %in% res$Allele[res$Marker == "D3S1358"]))
  expect_true(all(c("16", "18", "19") %in% res$Allele[res$Marker == "vWA"]))
  expect_true(all(c("29", "31.2") %in% res$Allele[res$Marker == "D21S11"]))
  expect_false(all(c("S") %in% res$Allele[res$Marker == "QS2"]))
  expect_true(all(c("9", "10", "13") %in% res$Allele[res$Marker == "D16S539"]))
  expect_true(all(c("12", "13", "14") %in% res$Allele[res$Marker == "D1S1656"]))
  expect_true(all(c("13", "14") %in% res$Allele[res$Marker == "D19S433"]))
  expect_true(all(c("15", "16") %in% res$Allele[res$Marker == "SE33"]))
  expect_true(all(c("13", "15") %in% res$Allele[res$Marker == "D10S1248"]))
  expect_true(all(c("13", "OL", "14", "16") %in% res$Allele[res$Marker == "D22S1045"]))
  expect_true(all(c("18", "23") %in% res$Allele[res$Marker == "D12S391"]))
  expect_true(all(c("14", "15") %in% res$Allele[res$Marker == "D8S1179"]))
  expect_true(all(c("22", "25") %in% res$Allele[res$Marker == "D2S1338"]))
  expect_true(all(c("OL", "10", "13.3", "14") %in% res$Allele[res$Marker == "D2S441"]))
  expect_true(all(c("16", "18") %in% res$Allele[res$Marker == "D18S51"]))
  expect_true(all(c("20", "23") %in% res$Allele[res$Marker == "FGA"]))


  # TEST 14 -------------------------------------------------------------------
  # Test that filtering of only sex markers work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set7, ref = ref7, kit = "ESSplexSEQS",
    add.missing.loci = FALSE,
    keep.na = FALSE,
    qs.rm = FALSE,
    sex.rm = TRUE,
    ignore.case = FALSE,
    filter.allele = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Data.Point" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Data.Point)))

  # Check markers.
  expect_true("QS1" %in% res$Marker)
  expect_false("AM" %in% res$Marker)
  expect_true("TH01" %in% res$Marker)
  expect_true("D3S1358" %in% res$Marker)
  expect_true("vWA" %in% res$Marker)
  expect_true("D21S11" %in% res$Marker)
  expect_true("QS2" %in% res$Marker)
  expect_true("D16S539" %in% res$Marker)
  expect_true("D1S1656" %in% res$Marker)
  expect_true("D19S433" %in% res$Marker)
  expect_true("SE33" %in% res$Marker)
  expect_true("D10S1248" %in% res$Marker)
  expect_true("D22S1045" %in% res$Marker)
  expect_true("D12S391" %in% res$Marker)
  expect_true("D8S1179" %in% res$Marker)
  expect_true("D2S1338" %in% res$Marker)
  expect_true("D2S441" %in% res$Marker)
  expect_true("D18S51" %in% res$Marker)
  expect_true("FGA" %in% res$Marker)

  # Check result.
  expect_that(ncol(res), equals(7))
  expect_that(nrow(res), equals(42))

  # Check success of filtering.
  expect_true(all(c("Q") %in% res$Allele[res$Marker == "QS1"]))
  expect_false(all(c("X", "Y") %in% res$Allele[res$Marker == "AM"]))
  expect_true(all(c("6", "9.3") %in% res$Allele[res$Marker == "TH01"]))
  expect_true(all(c("16", "17", "18") %in% res$Allele[res$Marker == "D3S1358"]))
  expect_true(all(c("16", "18", "19") %in% res$Allele[res$Marker == "vWA"]))
  expect_true(all(c("29", "31.2") %in% res$Allele[res$Marker == "D21S11"]))
  expect_true(all(c("S") %in% res$Allele[res$Marker == "QS2"]))
  expect_true(all(c("9", "10", "13") %in% res$Allele[res$Marker == "D16S539"]))
  expect_true(all(c("12", "13", "14") %in% res$Allele[res$Marker == "D1S1656"]))
  expect_true(all(c("13", "14") %in% res$Allele[res$Marker == "D19S433"]))
  expect_true(all(c("15", "16") %in% res$Allele[res$Marker == "SE33"]))
  expect_true(all(c("13", "15") %in% res$Allele[res$Marker == "D10S1248"]))
  expect_true(all(c("13", "OL", "14", "16") %in% res$Allele[res$Marker == "D22S1045"]))
  expect_true(all(c("18", "23") %in% res$Allele[res$Marker == "D12S391"]))
  expect_true(all(c("14", "15") %in% res$Allele[res$Marker == "D8S1179"]))
  expect_true(all(c("22", "25") %in% res$Allele[res$Marker == "D2S1338"]))
  expect_true(all(c("OL", "10", "13.3", "14") %in% res$Allele[res$Marker == "D2S441"]))
  expect_true(all(c("16", "18") %in% res$Allele[res$Marker == "D18S51"]))
  expect_true(all(c("20", "23") %in% res$Allele[res$Marker == "FGA"]))


  # TEST 15 -------------------------------------------------------------------
  # Test that filtering of sex markers and quality sensors work.

  # Analyse dataframe.
  res <- filterProfile(
    data = set7, ref = ref7, kit = "ESSplexSEQS",
    add.missing.loci = FALSE,
    keep.na = FALSE,
    qs.rm = TRUE,
    sex.rm = TRUE,
    ignore.case = FALSE,
    filter.allele = FALSE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Data.Point" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Data.Point)))

  # Check markers.
  expect_false("QS1" %in% res$Marker)
  expect_false("AM" %in% res$Marker)
  expect_true("TH01" %in% res$Marker)
  expect_true("D3S1358" %in% res$Marker)
  expect_true("vWA" %in% res$Marker)
  expect_true("D21S11" %in% res$Marker)
  expect_false("QS2" %in% res$Marker)
  expect_true("D16S539" %in% res$Marker)
  expect_true("D1S1656" %in% res$Marker)
  expect_true("D19S433" %in% res$Marker)
  expect_true("SE33" %in% res$Marker)
  expect_true("D10S1248" %in% res$Marker)
  expect_true("D22S1045" %in% res$Marker)
  expect_true("D12S391" %in% res$Marker)
  expect_true("D8S1179" %in% res$Marker)
  expect_true("D2S1338" %in% res$Marker)
  expect_true("D2S441" %in% res$Marker)
  expect_true("D18S51" %in% res$Marker)
  expect_true("FGA" %in% res$Marker)

  # Check result.
  expect_that(ncol(res), equals(7))
  expect_that(nrow(res), equals(40))

  # Check success of filtering.
  expect_false(all(c("Q") %in% res$Allele[res$Marker == "QS1"]))
  expect_false(all(c("X", "Y") %in% res$Allele[res$Marker == "AM"]))
  expect_true(all(c("6", "9.3") %in% res$Allele[res$Marker == "TH01"]))
  expect_true(all(c("16", "17", "18") %in% res$Allele[res$Marker == "D3S1358"]))
  expect_true(all(c("16", "18", "19") %in% res$Allele[res$Marker == "vWA"]))
  expect_true(all(c("29", "31.2") %in% res$Allele[res$Marker == "D21S11"]))
  expect_false(all(c("S") %in% res$Allele[res$Marker == "QS2"]))
  expect_true(all(c("9", "10", "13") %in% res$Allele[res$Marker == "D16S539"]))
  expect_true(all(c("12", "13", "14") %in% res$Allele[res$Marker == "D1S1656"]))
  expect_true(all(c("13", "14") %in% res$Allele[res$Marker == "D19S433"]))
  expect_true(all(c("15", "16") %in% res$Allele[res$Marker == "SE33"]))
  expect_true(all(c("13", "15") %in% res$Allele[res$Marker == "D10S1248"]))
  expect_true(all(c("13", "OL", "14", "16") %in% res$Allele[res$Marker == "D22S1045"]))
  expect_true(all(c("18", "23") %in% res$Allele[res$Marker == "D12S391"]))
  expect_true(all(c("14", "15") %in% res$Allele[res$Marker == "D8S1179"]))
  expect_true(all(c("22", "25") %in% res$Allele[res$Marker == "D2S1338"]))
  expect_true(all(c("OL", "10", "13.3", "14") %in% res$Allele[res$Marker == "D2S441"]))
  expect_true(all(c("16", "18") %in% res$Allele[res$Marker == "D18S51"]))
  expect_true(all(c("20", "23") %in% res$Allele[res$Marker == "FGA"]))


  # TEST 16 -------------------------------------------------------------------
  # Test that filtering of sex markers, quality sensors, and known alleles work.

  res <- filterProfile(
    data = set7, ref = ref7, kit = "ESSplexSEQS",
    add.missing.loci = FALSE,
    keep.na = FALSE,
    qs.rm = TRUE,
    sex.rm = TRUE,
    ignore.case = FALSE,
    filter.allele = TRUE,
    debug = FALSE
  )

  # Check return class.
  expect_match(class(res), class(data.frame()))

  # Check that expected columns exist.
  expect_true("Sample.Name" %in% names(res))
  expect_true("Marker" %in% names(res))
  expect_true("Dye" %in% names(res))
  expect_true("Allele" %in% names(res))
  expect_true("Size" %in% names(res))
  expect_true("Height" %in% names(res))
  expect_true("Data.Point" %in% names(res))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$Data.Point)))

  # Check markers.
  expect_false("QS1" %in% res$Marker)
  expect_false("AM" %in% res$Marker)
  expect_true("TH01" %in% res$Marker)
  expect_true("D3S1358" %in% res$Marker)
  expect_true("vWA" %in% res$Marker)
  expect_true("D21S11" %in% res$Marker)
  expect_false("QS2" %in% res$Marker)
  expect_true("D16S539" %in% res$Marker)
  expect_true("D1S1656" %in% res$Marker)
  expect_true("D19S433" %in% res$Marker)
  expect_true("SE33" %in% res$Marker)
  expect_true("D10S1248" %in% res$Marker)
  expect_true("D22S1045" %in% res$Marker)
  expect_true("D12S391" %in% res$Marker)
  expect_true("D8S1179" %in% res$Marker)
  expect_true("D2S1338" %in% res$Marker)
  expect_true("D2S441" %in% res$Marker)
  expect_true("D18S51" %in% res$Marker)
  expect_true("FGA" %in% res$Marker)

  # Check result.
  expect_that(ncol(res), equals(7))
  expect_that(nrow(res), equals(31))

  # Check success of filtering.
  expect_false(all(c("Q") %in% res$Allele[res$Marker == "QS1"]))
  expect_false(all(c("X", "Y") %in% res$Allele[res$Marker == "AM"]))
  expect_true(all(c("6", "9.3") %in% res$Allele[res$Marker == "TH01"]))
  expect_true(all(c("17", "18") %in% res$Allele[res$Marker == "D3S1358"]))
  expect_true(all(c("16", "19") %in% res$Allele[res$Marker == "vWA"]))
  expect_true(all(c("29", "31.2") %in% res$Allele[res$Marker == "D21S11"]))
  expect_false(all(c("S") %in% res$Allele[res$Marker == "QS2"]))
  expect_true(all(c("9", "13") %in% res$Allele[res$Marker == "D16S539"]))
  expect_true(all(c("12", "13") %in% res$Allele[res$Marker == "D1S1656"]))
  expect_true(all(c("13", "14") %in% res$Allele[res$Marker == "D19S433"]))
  expect_true(all(c("15", "16") %in% res$Allele[res$Marker == "SE33"]))
  expect_true(all(c("13", "15") %in% res$Allele[res$Marker == "D10S1248"]))
  expect_true(all(c("16") %in% res$Allele[res$Marker == "D22S1045"]))
  expect_true(all(c("18", "23") %in% res$Allele[res$Marker == "D12S391"]))
  expect_true(all(c("14", "15") %in% res$Allele[res$Marker == "D8S1179"]))
  expect_true(all(c("22", "25") %in% res$Allele[res$Marker == "D2S1338"]))
  expect_true(all(c("10", "14") %in% res$Allele[res$Marker == "D2S441"]))
  expect_true(all(c("16", "18") %in% res$Allele[res$Marker == "D18S51"]))
  expect_true(all(c("20", "23") %in% res$Allele[res$Marker == "FGA"]))
})
