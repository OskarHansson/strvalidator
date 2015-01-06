context("calculateBalance")

################################################################################
# TODO LIST
# TODO: Test ignore.case.
# TODO: ...

################################################################################
# CHANGE LOG
# 07.05.2014: Updated in response to new column 'TPH' and NA in 'MPH' if homozygot. 
# 23.02.2014: Updated in response to removing the 'perSample' option. 
# 20.01.2014: Added test (12) for multiple matches (two 'max' peaks)
# 
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateBalance.r")
# test_dir("tests/testthat")

test_that("calculateBalance", {

  # Get test data.
  data(set2)
  data(ref2)

  # TEST 01 -------------------------------------------------------------------
  
  # Analyse dataframe.
  res <- calculateBalance(data=set2, ref=ref2, lb="prop",
                         per.dye=TRUE, hb=2,
                         ignore.case=TRUE)

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
                          
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(198/241))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(195/206))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(379.5))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(220))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(200.5))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))
  
  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(862/3005))
  expect_that(res$Lb[2], equals(506/3005))
  expect_that(res$Lb[3], equals(914/3005))
  expect_that(res$Lb[4], equals(723/3005))
  expect_that(res$Lb[5], equals(1219/3363))
  expect_that(res$Lb[6], equals(619/3363))
  expect_that(res$Lb[7], equals(766/3363))
  expect_that(res$Lb[8], equals(759/3363))
  expect_that(res$Lb[9], equals(592/1760))
  expect_that(res$Lb[10], equals(743/1760))
  expect_that(res$Lb[11], equals(425/1760))
  
  expect_that(res$Lb[12], equals(440/1530))
  expect_that(res$Lb[13], equals(304/1530))
  expect_that(res$Lb[14], equals(439/1530))
  expect_that(res$Lb[15], equals(347/1530))
  expect_that(res$Lb[16], equals(638/1750))
  expect_that(res$Lb[17], equals(309/1750))
  expect_that(res$Lb[18], equals(402/1750))
  expect_that(res$Lb[19], equals(401/1750))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))
  
  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(862))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(759))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(440))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(401))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))
  
  # TEST 02 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, lb="prop",
                          per.dye=FALSE, hb=2,
                          ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(198/241))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(195/206))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(379.5))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(220))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(200.5))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))
  
  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(862/8128))
  expect_that(res$Lb[2], equals(506/8128))
  expect_that(res$Lb[3], equals(914/8128))
  expect_that(res$Lb[4], equals(723/8128))
  expect_that(res$Lb[5], equals(1219/8128))
  expect_that(res$Lb[6], equals(619/8128))
  expect_that(res$Lb[7], equals(766/8128))
  expect_that(res$Lb[8], equals(759/8128))
  expect_that(res$Lb[9], equals(592/8128))
  expect_that(res$Lb[10], equals(743/8128))
  expect_that(res$Lb[11], equals(425/8128))
  
  expect_that(res$Lb[12], equals(as.numeric(NA)))
  expect_that(res$Lb[13], equals(as.numeric(NA)))
  expect_that(res$Lb[14], equals(as.numeric(NA)))
  expect_that(res$Lb[15], equals(as.numeric(NA)))
  expect_that(res$Lb[16], equals(as.numeric(NA)))
  expect_that(res$Lb[17], equals(as.numeric(NA)))
  expect_that(res$Lb[18], equals(as.numeric(NA)))
  expect_that(res$Lb[19], equals(as.numeric(NA)))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))

  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(862))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(759))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(440))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(401))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))
  
  # TEST 03 -------------------------------------------------------------------

  res <- calculateBalance(data=set2, ref=ref2, lb="norm",
                          per.dye=TRUE, hb=2,
                          ignore.case=TRUE)

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(198/241))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(195/206))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(379.5))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(220))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(200.5))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))
  
  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(862/914))
  expect_that(res$Lb[2], equals(506/914))
  expect_that(res$Lb[3], equals(914/914))
  expect_that(res$Lb[4], equals(723/914))
  expect_that(res$Lb[5], equals(1219/1219))
  expect_that(res$Lb[6], equals(619/1219))
  expect_that(res$Lb[7], equals(766/1219))
  expect_that(res$Lb[8], equals(759/1219))
  expect_that(res$Lb[9], equals(592/743))
  expect_that(res$Lb[10], equals(743/743))
  expect_that(res$Lb[11], equals(425/743))
  
  expect_that(res$Lb[12], equals(440/440))
  expect_that(res$Lb[13], equals(304/440))
  expect_that(res$Lb[14], equals(439/440))
  expect_that(res$Lb[15], equals(347/440))
  expect_that(res$Lb[16], equals(638/638))
  expect_that(res$Lb[17], equals(309/638))
  expect_that(res$Lb[18], equals(402/638))
  expect_that(res$Lb[19], equals(401/638))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))

  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(862))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(759))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(440))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(401))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))
  
  
  # TEST 04 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, lb="norm",
                          per.dye=FALSE, hb=2,
                          ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(198/241))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(195/206))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(379.5))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(220))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(200.5))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))
  
  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(862/1219))
  expect_that(res$Lb[2], equals(506/1219))
  expect_that(res$Lb[3], equals(914/1219))
  expect_that(res$Lb[4], equals(723/1219))
  expect_that(res$Lb[5], equals(1219/1219))
  expect_that(res$Lb[6], equals(619/1219))
  expect_that(res$Lb[7], equals(766/1219))
  expect_that(res$Lb[8], equals(759/1219))
  expect_that(res$Lb[9], equals(592/1219))
  expect_that(res$Lb[10], equals(743/1219))
  expect_that(res$Lb[11], equals(425/1219))
  
  expect_that(res$Lb[12], equals(as.numeric(NA)))
  expect_that(res$Lb[13], equals(as.numeric(NA)))
  expect_that(res$Lb[14], equals(as.numeric(NA)))
  expect_that(res$Lb[15], equals(as.numeric(NA)))
  expect_that(res$Lb[16], equals(as.numeric(NA)))
  expect_that(res$Lb[17], equals(as.numeric(NA)))
  expect_that(res$Lb[18], equals(as.numeric(NA)))
  expect_that(res$Lb[19], equals(as.numeric(NA)))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))
  
  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(862))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(759))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(440))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(401))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))
  
  
#   # TEST 05 -------------------------------------------------------------------
#   
#   res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="prop",
#                           per.dye=TRUE, hb=2,
#                           ignore.case=TRUE)
#   
#   # Check return class.  
#   expect_that(class(res), matches(class(data.frame())))
#   
#   # Check that expected columns exist.  
#   expect_false(is.null(res$Sample.Name))
#   expect_false(is.null(res$Marker))
#   expect_false(is.null(res$Hb.n))
#   expect_false(is.null(res$Hb.Mean))
#   expect_false(is.null(res$Hb.Sd))
#   expect_false(is.null(res$Hb.95))
#   expect_false(is.null(res$Lb.n))
#   expect_false(is.null(res$Lb.Mean))
#   expect_false(is.null(res$Lb.Sd))
#   
#   # Check for NA's.
#   expect_false(any(is.na(res$Sample.Name)))
#   expect_false(any(is.na(res$Marker)))
#   expect_false(any(is.na(res$Hb.n)))
#   expect_false(any(is.na(res$Lb.n)))
# 
#   # Check result: Repeat unit difference.
#   expect_that(res$Delta[1], equals(3))
#   expect_that(res$Delta[2], equals(as.numeric(NA)))
#   expect_that(res$Delta[3], equals(2))
#   expect_that(res$Delta[4], equals(as.numeric(NA)))
#   expect_that(res$Delta[5], equals(1))
#   expect_that(res$Delta[6], equals(as.numeric(NA)))
#   expect_that(res$Delta[7], equals(as.numeric(NA)))
#   expect_that(res$Delta[8], equals(15.2))
#   expect_that(res$Delta[9], equals(as.numeric(NA)))
#   expect_that(res$Delta[10], equals(4))
#   expect_that(res$Delta[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for heterozygous balance.
#   expect_that(res$Hb.n[1], equals(2))
#   expect_that(res$Hb.n[2], equals(0))
#   expect_that(res$Hb.n[3], equals(2))
#   expect_that(res$Hb.n[4], equals(0))
#   expect_that(res$Hb.n[5], equals(2))
#   expect_that(res$Hb.n[6], equals(0))
#   expect_that(res$Hb.n[7], equals(0))
#   expect_that(res$Hb.n[8], equals(2))
#   expect_that(res$Hb.n[9], equals(0))
#   expect_that(res$Hb.n[10], equals(2))
#   expect_that(res$Hb.n[11], equals(0))
# 
#   # Check result: Mean heterozygous balance.
#   expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
#   expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
#   expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
#   expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
#   expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
#   expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
# 
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
#   expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
#   expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
#   expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
#   expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
#   expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
#   
#   # Check result: 95% CI for heterozygous balance.
#   expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
#   expect_that(res$Hb.95[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
#   expect_that(res$Hb.95[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
#   expect_that(res$Hb.95[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
#   expect_that(res$Hb.95[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
#   expect_that(res$Hb.95[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for locus balance.
#   expect_that(res$Lb.n[1], equals(2))
#   expect_that(res$Lb.n[2], equals(2))
#   expect_that(res$Lb.n[3], equals(2))
#   expect_that(res$Lb.n[4], equals(2))
#   expect_that(res$Lb.n[5], equals(2))
#   expect_that(res$Lb.n[6], equals(2))
#   expect_that(res$Lb.n[7], equals(2))
#   expect_that(res$Lb.n[8], equals(2))
#   expect_that(res$Lb.n[9], equals(1))
#   expect_that(res$Lb.n[10], equals(1))
#   expect_that(res$Lb.n[11], equals(1))
#   
#   # Check result: Mean locus balance.
#   expect_that(res$Lb.Mean[1], equals(mean(c(862/3005,440/1530))))
#   expect_that(res$Lb.Mean[2], equals(mean(c(506/3005,304/1530))))
#   expect_that(res$Lb.Mean[3], equals(mean(c(914/3005,439/1530))))
#   expect_that(res$Lb.Mean[4], equals(mean(c(723/3005,347/1530))))
#   expect_that(res$Lb.Mean[5], equals(mean(c(1219/3363,638/1750))))
#   expect_that(res$Lb.Mean[6], equals(mean(c(619/3363,309/1750))))
#   expect_that(res$Lb.Mean[7], equals(mean(c(766/3363,402/1750))))
#   expect_that(res$Lb.Mean[8], equals(mean(c(759/3363,401/1750))))
#   expect_that(res$Lb.Mean[9], equals(592/1760))
#   expect_that(res$Lb.Mean[10], equals(743/1760))
#   expect_that(res$Lb.Mean[11], equals(425/1760))
# 
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Lb.Sd[1], equals(sd(c(862/3005,440/1530))))
#   expect_that(res$Lb.Sd[2], equals(sd(c(506/3005,304/1530))))
#   expect_that(res$Lb.Sd[3], equals(sd(c(914/3005,439/1530))))
#   expect_that(res$Lb.Sd[4], equals(sd(c(723/3005,347/1530))))
#   expect_that(res$Lb.Sd[5], equals(sd(c(1219/3363,638/1750))))
#   expect_that(res$Lb.Sd[6], equals(sd(c(619/3363,309/1750))))
#   expect_that(res$Lb.Sd[7], equals(sd(c(766/3363,402/1750))))
#   expect_that(res$Lb.Sd[8], equals(sd(c(759/3363,401/1750))))
#   expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
#   
#   
#   # TEST 06 -------------------------------------------------------------------
#   
#   res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="prop",
#                           per.dye=FALSE, hb=2,
#                           ignore.case=TRUE)
#   
#   # Check return class.  
#   expect_that(class(res), matches(class(data.frame())))
#   
#   # Check that expected columns exist.  
#   expect_false(is.null(res$Sample.Name))
#   expect_false(is.null(res$Marker))
#   expect_false(is.null(res$Hb.n))
#   expect_false(is.null(res$Hb.Mean))
#   expect_false(is.null(res$Hb.Sd))
#   expect_false(is.null(res$Hb.95))
#   expect_false(is.null(res$Lb.n))
#   expect_false(is.null(res$Lb.Mean))
#   expect_false(is.null(res$Lb.Sd))
#   
#   # Check for NA's.
#   expect_false(any(is.na(res$Sample.Name)))
#   expect_false(any(is.na(res$Marker)))
#   expect_false(any(is.na(res$Hb.n)))
#   expect_false(any(is.na(res$Lb.n)))
#   
#   # Check result: Repeat unit difference.
#   expect_that(res$Delta[1], equals(3))
#   expect_that(res$Delta[2], equals(as.numeric(NA)))
#   expect_that(res$Delta[3], equals(2))
#   expect_that(res$Delta[4], equals(as.numeric(NA)))
#   expect_that(res$Delta[5], equals(1))
#   expect_that(res$Delta[6], equals(as.numeric(NA)))
#   expect_that(res$Delta[7], equals(as.numeric(NA)))
#   expect_that(res$Delta[8], equals(15.2))
#   expect_that(res$Delta[9], equals(as.numeric(NA)))
#   expect_that(res$Delta[10], equals(4))
#   expect_that(res$Delta[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for heterozygous balance.
#   expect_that(res$Hb.n[1], equals(2))
#   expect_that(res$Hb.n[2], equals(0))
#   expect_that(res$Hb.n[3], equals(2))
#   expect_that(res$Hb.n[4], equals(0))
#   expect_that(res$Hb.n[5], equals(2))
#   expect_that(res$Hb.n[6], equals(0))
#   expect_that(res$Hb.n[7], equals(0))
#   expect_that(res$Hb.n[8], equals(2))
#   expect_that(res$Hb.n[9], equals(0))
#   expect_that(res$Hb.n[10], equals(2))
#   expect_that(res$Hb.n[11], equals(0))
#   
#   # Check result: Mean heterozygous balance.
#   expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
#   expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
#   expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
#   expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
#   expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
#   expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
#   expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
#   expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
#   expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
#   expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
#   expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
#   
#   # Check result: 95% CI for heterozygous balance.
#   expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
#   expect_that(res$Hb.95[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
#   expect_that(res$Hb.95[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
#   expect_that(res$Hb.95[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
#   expect_that(res$Hb.95[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
#   expect_that(res$Hb.95[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for locus balance.
#   expect_that(res$Lb.n[1], equals(1))
#   expect_that(res$Lb.n[2], equals(1))
#   expect_that(res$Lb.n[3], equals(1))
#   expect_that(res$Lb.n[4], equals(1))
#   expect_that(res$Lb.n[5], equals(1))
#   expect_that(res$Lb.n[6], equals(1))
#   expect_that(res$Lb.n[7], equals(1))
#   expect_that(res$Lb.n[8], equals(1))
#   expect_that(res$Lb.n[9], equals(1))
#   expect_that(res$Lb.n[10], equals(1))
#   expect_that(res$Lb.n[11], equals(1))
#   
#   # Check result: Mean locus balance.
#   expect_that(res$Lb.Mean[1], equals(862/8128))
#   expect_that(res$Lb.Mean[2], equals(506/8128))
#   expect_that(res$Lb.Mean[3], equals(914/8128))
#   expect_that(res$Lb.Mean[4], equals(723/8128))
#   expect_that(res$Lb.Mean[5], equals(1219/8128))
#   expect_that(res$Lb.Mean[6], equals(619/8128))
#   expect_that(res$Lb.Mean[7], equals(766/8128))
#   expect_that(res$Lb.Mean[8], equals(759/8128))
#   expect_that(res$Lb.Mean[9], equals(592/8128))
#   expect_that(res$Lb.Mean[10], equals(743/8128))
#   expect_that(res$Lb.Mean[11], equals(425/8128))
#   
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Lb.Sd[1], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[3], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[5], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[8], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
#   
#   # TEST 07 -------------------------------------------------------------------
#   
#   res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="norm",
#                           per.dye=TRUE, hb=2,
#                           ignore.case=TRUE)
#   
#   # Check return class.  
#   expect_that(class(res), matches(class(data.frame())))
#   
#   # Check that expected columns exist.  
#   expect_false(is.null(res$Sample.Name))
#   expect_false(is.null(res$Marker))
#   expect_false(is.null(res$Hb.n))
#   expect_false(is.null(res$Hb.Mean))
#   expect_false(is.null(res$Hb.Sd))
#   expect_false(is.null(res$Hb.95))
#   expect_false(is.null(res$Lb.n))
#   expect_false(is.null(res$Lb.Mean))
#   expect_false(is.null(res$Lb.Sd))
#   
#   # Check for NA's.
#   expect_false(any(is.na(res$Sample.Name)))
#   expect_false(any(is.na(res$Marker)))
#   expect_false(any(is.na(res$Hb.n)))
#   expect_false(any(is.na(res$Lb.n)))
#   
#   # Check result: Repeat unit difference.
#   expect_that(res$Delta[1], equals(3))
#   expect_that(res$Delta[2], equals(as.numeric(NA)))
#   expect_that(res$Delta[3], equals(2))
#   expect_that(res$Delta[4], equals(as.numeric(NA)))
#   expect_that(res$Delta[5], equals(1))
#   expect_that(res$Delta[6], equals(as.numeric(NA)))
#   expect_that(res$Delta[7], equals(as.numeric(NA)))
#   expect_that(res$Delta[8], equals(15.2))
#   expect_that(res$Delta[9], equals(as.numeric(NA)))
#   expect_that(res$Delta[10], equals(4))
#   expect_that(res$Delta[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for heterozygous balance.
#   expect_that(res$Hb.n[1], equals(2))
#   expect_that(res$Hb.n[2], equals(0))
#   expect_that(res$Hb.n[3], equals(2))
#   expect_that(res$Hb.n[4], equals(0))
#   expect_that(res$Hb.n[5], equals(2))
#   expect_that(res$Hb.n[6], equals(0))
#   expect_that(res$Hb.n[7], equals(0))
#   expect_that(res$Hb.n[8], equals(2))
#   expect_that(res$Hb.n[9], equals(0))
#   expect_that(res$Hb.n[10], equals(2))
#   expect_that(res$Hb.n[11], equals(0))
#   
#   # Check result: Mean heterozygous balance.
#   expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
#   expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
#   expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
#   expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
#   expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
#   expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
#   expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
#   expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
#   expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
#   expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
#   expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
#   
#   # Check result: 95% CI for heterozygous balance.
#   expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
#   expect_that(res$Hb.95[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
#   expect_that(res$Hb.95[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
#   expect_that(res$Hb.95[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
#   expect_that(res$Hb.95[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
#   expect_that(res$Hb.95[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for locus balance.
#   expect_that(res$Lb.n[1], equals(2))
#   expect_that(res$Lb.n[2], equals(2))
#   expect_that(res$Lb.n[3], equals(2))
#   expect_that(res$Lb.n[4], equals(2))
#   expect_that(res$Lb.n[5], equals(2))
#   expect_that(res$Lb.n[6], equals(2))
#   expect_that(res$Lb.n[7], equals(2))
#   expect_that(res$Lb.n[8], equals(2))
#   expect_that(res$Lb.n[9], equals(1))
#   expect_that(res$Lb.n[10], equals(1))
#   expect_that(res$Lb.n[11], equals(1))
#   
#   # Check result: Mean locus balance.
#   expect_that(res$Lb.Mean[1], equals(mean(c(862/914,440/440))))
#   expect_that(res$Lb.Mean[2], equals(mean(c(506/914,304/440))))
#   expect_that(res$Lb.Mean[3], equals(mean(c(914/914,439/440))))
#   expect_that(res$Lb.Mean[4], equals(mean(c(723/914,347/440))))
#   expect_that(res$Lb.Mean[5], equals(mean(c(1219/1219,638/638))))
#   expect_that(res$Lb.Mean[6], equals(mean(c(619/1219,309/638))))
#   expect_that(res$Lb.Mean[7], equals(mean(c(766/1219,402/638))))
#   expect_that(res$Lb.Mean[8], equals(mean(c(759/1219,401/638))))
#   expect_that(res$Lb.Mean[9], equals(592/743))
#   expect_that(res$Lb.Mean[10], equals(743/743))
#   expect_that(res$Lb.Mean[11], equals(425/743))
#   
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Lb.Sd[1], equals(sd(c(862/914,440/440))))
#   expect_that(res$Lb.Sd[2], equals(sd(c(506/914,304/440))))
#   expect_that(res$Lb.Sd[3], equals(sd(c(914/914,439/440))))
#   expect_that(res$Lb.Sd[4], equals(sd(c(723/914,347/440))))
#   expect_that(res$Lb.Sd[5], equals(sd(c(1219/1219,638/638))))
#   expect_that(res$Lb.Sd[6], equals(sd(c(619/1219,309/638))))
#   expect_that(res$Lb.Sd[7], equals(sd(c(766/1219,402/638))))
#   expect_that(res$Lb.Sd[8], equals(sd(c(759/1219,401/638))))
#   expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
#   
#   
#   # TEST 08 -------------------------------------------------------------------
#   
#   res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="norm",
#                           per.dye=FALSE, hb=2,
#                           ignore.case=TRUE)
#   
#   # Check return class.  
#   expect_that(class(res), matches(class(data.frame())))
#   
#   # Check that expected columns exist.  
#   expect_false(is.null(res$Sample.Name))
#   expect_false(is.null(res$Marker))
#   expect_false(is.null(res$Hb.n))
#   expect_false(is.null(res$Hb.Mean))
#   expect_false(is.null(res$Hb.Sd))
#   expect_false(is.null(res$Hb.95))
#   expect_false(is.null(res$Lb.n))
#   expect_false(is.null(res$Lb.Mean))
#   expect_false(is.null(res$Lb.Sd))
#   
#   # Check for NA's.
#   expect_false(any(is.na(res$Sample.Name)))
#   expect_false(any(is.na(res$Marker)))
#   expect_false(any(is.na(res$Hb.n)))
#   expect_false(any(is.na(res$Lb.n)))
#   
#   # Check result: Repeat unit difference.
#   expect_that(res$Delta[1], equals(3))
#   expect_that(res$Delta[2], equals(as.numeric(NA)))
#   expect_that(res$Delta[3], equals(2))
#   expect_that(res$Delta[4], equals(as.numeric(NA)))
#   expect_that(res$Delta[5], equals(1))
#   expect_that(res$Delta[6], equals(as.numeric(NA)))
#   expect_that(res$Delta[7], equals(as.numeric(NA)))
#   expect_that(res$Delta[8], equals(15.2))
#   expect_that(res$Delta[9], equals(as.numeric(NA)))
#   expect_that(res$Delta[10], equals(4))
#   expect_that(res$Delta[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for heterozygous balance.
#   expect_that(res$Hb.n[1], equals(2))
#   expect_that(res$Hb.n[2], equals(0))
#   expect_that(res$Hb.n[3], equals(2))
#   expect_that(res$Hb.n[4], equals(0))
#   expect_that(res$Hb.n[5], equals(2))
#   expect_that(res$Hb.n[6], equals(0))
#   expect_that(res$Hb.n[7], equals(0))
#   expect_that(res$Hb.n[8], equals(2))
#   expect_that(res$Hb.n[9], equals(0))
#   expect_that(res$Hb.n[10], equals(2))
#   expect_that(res$Hb.n[11], equals(0))
#   
#   # Check result: Mean heterozygous balance.
#   expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
#   expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
#   expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
#   expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
#   expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
#   expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
#   expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
#   expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
#   expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
#   expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
#   expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
#   
#   # Check result: 95% CI for heterozygous balance.
#   expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
#   expect_that(res$Hb.95[2], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
#   expect_that(res$Hb.95[4], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
#   expect_that(res$Hb.95[6], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[7], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
#   expect_that(res$Hb.95[9], equals(as.numeric(NA)))
#   expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
#   expect_that(res$Hb.95[11], equals(as.numeric(NA)))
#   
#   # Check result: number of values for locus balance.
#   expect_that(res$Lb.n[1], equals(1))
#   expect_that(res$Lb.n[2], equals(1))
#   expect_that(res$Lb.n[3], equals(1))
#   expect_that(res$Lb.n[4], equals(1))
#   expect_that(res$Lb.n[5], equals(1))
#   expect_that(res$Lb.n[6], equals(1))
#   expect_that(res$Lb.n[7], equals(1))
#   expect_that(res$Lb.n[8], equals(1))
#   expect_that(res$Lb.n[9], equals(1))
#   expect_that(res$Lb.n[10], equals(1))
#   expect_that(res$Lb.n[11], equals(1))
#   
#   # Check result: Mean locus balance.
#   expect_that(res$Lb.Mean[1], equals(862/1219))
#   expect_that(res$Lb.Mean[2], equals(506/1219))
#   expect_that(res$Lb.Mean[3], equals(914/1219))
#   expect_that(res$Lb.Mean[4], equals(723/1219))
#   expect_that(res$Lb.Mean[5], equals(1219/1219))
#   expect_that(res$Lb.Mean[6], equals(619/1219))
#   expect_that(res$Lb.Mean[7], equals(766/1219))
#   expect_that(res$Lb.Mean[8], equals(759/1219))
#   expect_that(res$Lb.Mean[9], equals(592/1219))
#   expect_that(res$Lb.Mean[10], equals(743/1219))
#   expect_that(res$Lb.Mean[11], equals(425/1219))
#   
#   
#   # Check result: Heterozygous balance standard deviation.
#   expect_that(res$Lb.Sd[1], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[2], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[3], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[4], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[5], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[6], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[7], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[8], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
#   expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
  
  # TEST 09 -------------------------------------------------------------------
  # Test when a marker is missing from dataset.

  # Remove TH01 from dataset.
  setMissing <- set2[set2$Marker!="TH01",]
  setMissing <- set2[set2$Dye!="Y",]
  
  # Analyse dataframe.
  expect_that(calculateBalance(data=setMissing, ref=ref2, lb="prop",
                               per.dye=TRUE, hb=2,
                               ignore.case=TRUE),
              throws_error()) 
  

  # TEST 10 -------------------------------------------------------------------
  # Test when all markers in one dye channel is missing from dataset.
  
  # Remove yellow dye channel from dataset.
  setMissing <- set2[set2$Dye!="Y",]
  
  # Analyse dataframe.
  expect_that(calculateBalance(data=setMissing, ref=ref2, lb="prop",
                               per.dye=TRUE, hb=2,
                               ignore.case=TRUE),
              throws_error()) 
  
  # TEST 11 -------------------------------------------------------------------
  
  # Analyse dataframe.
  res <- calculateBalance(data=set2, ref=ref2, lb="prop",
                          per.dye=TRUE, hb=1,
                          ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))

  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(491/423))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(384/359))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(241/198))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(195/206))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(379.5))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(220))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(200.5))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))

  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(862/3005))
  expect_that(res$Lb[2], equals(506/3005))
  expect_that(res$Lb[3], equals(914/3005))
  expect_that(res$Lb[4], equals(723/3005))
  expect_that(res$Lb[5], equals(1219/3363))
  expect_that(res$Lb[6], equals(619/3363))
  expect_that(res$Lb[7], equals(766/3363))
  expect_that(res$Lb[8], equals(759/3363))
  expect_that(res$Lb[9], equals(592/1760))
  expect_that(res$Lb[10], equals(743/1760))
  expect_that(res$Lb[11], equals(425/1760))
  
  expect_that(res$Lb[12], equals(440/1530))
  expect_that(res$Lb[13], equals(304/1530))
  expect_that(res$Lb[14], equals(439/1530))
  expect_that(res$Lb[15], equals(347/1530))
  expect_that(res$Lb[16], equals(638/1750))
  expect_that(res$Lb[17], equals(309/1750))
  expect_that(res$Lb[18], equals(402/1750))
  expect_that(res$Lb[19], equals(401/1750))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))

  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(862))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(759))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(440))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(401))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))

  
  # TEST 12 -------------------------------------------------------------------

  # Test that two equally high maximum peaks works.
  setEqual <- set2
  setEqual[setEqual$Marker == "D3S1358",]$Height <- 400
  setEqual[setEqual$Marker == "D18S51",]$Height <- 550
  
  # Analyse dataframe.
  res <- calculateBalance(data=setEqual, ref=ref2, lb="prop",
                          per.dye=TRUE, hb=2,
                          ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$TPH))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Delta)))
  expect_true(any(is.na(res$Hb)))
  expect_true(any(is.na(res$Lb)))
  expect_true(any(is.na(res$MPH)))
  expect_true(any(is.na(res$TPH)))

  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(as.numeric(NA)))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(as.numeric(NA)))
  expect_that(res$Delta[5], equals(1))
  expect_that(res$Delta[6], equals(as.numeric(NA)))
  expect_that(res$Delta[7], equals(as.numeric(NA)))
  expect_that(res$Delta[8], equals(15.2))
  expect_that(res$Delta[9], equals(as.numeric(NA)))
  expect_that(res$Delta[10], equals(4))
  expect_that(res$Delta[11], equals(as.numeric(NA)))
  
  expect_that(res$Delta[12], equals(3))
  expect_that(res$Delta[13], equals(as.numeric(NA)))
  expect_that(res$Delta[14], equals(2))
  expect_that(res$Delta[15], equals(as.numeric(NA)))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(as.numeric(NA)))
  expect_that(res$Delta[18], equals(as.numeric(NA)))
  expect_that(res$Delta[19], equals(15.2))
  expect_that(res$Delta[20], equals(as.numeric(NA)))
  expect_that(res$Delta[21], equals(4))
  expect_that(res$Delta[22], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(1))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(587/632))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(1))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(1))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(198/241))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(1))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(179/183))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(400))
  expect_that(res$MPH[2], equals(as.numeric(NA)))
  expect_that(res$MPH[3], equals(457))
  expect_that(res$MPH[4], equals(as.numeric(NA)))
  expect_that(res$MPH[5], equals(609.5))
  expect_that(res$MPH[6], equals(as.numeric(NA)))
  expect_that(res$MPH[7], equals(as.numeric(NA)))
  expect_that(res$MPH[8], equals(550))
  expect_that(res$MPH[9], equals(as.numeric(NA)))
  expect_that(res$MPH[10], equals(371.5))
  expect_that(res$MPH[11], equals(as.numeric(NA)))
  
  expect_that(res$MPH[12], equals(400))
  expect_that(res$MPH[13], equals(as.numeric(NA)))
  expect_that(res$MPH[14], equals(219.5))
  expect_that(res$MPH[15], equals(as.numeric(NA)))
  expect_that(res$MPH[16], equals(319))
  expect_that(res$MPH[17], equals(as.numeric(NA)))
  expect_that(res$MPH[18], equals(as.numeric(NA)))
  expect_that(res$MPH[19], equals(550))
  expect_that(res$MPH[20], equals(as.numeric(NA)))
  expect_that(res$MPH[21], equals(181))
  expect_that(res$MPH[22], equals(as.numeric(NA)))

  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(800/2943))
  expect_that(res$Lb[2], equals(506/2943))
  expect_that(res$Lb[3], equals(914/2943))
  expect_that(res$Lb[4], equals(723/2943))
  expect_that(res$Lb[5], equals(1219/3704))
  expect_that(res$Lb[6], equals(619/3704))
  expect_that(res$Lb[7], equals(766/3704))
  expect_that(res$Lb[8], equals(1100/3704))
  expect_that(res$Lb[9], equals(592/1760))
  expect_that(res$Lb[10], equals(743/1760))
  expect_that(res$Lb[11], equals(425/1760))
  
  expect_that(res$Lb[12], equals(800/1890))
  expect_that(res$Lb[13], equals(304/1890))
  expect_that(res$Lb[14], equals(439/1890))
  expect_that(res$Lb[15], equals(347/1890))
  expect_that(res$Lb[16], equals(638/2449))
  expect_that(res$Lb[17], equals(309/2449))
  expect_that(res$Lb[18], equals(402/2449))
  expect_that(res$Lb[19], equals(1100/2449))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))
  
  # Check result: Total peak height.
  expect_that(res$TPH[1], equals(800))
  expect_that(res$TPH[2], equals(506))
  expect_that(res$TPH[3], equals(914))
  expect_that(res$TPH[4], equals(723))
  expect_that(res$TPH[5], equals(1219))
  expect_that(res$TPH[6], equals(619))
  expect_that(res$TPH[7], equals(766))
  expect_that(res$TPH[8], equals(1100))
  expect_that(res$TPH[9], equals(592))
  expect_that(res$TPH[10], equals(743))
  expect_that(res$TPH[11], equals(425))
  
  expect_that(res$TPH[12], equals(800))
  expect_that(res$TPH[13], equals(304))
  expect_that(res$TPH[14], equals(439))
  expect_that(res$TPH[15], equals(347))
  expect_that(res$TPH[16], equals(638))
  expect_that(res$TPH[17], equals(309))
  expect_that(res$TPH[18], equals(402))
  expect_that(res$TPH[19], equals(1100))
  expect_that(res$TPH[20], equals(284))
  expect_that(res$TPH[21], equals(362))
  expect_that(res$TPH[22], equals(as.numeric(NA)))

})