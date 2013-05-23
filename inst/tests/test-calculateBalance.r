context("calculateBalance")

# TODO: Test matchSource.
# TODO: Test ignoreCase.


test_that("calculateBalance", {

  # Get test data.
  data(set2)
  data(ref2)

  # TEST 01 -------------------------------------------------------------------
  
  # Analyse dataframe.
  res <- calculateBalance(data=set2, ref=ref2, perSample=TRUE, lb="prop",
                         perDye=TRUE, minHeight=NULL, maxHeight=NULL,
                         ignoreCase=TRUE, matchSource="ref")

  # Warn that 'Height' was not numeric.
  test01 <- set2
  test01$Height <- as.character(test01$Height)
  expect_that(calculateBalance(data=test01, ref=ref2), gives_warning())

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MpH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  
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
  expect_that(res$MpH[1], equals(431))
  expect_that(res$MpH[2], equals(253))
  expect_that(res$MpH[3], equals(457))
  expect_that(res$MpH[4], equals(361.5))
  expect_that(res$MpH[5], equals(609.5))
  expect_that(res$MpH[6], equals(309.5))
  expect_that(res$MpH[7], equals(383))
  expect_that(res$MpH[8], equals(379.5))
  expect_that(res$MpH[9], equals(296))
  expect_that(res$MpH[10], equals(371.5))
  expect_that(res$MpH[11], equals(212.5))
  
  expect_that(res$MpH[12], equals(220))
  expect_that(res$MpH[13], equals(152))
  expect_that(res$MpH[14], equals(219.5))
  expect_that(res$MpH[15], equals(173.5))
  expect_that(res$MpH[16], equals(319))
  expect_that(res$MpH[17], equals(154.5))
  expect_that(res$MpH[18], equals(201))
  expect_that(res$MpH[19], equals(200.5))
  expect_that(res$MpH[20], equals(142))
  expect_that(res$MpH[21], equals(181))
  expect_that(res$MpH[22], equals(as.numeric(NA)))
  
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
  
  # TEST 02 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=TRUE, lb="prop",
                          perDye=FALSE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MpH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  
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
  expect_that(res$MpH[1], equals(431))
  expect_that(res$MpH[2], equals(253))
  expect_that(res$MpH[3], equals(457))
  expect_that(res$MpH[4], equals(361.5))
  expect_that(res$MpH[5], equals(609.5))
  expect_that(res$MpH[6], equals(309.5))
  expect_that(res$MpH[7], equals(383))
  expect_that(res$MpH[8], equals(379.5))
  expect_that(res$MpH[9], equals(296))
  expect_that(res$MpH[10], equals(371.5))
  expect_that(res$MpH[11], equals(212.5))
  
  expect_that(res$MpH[12], equals(220))
  expect_that(res$MpH[13], equals(152))
  expect_that(res$MpH[14], equals(219.5))
  expect_that(res$MpH[15], equals(173.5))
  expect_that(res$MpH[16], equals(319))
  expect_that(res$MpH[17], equals(154.5))
  expect_that(res$MpH[18], equals(201))
  expect_that(res$MpH[19], equals(200.5))
  expect_that(res$MpH[20], equals(142))
  expect_that(res$MpH[21], equals(181))
  expect_that(res$MpH[22], equals(as.numeric(NA)))
  
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
  
  # TEST 03 -------------------------------------------------------------------

  res <- calculateBalance(data=set2, ref=ref2, perSample=TRUE, lb="norm",
                          perDye=TRUE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MpH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  
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
  expect_that(res$MpH[1], equals(431))
  expect_that(res$MpH[2], equals(253))
  expect_that(res$MpH[3], equals(457))
  expect_that(res$MpH[4], equals(361.5))
  expect_that(res$MpH[5], equals(609.5))
  expect_that(res$MpH[6], equals(309.5))
  expect_that(res$MpH[7], equals(383))
  expect_that(res$MpH[8], equals(379.5))
  expect_that(res$MpH[9], equals(296))
  expect_that(res$MpH[10], equals(371.5))
  expect_that(res$MpH[11], equals(212.5))
  
  expect_that(res$MpH[12], equals(220))
  expect_that(res$MpH[13], equals(152))
  expect_that(res$MpH[14], equals(219.5))
  expect_that(res$MpH[15], equals(173.5))
  expect_that(res$MpH[16], equals(319))
  expect_that(res$MpH[17], equals(154.5))
  expect_that(res$MpH[18], equals(201))
  expect_that(res$MpH[19], equals(200.5))
  expect_that(res$MpH[20], equals(142))
  expect_that(res$MpH[21], equals(181))
  expect_that(res$MpH[22], equals(as.numeric(NA)))
  
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
  
  
  # TEST 04 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=TRUE, lb="norm",
                          perDye=FALSE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MpH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  
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
  expect_that(res$MpH[1], equals(431))
  expect_that(res$MpH[2], equals(253))
  expect_that(res$MpH[3], equals(457))
  expect_that(res$MpH[4], equals(361.5))
  expect_that(res$MpH[5], equals(609.5))
  expect_that(res$MpH[6], equals(309.5))
  expect_that(res$MpH[7], equals(383))
  expect_that(res$MpH[8], equals(379.5))
  expect_that(res$MpH[9], equals(296))
  expect_that(res$MpH[10], equals(371.5))
  expect_that(res$MpH[11], equals(212.5))
  
  expect_that(res$MpH[12], equals(220))
  expect_that(res$MpH[13], equals(152))
  expect_that(res$MpH[14], equals(219.5))
  expect_that(res$MpH[15], equals(173.5))
  expect_that(res$MpH[16], equals(319))
  expect_that(res$MpH[17], equals(154.5))
  expect_that(res$MpH[18], equals(201))
  expect_that(res$MpH[19], equals(200.5))
  expect_that(res$MpH[20], equals(142))
  expect_that(res$MpH[21], equals(181))
  expect_that(res$MpH[22], equals(as.numeric(NA)))
  
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
  
  
  
  # TEST 05 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="prop",
                          perDye=TRUE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.95))
  expect_false(is.null(res$Lb.n))
  expect_false(is.null(res$Lb.Mean))
  expect_false(is.null(res$Lb.Sd))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Lb.n)))
  
  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(2))
  expect_that(res$Hb.n[2], equals(0))
  expect_that(res$Hb.n[3], equals(2))
  expect_that(res$Hb.n[4], equals(0))
  expect_that(res$Hb.n[5], equals(2))
  expect_that(res$Hb.n[6], equals(0))
  expect_that(res$Hb.n[7], equals(0))
  expect_that(res$Hb.n[8], equals(2))
  expect_that(res$Hb.n[9], equals(0))
  expect_that(res$Hb.n[10], equals(2))
  expect_that(res$Hb.n[11], equals(0))

  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
  expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
  expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
  expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
  expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
  expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))

  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
  expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
  
  # Check result: 95% CI for heterozygous balance.
  expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.95[2], equals(as.numeric(NA)))
  expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.95[4], equals(as.numeric(NA)))
  expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.95[6], equals(as.numeric(NA)))
  expect_that(res$Hb.95[7], equals(as.numeric(NA)))
  expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.95[9], equals(as.numeric(NA)))
  expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
  expect_that(res$Hb.95[11], equals(as.numeric(NA)))
  
  # Check result: number of values for locus balance.
  expect_that(res$Lb.n[1], equals(2))
  expect_that(res$Lb.n[2], equals(2))
  expect_that(res$Lb.n[3], equals(2))
  expect_that(res$Lb.n[4], equals(2))
  expect_that(res$Lb.n[5], equals(2))
  expect_that(res$Lb.n[6], equals(2))
  expect_that(res$Lb.n[7], equals(2))
  expect_that(res$Lb.n[8], equals(2))
  expect_that(res$Lb.n[9], equals(1))
  expect_that(res$Lb.n[10], equals(1))
  expect_that(res$Lb.n[11], equals(1))
  
  # Check result: Mean locus balance.
  expect_that(res$Lb.Mean[1], equals(mean(c(862/3005,440/1530))))
  expect_that(res$Lb.Mean[2], equals(mean(c(506/3005,304/1530))))
  expect_that(res$Lb.Mean[3], equals(mean(c(914/3005,439/1530))))
  expect_that(res$Lb.Mean[4], equals(mean(c(723/3005,347/1530))))
  expect_that(res$Lb.Mean[5], equals(mean(c(1219/3363,638/1750))))
  expect_that(res$Lb.Mean[6], equals(mean(c(619/3363,309/1750))))
  expect_that(res$Lb.Mean[7], equals(mean(c(766/3363,402/1750))))
  expect_that(res$Lb.Mean[8], equals(mean(c(759/3363,401/1750))))
  expect_that(res$Lb.Mean[9], equals(592/1760))
  expect_that(res$Lb.Mean[10], equals(743/1760))
  expect_that(res$Lb.Mean[11], equals(425/1760))

  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Lb.Sd[1], equals(sd(c(862/3005,440/1530))))
  expect_that(res$Lb.Sd[2], equals(sd(c(506/3005,304/1530))))
  expect_that(res$Lb.Sd[3], equals(sd(c(914/3005,439/1530))))
  expect_that(res$Lb.Sd[4], equals(sd(c(723/3005,347/1530))))
  expect_that(res$Lb.Sd[5], equals(sd(c(1219/3363,638/1750))))
  expect_that(res$Lb.Sd[6], equals(sd(c(619/3363,309/1750))))
  expect_that(res$Lb.Sd[7], equals(sd(c(766/3363,402/1750))))
  expect_that(res$Lb.Sd[8], equals(sd(c(759/3363,401/1750))))
  expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
  
  
  # TEST 06 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="prop",
                          perDye=FALSE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.95))
  expect_false(is.null(res$Lb.n))
  expect_false(is.null(res$Lb.Mean))
  expect_false(is.null(res$Lb.Sd))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Lb.n)))
  
  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(2))
  expect_that(res$Hb.n[2], equals(0))
  expect_that(res$Hb.n[3], equals(2))
  expect_that(res$Hb.n[4], equals(0))
  expect_that(res$Hb.n[5], equals(2))
  expect_that(res$Hb.n[6], equals(0))
  expect_that(res$Hb.n[7], equals(0))
  expect_that(res$Hb.n[8], equals(2))
  expect_that(res$Hb.n[9], equals(0))
  expect_that(res$Hb.n[10], equals(2))
  expect_that(res$Hb.n[11], equals(0))
  
  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
  expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
  expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
  expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
  expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
  expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
  expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
  
  # Check result: 95% CI for heterozygous balance.
  expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.95[2], equals(as.numeric(NA)))
  expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.95[4], equals(as.numeric(NA)))
  expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.95[6], equals(as.numeric(NA)))
  expect_that(res$Hb.95[7], equals(as.numeric(NA)))
  expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.95[9], equals(as.numeric(NA)))
  expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
  expect_that(res$Hb.95[11], equals(as.numeric(NA)))
  
  # Check result: number of values for locus balance.
  expect_that(res$Lb.n[1], equals(1))
  expect_that(res$Lb.n[2], equals(1))
  expect_that(res$Lb.n[3], equals(1))
  expect_that(res$Lb.n[4], equals(1))
  expect_that(res$Lb.n[5], equals(1))
  expect_that(res$Lb.n[6], equals(1))
  expect_that(res$Lb.n[7], equals(1))
  expect_that(res$Lb.n[8], equals(1))
  expect_that(res$Lb.n[9], equals(1))
  expect_that(res$Lb.n[10], equals(1))
  expect_that(res$Lb.n[11], equals(1))
  
  # Check result: Mean locus balance.
  expect_that(res$Lb.Mean[1], equals(862/8128))
  expect_that(res$Lb.Mean[2], equals(506/8128))
  expect_that(res$Lb.Mean[3], equals(914/8128))
  expect_that(res$Lb.Mean[4], equals(723/8128))
  expect_that(res$Lb.Mean[5], equals(1219/8128))
  expect_that(res$Lb.Mean[6], equals(619/8128))
  expect_that(res$Lb.Mean[7], equals(766/8128))
  expect_that(res$Lb.Mean[8], equals(759/8128))
  expect_that(res$Lb.Mean[9], equals(592/8128))
  expect_that(res$Lb.Mean[10], equals(743/8128))
  expect_that(res$Lb.Mean[11], equals(425/8128))
  
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Lb.Sd[1], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[3], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[5], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[8], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
  
  # TEST 07 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="norm",
                          perDye=TRUE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.95))
  expect_false(is.null(res$Lb.n))
  expect_false(is.null(res$Lb.Mean))
  expect_false(is.null(res$Lb.Sd))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Lb.n)))
  
  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(2))
  expect_that(res$Hb.n[2], equals(0))
  expect_that(res$Hb.n[3], equals(2))
  expect_that(res$Hb.n[4], equals(0))
  expect_that(res$Hb.n[5], equals(2))
  expect_that(res$Hb.n[6], equals(0))
  expect_that(res$Hb.n[7], equals(0))
  expect_that(res$Hb.n[8], equals(2))
  expect_that(res$Hb.n[9], equals(0))
  expect_that(res$Hb.n[10], equals(2))
  expect_that(res$Hb.n[11], equals(0))
  
  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
  expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
  expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
  expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
  expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
  expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
  expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
  
  # Check result: 95% CI for heterozygous balance.
  expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.95[2], equals(as.numeric(NA)))
  expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.95[4], equals(as.numeric(NA)))
  expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.95[6], equals(as.numeric(NA)))
  expect_that(res$Hb.95[7], equals(as.numeric(NA)))
  expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.95[9], equals(as.numeric(NA)))
  expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
  expect_that(res$Hb.95[11], equals(as.numeric(NA)))
  
  # Check result: number of values for locus balance.
  expect_that(res$Lb.n[1], equals(2))
  expect_that(res$Lb.n[2], equals(2))
  expect_that(res$Lb.n[3], equals(2))
  expect_that(res$Lb.n[4], equals(2))
  expect_that(res$Lb.n[5], equals(2))
  expect_that(res$Lb.n[6], equals(2))
  expect_that(res$Lb.n[7], equals(2))
  expect_that(res$Lb.n[8], equals(2))
  expect_that(res$Lb.n[9], equals(1))
  expect_that(res$Lb.n[10], equals(1))
  expect_that(res$Lb.n[11], equals(1))
  
  # Check result: Mean locus balance.
  expect_that(res$Lb.Mean[1], equals(mean(c(862/914,440/440))))
  expect_that(res$Lb.Mean[2], equals(mean(c(506/914,304/440))))
  expect_that(res$Lb.Mean[3], equals(mean(c(914/914,439/440))))
  expect_that(res$Lb.Mean[4], equals(mean(c(723/914,347/440))))
  expect_that(res$Lb.Mean[5], equals(mean(c(1219/1219,638/638))))
  expect_that(res$Lb.Mean[6], equals(mean(c(619/1219,309/638))))
  expect_that(res$Lb.Mean[7], equals(mean(c(766/1219,402/638))))
  expect_that(res$Lb.Mean[8], equals(mean(c(759/1219,401/638))))
  expect_that(res$Lb.Mean[9], equals(592/743))
  expect_that(res$Lb.Mean[10], equals(743/743))
  expect_that(res$Lb.Mean[11], equals(425/743))
  
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Lb.Sd[1], equals(sd(c(862/914,440/440))))
  expect_that(res$Lb.Sd[2], equals(sd(c(506/914,304/440))))
  expect_that(res$Lb.Sd[3], equals(sd(c(914/914,439/440))))
  expect_that(res$Lb.Sd[4], equals(sd(c(723/914,347/440))))
  expect_that(res$Lb.Sd[5], equals(sd(c(1219/1219,638/638))))
  expect_that(res$Lb.Sd[6], equals(sd(c(619/1219,309/638))))
  expect_that(res$Lb.Sd[7], equals(sd(c(766/1219,402/638))))
  expect_that(res$Lb.Sd[8], equals(sd(c(759/1219,401/638))))
  expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
  
  
  # TEST 08 -------------------------------------------------------------------
  
  res <- calculateBalance(data=set2, ref=ref2, perSample=FALSE, lb="norm",
                          perDye=FALSE, minHeight=NULL, maxHeight=NULL,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.95))
  expect_false(is.null(res$Lb.n))
  expect_false(is.null(res$Lb.Mean))
  expect_false(is.null(res$Lb.Sd))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Lb.n)))
  
  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(2))
  expect_that(res$Hb.n[2], equals(0))
  expect_that(res$Hb.n[3], equals(2))
  expect_that(res$Hb.n[4], equals(0))
  expect_that(res$Hb.n[5], equals(2))
  expect_that(res$Hb.n[6], equals(0))
  expect_that(res$Hb.n[7], equals(0))
  expect_that(res$Hb.n[8], equals(2))
  expect_that(res$Hb.n[9], equals(0))
  expect_that(res$Hb.n[10], equals(2))
  expect_that(res$Hb.n[11], equals(0))
  
  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
  expect_that(res$Hb.Mean[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[3], equals(mean(c(423/491,198/241))))
  expect_that(res$Hb.Mean[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[5], equals(mean(c(587/632,312/326))))
  expect_that(res$Hb.Mean[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[8], equals(mean(c(361/398,195/206))))
  expect_that(res$Hb.Mean[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Mean[10], equals(mean(c(359/384,179/183))))
  expect_that(res$Hb.Mean[11], equals(as.numeric(NA)))
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[3], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[5], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[8], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Hb.Sd[10], equals(sd(c(359/384,179/183))))
  expect_that(res$Hb.Sd[11], equals(as.numeric(NA)))
  
  # Check result: 95% CI for heterozygous balance.
  expect_that(res$Hb.95[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.95[2], equals(as.numeric(NA)))
  expect_that(res$Hb.95[3], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.95[4], equals(as.numeric(NA)))
  expect_that(res$Hb.95[5], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.95[6], equals(as.numeric(NA)))
  expect_that(res$Hb.95[7], equals(as.numeric(NA)))
  expect_that(res$Hb.95[8], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.95[9], equals(as.numeric(NA)))
  expect_that(res$Hb.95[10], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))
  expect_that(res$Hb.95[11], equals(as.numeric(NA)))
  
  # Check result: number of values for locus balance.
  expect_that(res$Lb.n[1], equals(1))
  expect_that(res$Lb.n[2], equals(1))
  expect_that(res$Lb.n[3], equals(1))
  expect_that(res$Lb.n[4], equals(1))
  expect_that(res$Lb.n[5], equals(1))
  expect_that(res$Lb.n[6], equals(1))
  expect_that(res$Lb.n[7], equals(1))
  expect_that(res$Lb.n[8], equals(1))
  expect_that(res$Lb.n[9], equals(1))
  expect_that(res$Lb.n[10], equals(1))
  expect_that(res$Lb.n[11], equals(1))
  
  # Check result: Mean locus balance.
  expect_that(res$Lb.Mean[1], equals(862/1219))
  expect_that(res$Lb.Mean[2], equals(506/1219))
  expect_that(res$Lb.Mean[3], equals(914/1219))
  expect_that(res$Lb.Mean[4], equals(723/1219))
  expect_that(res$Lb.Mean[5], equals(1219/1219))
  expect_that(res$Lb.Mean[6], equals(619/1219))
  expect_that(res$Lb.Mean[7], equals(766/1219))
  expect_that(res$Lb.Mean[8], equals(759/1219))
  expect_that(res$Lb.Mean[9], equals(592/1219))
  expect_that(res$Lb.Mean[10], equals(743/1219))
  expect_that(res$Lb.Mean[11], equals(425/1219))
  
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Lb.Sd[1], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[2], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[3], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[4], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[5], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[6], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[7], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[8], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[9], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[10], equals(as.numeric(NA)))
  expect_that(res$Lb.Sd[11], equals(as.numeric(NA)))
  
  # TEST 09 -------------------------------------------------------------------
  
  # Analyse dataframe.
  res <- calculateBalance(data=set2, ref=ref2, perSample=TRUE, lb="prop",
                          perDye=TRUE, minHeight=200, maxHeight=600,
                          ignoreCase=TRUE, matchSource="ref")
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb))
  expect_false(is.null(res$Lb))
  expect_false(is.null(res$MpH))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(as.numeric(NA)))
  expect_that(res$Hb[3], equals(423/491))
  expect_that(res$Hb[4], equals(as.numeric(NA)))
  expect_that(res$Hb[5], equals(as.numeric(NA)))
  expect_that(res$Hb[6], equals(as.numeric(NA)))
  expect_that(res$Hb[7], equals(as.numeric(NA)))
  expect_that(res$Hb[8], equals(361/398))
  expect_that(res$Hb[9], equals(as.numeric(NA)))
  expect_that(res$Hb[10], equals(359/384))
  expect_that(res$Hb[11], equals(as.numeric(NA)))
  
  expect_that(res$Hb[12], equals(215/225))
  expect_that(res$Hb[13], equals(as.numeric(NA)))
  expect_that(res$Hb[14], equals(as.numeric(NA)))
  expect_that(res$Hb[15], equals(as.numeric(NA)))
  expect_that(res$Hb[16], equals(312/326))
  expect_that(res$Hb[17], equals(as.numeric(NA)))
  expect_that(res$Hb[18], equals(as.numeric(NA)))
  expect_that(res$Hb[19], equals(as.numeric(NA)))
  expect_that(res$Hb[20], equals(as.numeric(NA)))
  expect_that(res$Hb[21], equals(as.numeric(NA)))
  expect_that(res$Hb[22], equals(as.numeric(NA)))
  
  # Check result: Mean peak height.
  expect_that(res$MpH[1], equals(431))
  expect_that(res$MpH[2], equals(253))
  expect_that(res$MpH[3], equals(457))
  expect_that(res$MpH[4], equals(as.numeric(NA)))
  expect_that(res$MpH[5], equals(as.numeric(NA)))
  expect_that(res$MpH[6], equals(as.numeric(NA)))
  expect_that(res$MpH[7], equals(as.numeric(NA)))
  expect_that(res$MpH[8], equals(379.5))
  expect_that(res$MpH[9], equals(296))
  expect_that(res$MpH[10], equals(371.5))
  expect_that(res$MpH[11], equals(212.5))
  
  expect_that(res$MpH[12], equals(220))
  expect_that(res$MpH[13], equals(152))
  expect_that(res$MpH[14], equals(as.numeric(NA)))
  expect_that(res$MpH[15], equals(173.5))
  expect_that(res$MpH[16], equals(319))
  expect_that(res$MpH[17], equals(154.5))
  expect_that(res$MpH[18], equals(201))
  expect_that(res$MpH[19], equals(as.numeric(NA)))
  expect_that(res$MpH[20], equals(142))
  expect_that(res$MpH[21], equals(as.numeric(NA)))
  expect_that(res$MpH[22], equals(as.numeric(NA)))
  
  # Check result: Locus balance.
  expect_that(res$Lb[1], equals(as.numeric(NA)))
  expect_that(res$Lb[2], equals(as.numeric(NA)))
  expect_that(res$Lb[3], equals(as.numeric(NA)))
  expect_that(res$Lb[4], equals(as.numeric(NA)))
  expect_that(res$Lb[5], equals(as.numeric(NA)))
  expect_that(res$Lb[6], equals(as.numeric(NA)))
  expect_that(res$Lb[7], equals(as.numeric(NA)))
  expect_that(res$Lb[8], equals(as.numeric(NA)))
  expect_that(res$Lb[9], equals(592/1760))
  expect_that(res$Lb[10], equals(743/1760))
  expect_that(res$Lb[11], equals(425/1760))
  
  # TODO: check how to calc total Lb when using min/max. Should give NA is a 
  #       peak has been removed in a color/sample.
  expect_that(res$Lb[12], equals(440/1332))
  expect_that(res$Lb[13], equals(304/1332))
  expect_that(res$Lb[14], equals(241/1332))
  expect_that(res$Lb[15], equals(347/1332))
  expect_that(res$Lb[16], equals(638/1555))
  expect_that(res$Lb[17], equals(309/1555))
  expect_that(res$Lb[18], equals(402/1555))
  expect_that(res$Lb[19], equals(206/1555))
  expect_that(res$Lb[20], equals(as.numeric(NA)))
  expect_that(res$Lb[21], equals(as.numeric(NA)))
  expect_that(res$Lb[22], equals(as.numeric(NA)))
  
  
  
})