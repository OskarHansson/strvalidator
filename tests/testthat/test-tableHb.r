context("tableBalance")

################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 07.09.2016: Updated to work with calculateHb, replacing calculateBalance.
# 13.11.2015: Updated hb=2 to hb=3 in to correspond to new implemented method in calculateBalance.
# 07.05.2014: First version.
# 
# 
# test_dir("inst/tests/")
# test_file("tests/testthat/test-tableBalance.r")
# test_dir("tests/testthat")

test_that("tableBalance", {

  # Get test data.
  data(set2)
  data(ref2)

  # TEST 01 -------------------------------------------------------------------
  
  # Analyse dataframe.
  tmp <- calculateHb(data=set2, ref=ref2, hb=3, kit="SGMPlus", ignore.case=TRUE)
  
  res <- tableHb(data=tmp, scope="locus", quant=0.05)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Stdv))
  expect_false(is.null(res$Hb.Perc.5))

  # Check for NA's.
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Hb.Min)))
  expect_false(any(is.na(res$Hb.Mean)))
  expect_false(any(is.na(res$Hb.Stdv)))
  expect_false(any(is.na(res$Hb.Perc.5)))

  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(2))
  expect_that(res$Hb.n[2], equals(2))
  expect_that(res$Hb.n[3], equals(2))
  expect_that(res$Hb.n[4], equals(2))
  expect_that(res$Hb.n[5], equals(2))

  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225))))
  expect_that(res$Hb.Mean[2], equals(mean(c(423/491,198/241))))
  expect_that(res$Hb.Mean[3], equals(mean(c(587/632,312/326))))
  expect_that(res$Hb.Mean[4], equals(mean(c(361/398,195/206))))
  expect_that(res$Hb.Mean[5], equals(mean(c(359/384,179/183))))

  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Stdv[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Stdv[2], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Stdv[3], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Stdv[4], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Stdv[5], equals(sd(c(359/384,179/183))))

  # Check result: 5 percentile for heterozygous balance.
  expect_that(res$Hb.Perc.5[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.Perc.5[2], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.Perc.5[3], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.Perc.5[4], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.Perc.5[5], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))

  
  # TEST 02 -------------------------------------------------------------------
  
  # Analyse dataframe.
  tmp <- calculateHb(data=set2, ref=ref2, hb=3, kit="SGMPlus", ignore.case=TRUE)

  res <- tableHb(data=tmp, scope="global", quant=0.10)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Stdv))
  expect_false(is.null(res$Hb.Perc.10))

  # Check for NA's.
  expect_true(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Hb.Min)))
  expect_false(any(is.na(res$Hb.Mean)))
  expect_false(any(is.na(res$Hb.Stdv)))
  expect_false(any(is.na(res$Hb.Perc.10)))

  # Check result: number of values for heterozygous balance.
  expect_that(res$Hb.n[1], equals(10))
  
  # Check result: Mean heterozygous balance.
  expect_that(res$Hb.Mean[1], equals(mean(c(402/460,215/225,
                                            423/491,198/241, 
                                            587/632,312/326,
                                            361/398,195/206,
                                            359/384,179/183))))
  
  # Check result: Heterozygous balance standard deviation.
  expect_that(res$Hb.Stdv[1], equals(sd(c(402/460,215/225,
                                          423/491,198/241,
                                          587/632,312/326,
                                          361/398,195/206,
                                          359/384,179/183))))
  
  # Check result: 10 percentile for heterozygous balance.
  expect_that(res$Hb.Perc.10[1], equals(as.numeric(quantile(c(402/460,215/225,
                                                              423/491,198/241,
                                                              587/632,312/326,
                                                              361/398,195/206,
                                                              359/384,179/183), 0.10))))

  
})