context("tableBalance")

################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 25.09.2016: Added Test 3 for Lb.
# 24.09.2016: Updated to work with tableBalance, replacing tableHb.
# 07.09.2016: Updated to work with calculateHb, replacing calculateBalance.
# 13.11.2015: Updated hb=2 to hb=3 in to correspond to new implemented method in calculateBalance.
# 07.05.2014: First version.
# 
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-tableBalance.r")
# test_dir("tests/testthat")

test_that("tableBalance", {

  # Get test data.
  data(set2)
  data(ref2)
  
  data(set1)
  data(ref1)
  
  set1 <- slim(data = set1,
               fix = c("Sample.Name", "Marker", "Dye"),
               stack = c("Allele", "Height"))
  
  ref1 <- slim(data = ref1,
               fix = c("Sample.Name", "Marker"),
               stack = c("Allele"))
  
  # TEST 01 -------------------------------------------------------------------
  
  # Analyse dataframe.
  tmp <- calculateHb(data=set2, ref=ref2, hb=3, kit="SGMPlus", ignore.case=TRUE)
  
  res <- tableBalance(data=tmp, scope="locus", quant=0.05)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.Perc.5))

  # Check for NA's.
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Hb.Min)))
  expect_false(any(is.na(res$Hb.Mean)))
  expect_false(any(is.na(res$Hb.Sd)))
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
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225))))
  expect_that(res$Hb.Sd[2], equals(sd(c(423/491,198/241))))
  expect_that(res$Hb.Sd[3], equals(sd(c(587/632,312/326))))
  expect_that(res$Hb.Sd[4], equals(sd(c(361/398,195/206))))
  expect_that(res$Hb.Sd[5], equals(sd(c(359/384,179/183))))

  # Check result: 5 percentile for heterozygous balance.
  expect_that(res$Hb.Perc.5[1], equals(as.numeric(quantile(c(402/460,215/225), 0.05))))
  expect_that(res$Hb.Perc.5[2], equals(as.numeric(quantile(c(423/491,198/241), 0.05))))
  expect_that(res$Hb.Perc.5[3], equals(as.numeric(quantile(c(587/632,312/326), 0.05))))
  expect_that(res$Hb.Perc.5[4], equals(as.numeric(quantile(c(361/398,195/206), 0.05))))
  expect_that(res$Hb.Perc.5[5], equals(as.numeric(quantile(c(359/384,179/183), 0.05))))

  
  # TEST 02 -------------------------------------------------------------------
  
  # Analyse dataframe.
  tmp <- calculateHb(data=set2, ref=ref2, hb=3, kit="SGMPlus", ignore.case=TRUE)

  res <- tableBalance(data=tmp, scope="global", quant=0.10)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(is.null(res$Sample.Name))
  expect_true(is.null(res$Marker))
  expect_false(is.null(res$Hb.n))
  expect_false(is.null(res$Hb.Mean))
  expect_false(is.null(res$Hb.Sd))
  expect_false(is.null(res$Hb.Perc.10))

  # Check for NA's.
  expect_false(any(is.na(res$Hb.n)))
  expect_false(any(is.na(res$Hb.Min)))
  expect_false(any(is.na(res$Hb.Mean)))
  expect_false(any(is.na(res$Hb.Sd)))
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
  expect_that(res$Hb.Sd[1], equals(sd(c(402/460,215/225,
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

  # TEST 03 -------------------------------------------------------------------
  
  # Analyse dataframe.
  tmp <- calculateLb(data=set1, ref=ref1, option = "prop", kit="ESX17", ignore.case=TRUE)
  
  res <- tableBalance(data=tmp, scope="locus", quant=0.50)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Lb.n))
  expect_false(is.null(res$Lb.Mean))
  expect_false(is.null(res$Lb.Sd))
  expect_false(is.null(res$Lb.Perc.50))
  
  # Check for NA's.
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Lb.n)))
  expect_false(any(is.na(res$Lb.Min)))
  expect_false(any(is.na(res$Lb.Mean)))
  expect_false(any(is.na(res$Lb.Sd)))
  expect_false(any(is.na(res$Lb.Perc.50)))
  
  # Check result: number of values for inter-locus balance.
  expect_true(all(res$Lb.n == 8))

  # Check result: Min inter-locus balance.
  expect_that(res$Lb.Min[1], equals(0.048600544))
  expect_that(res$Lb.Min[2], equals(0.048662025))
  expect_that(res$Lb.Min[3], equals(0.049292203))
  expect_that(res$Lb.Min[4], equals(0.037338186))
  expect_that(res$Lb.Min[5], equals(0.03024454))
  expect_that(res$Lb.Min[6], equals(0.039394177))
  expect_that(res$Lb.Min[7], equals(0.048654504))
  expect_that(res$Lb.Min[8], equals(0.041719227))
  expect_that(res$Lb.Min[9], equals(0.037007766))
  expect_that(res$Lb.Min[10], equals(0.034652096))
  expect_that(res$Lb.Min[11], equals(0.032356098))
  expect_that(res$Lb.Min[12], equals(0.042946988))
  expect_that(res$Lb.Min[13], equals(0.04126255))
  expect_that(res$Lb.Min[14], equals(0.107477969))
  expect_that(res$Lb.Min[15], equals(0.058938857))
  expect_that(res$Lb.Min[16], equals(0.080518159))
  expect_that(res$Lb.Min[17], equals(0.059245481))
  
  # Check result: Mean inter-locus balance.
  expect_that(res$Lb.Mean[1], equals(0.052283019))
  expect_that(res$Lb.Mean[2], equals(0.058387614))
  expect_that(res$Lb.Mean[3], equals(0.055042658))
  expect_that(res$Lb.Mean[4], equals(0.042483131))
  expect_that(res$Lb.Mean[5], equals(0.038350197))
  expect_that(res$Lb.Mean[6], equals(0.04650999))
  expect_that(res$Lb.Mean[7], equals(0.058029856))
  expect_that(res$Lb.Mean[8], equals(0.053037101))
  expect_that(res$Lb.Mean[9], equals(0.045257733))
  expect_that(res$Lb.Mean[10], equals(0.041672006))
  expect_that(res$Lb.Mean[11], equals(0.037998933))
  expect_that(res$Lb.Mean[12], equals(0.050232335))
  expect_that(res$Lb.Mean[13], equals(0.050088299))
  expect_that(res$Lb.Mean[14], equals(0.138718137))
  expect_that(res$Lb.Mean[15], equals(0.069514774))
  expect_that(res$Lb.Mean[16], equals(0.092054472))
  expect_that(res$Lb.Mean[17], equals(0.070339746))
  
  # Check result: Inter-locus balance standard deviation.
  expect_that(res$Lb.Sd[1], equals(0.003277776))
  expect_that(res$Lb.Sd[2], equals(0.008279161))
  expect_that(res$Lb.Sd[3], equals(0.004423236))
  expect_that(res$Lb.Sd[4], equals(0.005637513))
  expect_that(res$Lb.Sd[5], equals(0.005354049))
  expect_that(res$Lb.Sd[6], equals(0.00613077))
  expect_that(res$Lb.Sd[7], equals(0.007031517))
  expect_that(res$Lb.Sd[8], equals(0.006103916))
  expect_that(res$Lb.Sd[9], equals(0.005214698))
  expect_that(res$Lb.Sd[10], equals(0.004914415))
  expect_that(res$Lb.Sd[11], equals(0.005110143))
  expect_that(res$Lb.Sd[12], equals(0.004469587))
  expect_that(res$Lb.Sd[13], equals(0.005841286))
  expect_that(res$Lb.Sd[14], equals(0.018571829))
  expect_that(res$Lb.Sd[15], equals(0.00769572))
  expect_that(res$Lb.Sd[16], equals(0.009962088))
  expect_that(res$Lb.Sd[17], equals(0.009275144))

  # Check result: Max inter-locus balance.
  expect_that(res$Lb.Max[1], equals(0.057763768))
  expect_that(res$Lb.Max[2], equals(0.069457361))
  expect_that(res$Lb.Max[3], equals(0.061920789))
  expect_that(res$Lb.Max[4], equals(0.051510603))
  expect_that(res$Lb.Max[5], equals(0.048600544))
  expect_that(res$Lb.Max[6], equals(0.059365188))
  expect_that(res$Lb.Max[7], equals(0.068569997))
  expect_that(res$Lb.Max[8], equals(0.05912144))
  expect_that(res$Lb.Max[9], equals(0.052885888))
  expect_that(res$Lb.Max[10], equals(0.049457177))
  expect_that(res$Lb.Max[11], equals(0.046060889))
  expect_that(res$Lb.Max[12], equals(0.0559448))
  expect_that(res$Lb.Max[13], equals(0.057138451))
  expect_that(res$Lb.Max[14], equals(0.157758543))
  expect_that(res$Lb.Max[15], equals(0.079043085))
  expect_that(res$Lb.Max[16], equals(0.112178367))
  expect_that(res$Lb.Max[17], equals(0.085363181))
  
  # Check result: 50 percentile for inter-locus balance.
  expect_that(res$Lb.Perc.50[1], equals(0.05109985))
  expect_that(res$Lb.Perc.50[2], equals(0.05865409))
  expect_that(res$Lb.Perc.50[3], equals(0.05626877))
  expect_that(res$Lb.Perc.50[4], equals(0.03965191))
  expect_that(res$Lb.Perc.50[5], equals(0.03881472))
  expect_that(res$Lb.Perc.50[6], equals(0.04511564))
  expect_that(res$Lb.Perc.50[7], equals(0.05743930))
  expect_that(res$Lb.Perc.50[8], equals(0.05567573))
  expect_that(res$Lb.Perc.50[9], equals(0.04520154))
  expect_that(res$Lb.Perc.50[10], equals(0.04158521))
  expect_that(res$Lb.Perc.50[11], equals(0.03812017))
  expect_that(res$Lb.Perc.50[12], equals(0.05022724))
  expect_that(res$Lb.Perc.50[13], equals(0.05062542))
  expect_that(res$Lb.Perc.50[14], equals(0.14312375))
  expect_that(res$Lb.Perc.50[15], equals(0.06942801))
  expect_that(res$Lb.Perc.50[16], equals(0.09111388))
  expect_that(res$Lb.Perc.50[17], equals(0.06878986))

})