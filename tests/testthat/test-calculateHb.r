context("calculateHb")

################################################################################
# TODO LIST
# TODO: Test ignore.case.
# TODO: ...

################################################################################
# CHANGE LOG
# 29.08.2016: First tests 1-11.
# 
# require(strvalidator)
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateHb.r")
# test_dir("tests/testthat")

test_that("calculateHb", {

  # Get SGM Plus test data.
  data("set2")
  data("ref2")
  
  # Get Fusion test data and extract one sample.
  data("set6")
  set6 <- subset(set6, Sample.Name=="D_0.50000NG-2")
  data("ref61")
  data("ref62")

  # Create a dataset with two equally high maximum peaks.
  setEqual <- set2
  setEqual[setEqual$Marker == "D3S1358",]$Height <- 400
  setEqual[setEqual$Marker == "D18S51",]$Height <- 550
  
  # Create a dataset with extra peaks (unfiltered data).
  setUnfiltered <- set2
  extraName <- c("SampleA01","SampleA01","SampleA02")
  extraMarker <- c("D3S1358", "D16S539", "FGA")
  extraAllele <- c("17", "10", "24")
  extraHeight <- c(50, 400, 40)
  extraDye <- c("B", "B", "Y")
  extra <- data.frame(Sample.Name = extraName,
                      Marker = extraMarker,
                      Allele = extraAllele,
                      Height = extraHeight,
                      Dye = extraDye,
                      stringsAsFactors = FALSE)
  setUnfiltered <- rbind(setUnfiltered, extra)
  
  

  # TEST 01 -------------------------------------------------------------------
  # Test normal calculation of hb=1.
  
  # Analyse dataframe.
  res <- calculateHb(data=set2, ref=ref2, hb=1, kit="SGMplus", ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Dye))
  expect_false(is.null(res$Delta))
  expect_false(is.null(res$HMW))
  expect_false(is.null(res$LMW))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$Hb))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Delta)))
  expect_false(any(is.na(res$HMW)))
  expect_false(any(is.na(res$LMW)))
  expect_false(any(is.na(res$MPH)))
  expect_false(any(is.na(res$Hb)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(2))
  expect_that(res$Delta[3], equals(1))
  expect_that(res$Delta[4], equals(15.2))
  expect_that(res$Delta[5], equals(4))
  expect_that(res$Delta[6], equals(3))
  expect_that(res$Delta[7], equals(2))
  expect_that(res$Delta[8], equals(1))
  expect_that(res$Delta[9], equals(15.2))
  expect_that(res$Delta[10], equals(4))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(491/423))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(361/398))
  expect_that(res$Hb[5], equals(384/359))
  expect_that(res$Hb[6], equals(215/225))
  expect_that(res$Hb[7], equals(241/198))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(195/206))
  expect_that(res$Hb[10], equals(179/183))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(457))
  expect_that(res$MPH[3], equals(609.5))
  expect_that(res$MPH[4], equals(379.5))
  expect_that(res$MPH[5], equals(371.5))
  expect_that(res$MPH[6], equals(220))
  expect_that(res$MPH[7], equals(219.5))
  expect_that(res$MPH[8], equals(319))
  expect_that(res$MPH[9], equals(200.5))
  expect_that(res$MPH[10], equals(181))

    
  # TEST 02 -------------------------------------------------------------------
  # Test normal calculation of hb=2.
  
  # Analyse dataframe.
  res <- calculateHb(data=set2, ref=ref2, hb=2, kit="SGMplus", ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Dye))
  expect_false(is.null(res$Delta))
  expect_false(is.null(res$LMW))
  expect_false(is.null(res$HMW))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$Hb))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Delta)))
  expect_false(any(is.na(res$LMW)))
  expect_false(any(is.na(res$HMW)))
  expect_false(any(is.na(res$MPH)))
  expect_false(any(is.na(res$Hb)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(2))
  expect_that(res$Delta[3], equals(1))
  expect_that(res$Delta[4], equals(15.2))
  expect_that(res$Delta[5], equals(4))
  expect_that(res$Delta[6], equals(3))
  expect_that(res$Delta[7], equals(2))
  expect_that(res$Delta[8], equals(1))
  expect_that(res$Delta[9], equals(15.2))
  expect_that(res$Delta[10], equals(4))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(460/402))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(632/587))
  expect_that(res$Hb[4], equals(398/361))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(225/215))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(326/312))
  expect_that(res$Hb[9], equals(206/195))
  expect_that(res$Hb[10], equals(183/179))

  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(457))
  expect_that(res$MPH[3], equals(609.5))
  expect_that(res$MPH[4], equals(379.5))
  expect_that(res$MPH[5], equals(371.5))
  expect_that(res$MPH[6], equals(220))
  expect_that(res$MPH[7], equals(219.5))
  expect_that(res$MPH[8], equals(319))
  expect_that(res$MPH[9], equals(200.5))
  expect_that(res$MPH[10], equals(181))

  
  # TEST 03 -------------------------------------------------------------------
  # Test normal calculation of hb=3.
  
  # Analyse dataframe.
  res <- calculateHb(data=set2, ref=ref2, hb=3, kit="SGMplus", ignore.case=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_false(is.null(res$Sample.Name))
  expect_false(is.null(res$Marker))
  expect_false(is.null(res$Dye))
  expect_false(is.null(res$Delta))
  expect_false(is.null(res$Small))
  expect_false(is.null(res$Large))
  expect_false(is.null(res$MPH))
  expect_false(is.null(res$Hb))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Dye)))
  expect_false(any(is.na(res$Delta)))
  expect_false(any(is.na(res$Small)))
  expect_false(any(is.na(res$Large)))
  expect_false(any(is.na(res$MPH)))
  expect_false(any(is.na(res$Hb)))
  
  # Check result: Repeat unit difference.
  expect_that(res$Delta[1], equals(3))
  expect_that(res$Delta[2], equals(2))
  expect_that(res$Delta[3], equals(1))
  expect_that(res$Delta[4], equals(15.2))
  expect_that(res$Delta[5], equals(4))
  expect_that(res$Delta[6], equals(3))
  expect_that(res$Delta[7], equals(2))
  expect_that(res$Delta[8], equals(1))
  expect_that(res$Delta[9], equals(15.2))
  expect_that(res$Delta[10], equals(4))
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(361/398))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(215/225))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(195/206))
  expect_that(res$Hb[10], equals(179/183))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(431))
  expect_that(res$MPH[2], equals(457))
  expect_that(res$MPH[3], equals(609.5))
  expect_that(res$MPH[4], equals(379.5))
  expect_that(res$MPH[5], equals(371.5))
  expect_that(res$MPH[6], equals(220))
  expect_that(res$MPH[7], equals(219.5))
  expect_that(res$MPH[8], equals(319))
  expect_that(res$MPH[9], equals(200.5))
  expect_that(res$MPH[10], equals(181))
  
  
  # TEST 04 -------------------------------------------------------------------
  # Test that two equally high maximum peaks works.

  # Analyse dataframe.
  res <- calculateHb(data=setEqual, ref=ref2, hb=1,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(1))
  expect_that(res$Hb[2], equals(491/423))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(1))
  expect_that(res$Hb[5], equals(384/359))
  expect_that(res$Hb[6], equals(1))
  expect_that(res$Hb[7], equals(241/198))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(1))
  expect_that(res$Hb[10], equals(179/183))

  # Analyse dataframe.
  res <- calculateHb(data=setEqual, ref=ref2, hb=2,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(1))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(632/587))
  expect_that(res$Hb[4], equals(1))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(1))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(326/312))
  expect_that(res$Hb[9], equals(1))
  expect_that(res$Hb[10], equals(183/179))

  # Analyse dataframe.
  res <- calculateHb(data=setEqual, ref=ref2, hb=3,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(1))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(1))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(1))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(1))
  expect_that(res$Hb[10], equals(179/183))

  
  # TEST 05 -------------------------------------------------------------------
  # Test that unfiltered data works.

  # Analyse dataframe.
  res <- calculateHb(data=setUnfiltered, ref=ref2, hb=1,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(491/423))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(361/398))
  expect_that(res$Hb[5], equals(384/359))
  expect_that(res$Hb[6], equals(215/225))
  expect_that(res$Hb[7], equals(241/198))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(195/206))
  expect_that(res$Hb[10], equals(179/183))
  
  # Analyse dataframe.
  res <- calculateHb(data=setUnfiltered, ref=ref2, hb=2,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(460/402))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(632/587))
  expect_that(res$Hb[4], equals(398/361))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(225/215))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(326/312))
  expect_that(res$Hb[9], equals(206/195))
  expect_that(res$Hb[10], equals(183/179))
  
  # Analyse dataframe.
  res <- calculateHb(data=setUnfiltered, ref=ref2, hb=3,
                             kit="SGMplus", ignore.case=TRUE)
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(402/460))
  expect_that(res$Hb[2], equals(423/491))
  expect_that(res$Hb[3], equals(587/632))
  expect_that(res$Hb[4], equals(361/398))
  expect_that(res$Hb[5], equals(359/384))
  expect_that(res$Hb[6], equals(215/225))
  expect_that(res$Hb[7], equals(198/241))
  expect_that(res$Hb[8], equals(312/326))
  expect_that(res$Hb[9], equals(195/206))
  expect_that(res$Hb[10], equals(179/183))

    
  # TEST 06 -------------------------------------------------------------------
  # Test that different marker order in ref and data works for hb=1.
  
  # Analyse dataframe.
  res <- calculateHb(data=set6, ref=ref61, hb=1, kit = "Fusion")

  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(10166/7985))
  expect_that(res$Hb[2], equals(9708/10860))
  expect_that(res$Hb[3], equals(9975/7715))
  expect_that(res$Hb[4], equals(5653/5835))
  expect_that(res$Hb[5], equals(6329/5730))
  expect_that(res$Hb[6], equals(3829/6831))
  expect_that(res$Hb[7], equals(21648/23147))
  expect_that(res$Hb[8], equals(10570/13931))
  expect_that(res$Hb[9], equals(7275/10028))
  expect_that(res$Hb[10], equals(3774/4312))
  expect_that(res$Hb[11], equals(2772/4196))
  expect_that(res$Hb[12], equals(15485/15388))
  expect_that(res$Hb[13], equals(6422/8630))
  expect_that(res$Hb[14], equals(8769/8860))
  expect_that(res$Hb[15], equals(7703/6465))
  expect_that(res$Hb[16], equals(5132/5203))
  expect_that(res$Hb[17], equals(5063/4745))
  expect_that(res$Hb[18], equals(10837/13568))
  expect_that(res$Hb[19], equals(11850/16027))
  expect_that(res$Hb[20], equals(8669/11523))
  expect_that(res$Hb[21], equals(12385/7159))
  expect_that(res$Hb[22], equals(3341/3471))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))
  
  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  # TEST 07 -------------------------------------------------------------------
  # Test that different marker order in ref and data works for hb=2.

  # Analyse dataframe.
  res <- calculateHb(data=set6, ref=ref61, hb=2, kit = "Fusion")
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(7985/10166))
  expect_that(res$Hb[2], equals(10860/9708))
  expect_that(res$Hb[3], equals(7715/9975))
  expect_that(res$Hb[4], equals(5835/5653))
  expect_that(res$Hb[5], equals(5730/6329))
  expect_that(res$Hb[6], equals(6831/3829))
  expect_that(res$Hb[7], equals(23147/21648))
  expect_that(res$Hb[8], equals(13931/10570))
  expect_that(res$Hb[9], equals(10028/7275))
  expect_that(res$Hb[10], equals(4312/3774))
  expect_that(res$Hb[11], equals(4196/2772))
  expect_that(res$Hb[12], equals(15388/15485))
  expect_that(res$Hb[13], equals(8630/6422))
  expect_that(res$Hb[14], equals(8860/8769))
  expect_that(res$Hb[15], equals(6465/7703))
  expect_that(res$Hb[16], equals(5203/5132))
  expect_that(res$Hb[17], equals(4745/5063))
  expect_that(res$Hb[18], equals(13568/10837))
  expect_that(res$Hb[19], equals(16027/11850))
  expect_that(res$Hb[20], equals(11523/8669))
  expect_that(res$Hb[21], equals(7159/12385))
  expect_that(res$Hb[22], equals(3471/3341))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))

  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  
  # TEST 08 -------------------------------------------------------------------
  # Test that different marker order in ref and data works for hb=3.

  # Analyse dataframe.
  res <- calculateHb(data=set6, ref=ref61, hb=3, kit = "Fusion")
  
  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(7985/10166))
  expect_that(res$Hb[2], equals(9708/10860))
  expect_that(res$Hb[3], equals(7715/9975))
  expect_that(res$Hb[4], equals(5653/5835))
  expect_that(res$Hb[5], equals(5730/6329))
  expect_that(res$Hb[6], equals(3829/6831))
  expect_that(res$Hb[7], equals(21648/23147))
  expect_that(res$Hb[8], equals(10570/13931))
  expect_that(res$Hb[9], equals(7275/10028))
  expect_that(res$Hb[10], equals(3774/4312))
  expect_that(res$Hb[11], equals(2772/4196))
  expect_that(res$Hb[12], equals(15388/15485))
  expect_that(res$Hb[13], equals(6422/8630))
  expect_that(res$Hb[14], equals(8769/8860))
  expect_that(res$Hb[15], equals(6465/7703))
  expect_that(res$Hb[16], equals(5132/5203))
  expect_that(res$Hb[17], equals(4745/5063))
  expect_that(res$Hb[18], equals(10837/13568))
  expect_that(res$Hb[19], equals(11850/16027))
  expect_that(res$Hb[20], equals(8669/11523))
  expect_that(res$Hb[21], equals(7159/12385))
  expect_that(res$Hb[22], equals(3341/3471))
  
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))
  
  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  
  # TEST 09 -------------------------------------------------------------------
  # Test that identical marker order in ref and data works for hb=1.

  res <- calculateHb(data=set6, ref=ref62, hb=1, kit = "Fusion")

  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(10166/7985))
  expect_that(res$Hb[2], equals(9708/10860))
  expect_that(res$Hb[3], equals(9975/7715))
  expect_that(res$Hb[4], equals(5653/5835))
  expect_that(res$Hb[5], equals(6329/5730))
  expect_that(res$Hb[6], equals(3829/6831))
  expect_that(res$Hb[7], equals(21648/23147))
  expect_that(res$Hb[8], equals(10570/13931))
  expect_that(res$Hb[9], equals(7275/10028))
  expect_that(res$Hb[10], equals(3774/4312))
  expect_that(res$Hb[11], equals(2772/4196))
  expect_that(res$Hb[12], equals(15485/15388))
  expect_that(res$Hb[13], equals(6422/8630))
  expect_that(res$Hb[14], equals(8769/8860))
  expect_that(res$Hb[15], equals(7703/6465))
  expect_that(res$Hb[16], equals(5132/5203))
  expect_that(res$Hb[17], equals(5063/4745))
  expect_that(res$Hb[18], equals(10837/13568))
  expect_that(res$Hb[19], equals(11850/16027))
  expect_that(res$Hb[20], equals(8669/11523))
  expect_that(res$Hb[21], equals(12385/7159))
  expect_that(res$Hb[22], equals(3341/3471))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))

  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  
  # TEST 10 -------------------------------------------------------------------
  # Test that identical marker order in ref and data works for hb=2.
  
  # Analyse dataframe.
  res <- calculateHb(data=set6, ref=ref62, hb=2, kit = "Fusion")

  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(7985/10166))
  expect_that(res$Hb[2], equals(10860/9708))
  expect_that(res$Hb[3], equals(7715/9975))
  expect_that(res$Hb[4], equals(5835/5653))
  expect_that(res$Hb[5], equals(5730/6329))
  expect_that(res$Hb[6], equals(6831/3829))
  expect_that(res$Hb[7], equals(23147/21648))
  expect_that(res$Hb[8], equals(13931/10570))
  expect_that(res$Hb[9], equals(10028/7275))
  expect_that(res$Hb[10], equals(4312/3774))
  expect_that(res$Hb[11], equals(4196/2772))
  expect_that(res$Hb[12], equals(15388/15485))
  expect_that(res$Hb[13], equals(8630/6422))
  expect_that(res$Hb[14], equals(8860/8769))
  expect_that(res$Hb[15], equals(6465/7703))
  expect_that(res$Hb[16], equals(5203/5132))
  expect_that(res$Hb[17], equals(4745/5063))
  expect_that(res$Hb[18], equals(13568/10837))
  expect_that(res$Hb[19], equals(16027/11850))
  expect_that(res$Hb[20], equals(11523/8669))
  expect_that(res$Hb[21], equals(7159/12385))
  expect_that(res$Hb[22], equals(3471/3341))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))
  
  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  
  # TEST 11 -------------------------------------------------------------------
  # Test that identical marker order in ref and data works for hb=3.
  
  # Analyse dataframe.
  res <- calculateHb(data=set6, ref=ref62, hb=3, kit = "Fusion")

  # Check result: Heterozygous balance.
  expect_that(res$Hb[1], equals(7985/10166))
  expect_that(res$Hb[2], equals(9708/10860))
  expect_that(res$Hb[3], equals(7715/9975))
  expect_that(res$Hb[4], equals(5653/5835))
  expect_that(res$Hb[5], equals(5730/6329))
  expect_that(res$Hb[6], equals(3829/6831))
  expect_that(res$Hb[7], equals(21648/23147))
  expect_that(res$Hb[8], equals(10570/13931))
  expect_that(res$Hb[9], equals(7275/10028))
  expect_that(res$Hb[10], equals(3774/4312))
  expect_that(res$Hb[11], equals(2772/4196))
  expect_that(res$Hb[12], equals(15388/15485))
  expect_that(res$Hb[13], equals(6422/8630))
  expect_that(res$Hb[14], equals(8769/8860))
  expect_that(res$Hb[15], equals(6465/7703))
  expect_that(res$Hb[16], equals(5132/5203))
  expect_that(res$Hb[17], equals(4745/5063))
  expect_that(res$Hb[18], equals(10837/13568))
  expect_that(res$Hb[19], equals(11850/16027))
  expect_that(res$Hb[20], equals(8669/11523))
  expect_that(res$Hb[21], equals(7159/12385))
  expect_that(res$Hb[22], equals(3341/3471))
  
  # Check result: Mean peak height.
  expect_that(res$MPH[1], equals(9075.5))
  expect_that(res$MPH[2], equals(10284))
  expect_that(res$MPH[3], equals(8845))
  expect_that(res$MPH[4], equals(5744))
  expect_that(res$MPH[5], equals(6029.5))
  expect_that(res$MPH[6], equals(5330))
  expect_that(res$MPH[7], equals(22397.5))
  expect_that(res$MPH[8], equals(12250.5))
  expect_that(res$MPH[9], equals(8651.5))
  expect_that(res$MPH[10], equals(4043))
  expect_that(res$MPH[11], equals(3484))
  expect_that(res$MPH[12], equals(15436.5))
  expect_that(res$MPH[13], equals(7526))
  expect_that(res$MPH[14], equals(8814.5))
  expect_that(res$MPH[15], equals(7084))
  expect_that(res$MPH[16], equals(5167.5))
  expect_that(res$MPH[17], equals(4904))
  expect_that(res$MPH[18], equals(12202.5))
  expect_that(res$MPH[19], equals(13938.5))
  expect_that(res$MPH[20], equals(10096))
  expect_that(res$MPH[21], equals(9772))
  expect_that(res$MPH[22], equals(3406))
  
  # Check result: Allele repeat difference.
  expect_that(res$Delta[1], equals(1))
  expect_that(res$Delta[2], equals(1))
  expect_that(res$Delta[3], equals(2))
  expect_that(res$Delta[4], equals(3))
  expect_that(res$Delta[5], equals(2))
  expect_that(res$Delta[6], equals(1))
  expect_that(res$Delta[7], equals(1))
  expect_that(res$Delta[8], equals(3))
  expect_that(res$Delta[9], equals(3))
  expect_that(res$Delta[10], equals(1))
  expect_that(res$Delta[11], equals(2))
  expect_that(res$Delta[12], equals(1))
  expect_that(res$Delta[13], equals(3))
  expect_that(res$Delta[14], equals(3))
  expect_that(res$Delta[15], equals(2))
  expect_that(res$Delta[16], equals(1))
  expect_that(res$Delta[17], equals(2))
  expect_that(res$Delta[18], equals(3))
  expect_that(res$Delta[19], equals(2))
  expect_that(res$Delta[20], equals(1))
  expect_that(res$Delta[21], equals(2))
  expect_that(res$Delta[22], equals(3))
  
  
})