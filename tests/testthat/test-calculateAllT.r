context("calculateAllT")

################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 13.07.2018: First version.
# 
# require(strvalidator)
# require(testthat)
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateLb.r")
# test_dir("tests/testthat")

test_that("calculateAllT", {

  # Get test data.
  data(set4)
  data(ref4)

  # Score dropout.
  kit="ESX17"
  set.seed(123) # Set random seed for reproducible result on method X.
  dropout <- suppressMessages(calculateDropout(data=set4, ref=ref4, kit=kit,
                                               ignore.case=TRUE))
  # Calculate average peak height.
  dfH <- suppressMessages(calculateHeight(data = set4, ref = ref4, na.replace = 0,
                         add = FALSE, exclude = "OL", sex.rm = TRUE,
                         qs.rm = TRUE, kit = kit,
                         ignore.case = TRUE, exact = FALSE))
  
  # Add average peak height to dataset.
  dropout <- suppressMessages(addData(data = dropout, new.data = dfH,
                     by.col = "Sample.Name", then.by.col = NULL,
                     exact = TRUE, ignore.case = TRUE))
  
  # TEST 01 -------------------------------------------------------------------
  # Test that the expected result is calculated.
  
  # Analyse dataframe.
  res <- suppressMessages(
    calculateAllT(data = dropout, kit = kit,
                  p.dropout = 0.01, p.conservative = 0.05)
    )

  # Check return class.  
  expect_match(class(res), class(data.frame()))

  # Check that expected Explanatory_variable was recorded.
  expect_equal(res[1,1], "Random (Ph)")
  expect_equal(res[2,1], "LMW (Ph)")
  expect_equal(res[3,1], "HMW (Ph)")
  expect_equal(res[4,1], "Locus (Ph)")
  expect_equal(res[5,1], "Random (H)")
  expect_equal(res[6,1], "LMW (H)")
  expect_equal(res[7,1], "HMW (H)")
  expect_equal(res[8,1], "Locus (H)")
  expect_equal(res[9,1], "Random log(Ph)")
  expect_equal(res[10,1], "LMW log(Ph)")
  expect_equal(res[11,1], "HMW log(Ph)")
  expect_equal(res[12,1], "Locus log(Ph)")
  expect_equal(res[13,1], "Random log(H)")
  expect_equal(res[14,1], "LMW log(H)")
  expect_equal(res[15,1], "HMW log(H)")
  expect_equal(res[16,1], "Locus log(H)")
  
  # Check that expected P(dropout)=0.01@T was calculated.
  expect_equal(res[1,2], 611)
  expect_equal(res[2,2], 495)
  expect_equal(res[3,2], 784)
  expect_equal(res[4,2], 646)
  expect_equal(res[5,2], 531)
  expect_equal(res[6,2], 517)
  expect_equal(res[7,2], 513)
  expect_equal(res[8,2], 533)
  expect_equal(res[9,2], 883)
  expect_equal(res[10,2], 627)
  expect_equal(res[11,2], 1080)
  expect_equal(res[12,2], 897)
  expect_equal(res[13,2], 744)
  expect_equal(res[14,2], 696)
  expect_equal(res[15,2], 672)
  expect_equal(res[16,2], 766)

  # Check that expected P(dropout>0.01)<0.05@T was calculated.
  expect_equal(res[1,3], 1112)
  expect_equal(res[2,3], 811)
  expect_equal(res[3,3], 1514)
  expect_equal(res[4,3], 901)
  expect_equal(res[5,3], 779)
  expect_equal(res[6,3], 767)
  expect_equal(res[7,3], 777)
  expect_equal(res[8,3], 678)
  expect_equal(res[9,3], 2345)
  expect_equal(res[10,3], 1378)
  expect_equal(res[11,3], 3423)
  expect_equal(res[12,3], 1597)
  expect_equal(res[13,3], 1467)
  expect_equal(res[14,3], 1381)
  expect_equal(res[15,3], 1358)
  expect_equal(res[16,3], 1194)
  
  # Check that expected Hosmer-Lemeshow_p was calculated.
  expect_equal(res[1,4], 0.8505)
  expect_equal(res[2,4], 0.9922)
  expect_equal(res[3,4], 0.9992)
  expect_equal(res[4,4], 0.9903)
  expect_equal(res[5,4], 0.9999)
  expect_equal(res[6,4], 0.9915)
  expect_equal(res[7,4], 0.9989)
  expect_equal(res[8,4], 0.9963)
  expect_equal(res[9,4], 0.3480)
  expect_equal(res[10,4], 0.8312)
  expect_equal(res[11,4], 0.8982)
  expect_equal(res[12,4], 0.8022)
  expect_equal(res[13,4], 0.9871)
  expect_equal(res[14,4], 0.9245)
  expect_equal(res[15,4], 0.9650)
  expect_equal(res[16,4], 0.8995)
  
  # Check that expected ß0 was calculated.
  expect_equal(res[1,5], -0.3754)
  expect_equal(res[2,5], 0.2337)
  expect_equal(res[3,5], -0.9888)
  expect_equal(res[4,5], 0.9789)
  expect_equal(res[5,5], 0.7768)
  expect_equal(res[6,5], 0.7341)
  expect_equal(res[7,5], 0.6129)
  expect_equal(res[8,5], 2.4478)
  expect_equal(res[9,5], 6.9153)
  expect_equal(res[10,5], 9.6917)
  expect_equal(res[11,5], 5.2677)
  expect_equal(res[12,5], 11.5748)
  expect_equal(res[13,5], 10.6388)
  expect_equal(res[14,5], 10.8486)
  expect_equal(res[15,5], 10.7495)
  expect_equal(res[16,5], 15.5977)
  
  # Check that expected ß1 was calculated.
  expect_equal(res[1,6], -0.0069)
  expect_equal(res[2,6], -0.0097)
  expect_equal(res[3,6], -0.0046)
  expect_equal(res[4,6], -0.0086)
  expect_equal(res[5,6], -0.0101)
  expect_equal(res[6,6], -0.0103)
  expect_equal(res[7,6], -0.0102)
  expect_equal(res[8,6], -0.0132)
  expect_equal(res[9,6], -1.6970)
  expect_equal(res[10,6], -2.2182)
  expect_equal(res[11,6], -1.4120)
  expect_equal(res[12,6], -2.3783)
  expect_equal(res[13,6], -2.3039)
  expect_equal(res[14,6], -2.3593)
  expect_equal(res[15,6], -2.3572)
  expect_equal(res[16,6], -3.0403)
  
  # Check that expected observed was calculated.
  expect_equal(res[1,7], 381)
  expect_equal(res[2,7], 382)
  expect_equal(res[3,7], 383)
  expect_equal(res[4,7], 367)
  expect_equal(res[5,7], 381)
  expect_equal(res[6,7], 382)
  expect_equal(res[7,7], 383)
  expect_equal(res[8,7], 367)
  expect_equal(res[9,7], 381)
  expect_equal(res[10,7], 382)
  expect_equal(res[11,7], 383)
  expect_equal(res[12,7], 367)
  expect_equal(res[13,7], 381)
  expect_equal(res[14,7], 382)
  expect_equal(res[15,7], 383)
  expect_equal(res[16,7], 367)
  
  # Check that expected dropout was calculated.
  expect_equal(res[1,8], 17)
  expect_equal(res[2,8], 16)
  expect_equal(res[3,8], 15)
  expect_equal(res[4,8], 31)
  expect_equal(res[5,8], 17)
  expect_equal(res[6,8], 16)
  expect_equal(res[7,8], 15)
  expect_equal(res[8,8], 31)
  expect_equal(res[9,8], 17)
  expect_equal(res[10,8], 16)
  expect_equal(res[11,8], 15)
  expect_equal(res[12,8], 31)
  expect_equal(res[13,8], 17)
  expect_equal(res[14,8], 16)
  expect_equal(res[15,8], 15)
  expect_equal(res[16,8], 31)
  
  # TEST 02 -------------------------------------------------------------------
  # Test that input data is checked.
  
  # Dataframe with required column names.
  dfNames <- data.frame(MethodX=NA, Method1=NA, Method2=NA, MethodL=NA, Height=NA, H=NA, MethodL.Ph=NA)
  
  # Missing 'MethodX' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-1], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # Missing 'Method1' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-2], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))

  # Missing 'Method2' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-3], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # Missing 'MethodL' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-4], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # Missing 'Height' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-5], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # Missing 'H' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-6], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # Missing 'MethodL.Ph' column should generate an error.
  expect_error(calculateAllT(data = dfNames[,-7], kit = kit,
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # kit not in kit definition file should generate an error.
  expect_error(calculateAllT(data = dropout, kit = "KitNotDefined",
                             p.dropout = 0.01, p.conservative = 0.05))
  
  # p.dropout > 1 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                  p.dropout = 1.01, p.conservative = 0.05))
  
  # p.dropout < 0 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                             p.dropout = -0.01, p.conservative = 0.05))
  
  # length(p.dropout) > 1 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                             p.dropout = c(0.01, 0.01), p.conservative = 0.05))

  # p.conservative > 1 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                             p.dropout = 0.01, p.conservative = 1.05))
  
  # p.conservative < 0 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                             p.dropout = 0.01, p.conservative = -0.05))
  
  # length(p.conservative) > 1 should generate an error.
  expect_error(calculateAllT(data = dropout, kit = kit,
                             p.dropout = 0.01, p.conservative = c(0.05, 0.05)))
  
  
})
  