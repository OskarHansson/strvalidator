context("calculateH")

################################################################################
#' TODO LIST
#' TODO: ...

################################################################################
#' CHANGE LOG
#' dd.mm.yyyy: ...
#' 
#' test_dir("inst/tests/")
#' test_file("tests/testthat/test-calculateH.r")
#' test_dir("tests/testthat")

test_that("calculateH", {

  # Generate test data.
  markers <- c("D3","vWA","D16","D2","AM","D8","D21","D18","D19","TH01","FGA")
  height.1 <- c(3349, 1273,627,77,7189,3303,582,175,2854,1217,460)
  height.2 <- c(2296,1470,377,NA,NA,3026,737,174,2547,NA,355)
  het <- c(1,1,1,1,0,1,1,1,1,0,1)

  df1 <- data.frame(Sample.Name="MySample",
                   Marker=markers,
                   Height.1=height.1,
                   Height.2=height.2,
                   Heterozygous=het,
                   stringsAsFactors=FALSE)
  
  df1 <- slim(data=df1,
             fix=c("Sample.Name","Marker","Heterozygous"),
             stack=c("Height"))

  # Generate test data.
  markers <- c("D3","vWA","D16","D2","AM","D8","D21","D18","D19","TH01","FGA")
  height.1 <- c(3349, 1273,627,77,7189,3303,582,175,3416,1217,460)
  height.2 <- c(2296,1470,377,NA,NA,3026,737,174,NA,NA,355)
  het <- c(1,1,1,1,0,1,1,1,0,0,1)
  
  df2 <- data.frame(Sample.Name="AnotherSample",
                   Marker=markers,
                   Height.1=height.1,
                   Height.2=height.2,
                   Heterozygous=het,
                   stringsAsFactors=FALSE)
  
  df2 <- slim(data=df2,
             fix=c("Sample.Name","Marker","Heterozygous"),
             stack=c("Height"))

  df2 <- rbind(df1,df2)
  
  # TEST 01 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  
  # Analyse dataframe.
  res <- calculateH(data=df1)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  
  # Check result.
  expect_that(res$H, equals(1528))
  expect_that(res$Peaks, equals(19))

  # TEST 02 -------------------------------------------------------------------
  # Test that analysis of a dataset works.
  
  # Analyse dataframe.
  res <- calculateH(data=df2)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$H[1], equals(1528))
  expect_that(res$H[2], equals(30103/21))
  expect_that(res$Peaks[1], equals(19))  
  expect_that(res$Peaks[2], equals(18)) 
  
})