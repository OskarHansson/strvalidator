context("calculateH")

################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 04.03.2014: Added test for no NA and na!=NULL.
# 25.02.2014: Updated test to change in 'calculateH'. Added more tests.
# 
# test_dir("inst/tests/")
# test_file("tests/testthat/test-calculateH.r")
# test_dir("tests/testthat")

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
  
  # One negative sample.
  df3 <- df2
  df3[df3$Sample.Name == "AnotherSample",]$Height <- as.numeric(NA)
  
  
  # TEST 01 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  
  # Analyse dataframe.
  res <- calculateH(data=df1, na=NULL, add=FALSE)
  
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
  res <- calculateH(data=df2, na=NULL, add=FALSE)
  
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

  # TEST 03 -------------------------------------------------------------------
  # Test that analysis of a dataset with negative samples work.
  
  # Analyse dataframe.
  res <- calculateH(data=df3, na=NULL, add=FALSE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_true(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  
  # Check result.
  expect_that(res$H[1], equals(1528))
  expect_that(res$H[2], equals(as.numeric(NA)))
  expect_that(res$Peaks[1], equals(19))  
  expect_that(res$Peaks[2], equals(0)) 

  # TEST 04 -------------------------------------------------------------------
  # Test that analysis of a dataset with negative samples work,
  # with replacement of NA.
  
  # Analyse dataframe.
  res <- calculateH(data=df3, na=0, add=FALSE)
  
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
  expect_that(res$H[2], equals(0))
  expect_that(res$Peaks[1], equals(19))  
  expect_that(res$Peaks[2], equals(0)) 

  # TEST 05 -------------------------------------------------------------------
  # Test that analysis of one sample works.
  # Add to dataframe.
  
  # Analyse dataframe.
  res <- calculateH(data=df1, na=NULL, add=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Heterozygous", names(res))))
  expect_true(any(grepl("Height", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Heterozygous)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  
  # Check result.
  expect_that(res$Sample.Name, equals(df1$Sample.Name))
  expect_that(res$Marker, equals(df1$Marker))
  expect_that(res$Heterozygous, equals(df1$Heterozygous))
  expect_that(res$Height, equals(as.numeric(df1$Height)))
  expect_that(unique(res$H), equals(1528))
  expect_that(unique(res$Peaks), equals(19))
  
  # TEST 06 -------------------------------------------------------------------
  # Test that analysis of a dataset works.
  # Add to dataframe.
  
  # Analyse dataframe.
  res <- calculateH(data=df2, na=NULL, add=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Heterozygous", names(res))))
  expect_true(any(grepl("Height", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Heterozygous)))
  expect_false(any(is.na(res$Height)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  
  # Check result.
  expect_that(res$Sample.Name, equals(df2$Sample.Name))
  expect_that(res$Marker, equals(df2$Marker))
  expect_that(res$Heterozygous, equals(df2$Heterozygous))
  expect_that(res$Height, equals(as.numeric(df2$Height)))
  expect_that(unique(res$H)[1], equals(1528))
  expect_that(unique(res$H)[2], equals(30103/21))
  expect_that(unique(res$Peaks)[1], equals(19))  
  expect_that(unique(res$Peaks)[2], equals(18)) 
  
  # TEST 07 -------------------------------------------------------------------
  # Test that analysis of a dataset with negative samples work.
  # Add to dataframe.
  
  # Analyse dataframe.
  res <- calculateH(data=df3, na=NULL, add=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Heterozygous", names(res))))
  expect_true(any(grepl("Height", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Heterozygous)))
  expect_true(any(is.na(res$Height)))
  expect_true(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))
  
  # Check result.
  expect_that(res$Sample.Name, equals(df3$Sample.Name))
  expect_that(res$Marker, equals(df3$Marker))
  expect_that(res$Heterozygous, equals(df3$Heterozygous))
  expect_that(res$Height, equals(as.numeric(df3$Height)))
  expect_that(unique(res$H)[1], equals(1528))
  expect_that(unique(res$H)[2], equals(as.numeric(NA)))
  expect_that(unique(res$Peaks)[1], equals(19))  
  expect_that(unique(res$Peaks)[2], equals(0)) 
  
  # TEST 08 -------------------------------------------------------------------
  # Test that analysis of a dataset with negative samples work,
  # with replacement of NA.
  # Add to dataframe.
  
  # Analyse dataframe.
  res <- calculateH(data=df3, na=0, add=TRUE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Heterozygous", names(res))))
  expect_true(any(grepl("Height", names(res))))
  expect_true(any(grepl("H", names(res))))
  expect_true(any(grepl("Peaks", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Heterozygous)))
  expect_true(any(is.na(res$Height)))
  expect_false(any(is.na(res$H)))
  expect_false(any(is.na(res$Peaks)))

  # Check result.
  expect_that(res$Sample.Name, equals(df3$Sample.Name))
  expect_that(res$Marker, equals(df3$Marker))
  expect_that(res$Heterozygous, equals(df3$Heterozygous))
  expect_that(res$Height, equals(as.numeric(df3$Height)))
  expect_that(unique(res$H)[1], equals(1528))
  expect_that(unique(res$H)[2], equals(0))
  expect_that(unique(res$Peaks)[1], equals(19))  
  expect_that(unique(res$Peaks)[2], equals(0)) 

  # TEST 09 -------------------------------------------------------------------
  # Test that analysis work when no NA and na!=NULL.
  
  # Analyse dataframe.
  res <- calculateH(data=df1, na=0, add=FALSE)
  
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
  
  
})