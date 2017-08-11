################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 07.08.2017: Added audit trail.
# 18.09.2016: Now retains information in 'File.Name' and 'File.Time' when add markers.
# 18.09.2016: Fixed attribute kit not save correct.
# 06.09.2016: Added check for conflicting options filter.alleles and add.missing.loci.
# 28.08.2016: Added option to use word boundaries for sample name matching.
# 28.08.2016: Added options to remove sex markers and quality sensors.
# 28.04.2016: Fixed numeric 'Allele' in 'ref' dataset not converted to character.
# 09.01.2016: Added more attributes to result.
# 16.12.2015: Added attributes to result and improved use of 'grepl'.
# 15.12.2015: Added option to use 'exact' matching of sample names.
# 29.08.2015: Added importFrom.
# 09.04.2015: Added option 'invert' to filter peaks NOT in reference.
# 15.12.2014: Changed parameter names to format: lower.case
# 22.01.2014: Fixed bug. add.missing.loci=TRUE now overrides keep.na=FALSE.
# 10.12.2013: Fixed bug returning all NAs when add.missing.loci=TRUE.
# 08.12.2013: Does not discard columns anymore.
# 08.12.2013: Possible to use a 'ref' without 'Sample.Name' i.e. one profile
#             for all samples in 'data'.
# 06.06.2013: Fixed bug in checking for 'fat' data.
# 03.06.2013: Fixed bug discarding NA loci when add.missing.loci=TRUE.
# 28.04.2013: Fixed "NA" bug (NA worked but not "NA").

#' @title Filter Profile
#'
#' @description
#' Filter peaks from profiles.
#'
#' @details
#' Filters out the peaks matching (or not matching) specified known profiles
#' from typing data containing 'noise' such as stutters.
#' If 'ref' does not contain a 'Sample.Name' column it will be used
#' as reference for all samples in 'data'. The 'invert' option filters out
#' peaks NOT matching the reference (e.g. drop-in peaks). Sex markers and 
#' quality sensors can be removed.
#' NB! add.missing.loci overrides keep.na.
#' Returns data where allele names match/not match 'ref' allele names.
#' Required columns are: 'Sample.Name', 'Marker', and 'Allele'.
#' 
#' @param data data frame with genotype data in 'slim' format.
#' @param ref data frame with reference profile in 'slim' format.
#' @param keep.na logical. FALSE discards NA alleles.
#'  TRUE keep loci/sample even if no matching allele.
#' @param add.missing.loci logical. TRUE add loci present in ref but not in data.
#' Overrides keep.na=FALSE.   
#' @param ignore.case logical TRUE ignore case.
#' @param word logical TRUE adds word boundaries when matching sample names.
#' @param exact logical TRUE use exact matching of sample names.
#' @param invert logical TRUE filter peaks NOT matching the reference.
#' @param sex.rm logical TRUE removes sex markers defined by 'kit'.
#' @param qs.rm logical TRUE removes quality sensors defined by 'kit'.
#' @param kit character string defining the kit used.
#' If NULL automatic detection will be attempted.
#' @param filter.allele logical TRUE filter known alleles. FALSE increase the
#' performance if only sex markers or quality sensors should be removed.
#' @param debug logical indicating printing debug information.
#' 
#' 
#' @export
#' 
#' @importFrom plyr rbind.fill
#' @importFrom utils str
#' 
#' @return data.frame with extracted result.
#' 

filterProfile <- function(data, ref=NULL, add.missing.loci=FALSE, keep.na=FALSE,
                          ignore.case=TRUE, exact=FALSE, word=FALSE,
                          invert=FALSE, sex.rm=FALSE, qs.rm=FALSE, kit=NULL,
                          filter.allele=TRUE, debug=FALSE){
  
  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)
  attr_ref <- substitute(ref)
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(str(data))
    print("ref:")
    print(str(ref))
    print("add.missing.loci:")
    print(add.missing.loci)
    print("keep.na:")
    print(keep.na)
    print("ignore.case:")
    print(ignore.case)
    print("exact:")
    print(exact)
    print("invert:")
    print(invert)
    print("word:")
    print(word)
    print("sex.rm:")
    print(sex.rm)
    print("qs.rm:")
    print(qs.rm)
    print("kit:")
    print(kit)
  }

  # CHECK DATA ----------------------------------------------------------------
  
  # Check if slim format.
  if(sum(grepl("Allele", names(ref))) > 1){
    stop("'ref' must be in 'slim' format.", call. = TRUE)
  }
  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format.", call. = TRUE)
  }
  
  # Check dataset.
  if(!"Sample.Name" %in% names(data)){
    stop("'data' must contain a column 'Sample.Name'.", call. = TRUE)
  }
  
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'.", call. = TRUE)
  }
  
  if(filter.allele){
    if(!"Allele" %in% names(data)){
      stop("'data' must contain a column 'Allele'.", call. = TRUE)
    }
  }

  # Check reference dataset.
  if(!is.null(ref)){
    
    if(!"Sample.Name" %in% names(ref)){
      message("'ref' does not contain a column 'Sample.Name'.")
      message("The same reference will be used for all samples.")
    }
    if(!"Marker" %in% names(ref)){
      stop("'ref' must contain a column 'Marker'.", call. = TRUE)
    }
    if(!"Allele" %in% names(ref)){
      stop("'ref' must contain a column 'Allele'.", call. = TRUE)
    }
    
  }
  
  # Check logical flags.
  if(!is.logical(add.missing.loci)){
    stop("'add.missing.loci' must be logical.", call. = TRUE)
  }
  if(!is.logical(keep.na)){
    stop("'keep.na' must be logical.", call. = TRUE)
  }
  if(!is.logical(ignore.case)){
    stop("'ignore.case' must be logical.", call. = TRUE)
  }
  if(!is.logical(exact)){
    stop("'exact' must be logical.", call. = TRUE)
  }
  if(!is.logical(invert)){
    stop("'invert' must be logical.", call. = TRUE)
  }
  if(!is.logical(word)){
    stop("'word' must be logical.", call. = TRUE)
  }
  if(!is.logical(sex.rm)){
    stop("'sex.rm' must be logical.", call. = TRUE)
  }
  if(!is.logical(qs.rm)){
    stop("'qs.rm' must be logical.", call. = TRUE)
  }
  if(!is.logical(filter.allele)){
    stop("'filter.allele' must be logical.", call. = TRUE)
  }
  
  # PREPARE -------------------------------------------------------------------

  # Check if character data.
  if("Allele" %in% names(ref)){
    if(!is.character(ref$Allele)){
      
      message("'Allele' must be character. 'ref' converted.")
      
      ref$Allele <- as.character(ref$Allele)
      
    }
  }

  if("Allele" %in% names(data)){
    if(!is.character(data$Allele)){
      
      message("'Allele' must be character. 'data' converted.")
      
      data$Allele <- as.character(data$Allele)
      
    }
  }

  # Check conflicting options.
  if(!filter.allele & add.missing.loci){
    
    message("'filter.allele' overrides 'add.missing.loci'. Setting add.missing.loci=FALSE")
    add.missing.loci <- FALSE
    
  }
  # Check conflicting options.
  if(add.missing.loci & !keep.na){
    
    message("'add.missing.loci' overrides 'keep.na'. Setting keep.na=TRUE")
    
    keep.na=TRUE
    
  }
  # Check conflicting options.
  if(is.null(ref) & filter.allele){
    
    message("'ref' cannot be NULL if 'filter.allele=TRUE'. Setting filter.allele=FALSE")
    filter.allele <- FALSE
    
  }
  
  # Remove sex markers. 
  if(sex.rm){
    
    # Check if kit is provided.    
    if(is.null(kit)){
      
      message("No kit defined. Attempt to auto detect:")
      
      kit <-detectKit(data)[1]
      
      message("Using kit=", kit)
      
      # Check kit.  
      if(is.na(kit)){
        stop("No matching kit was found in the kit definition file.")
      }
      
    }

    message("Removing sex markers defined in kit: ", kit, ".")
    
    # Get sex markers.    
    sexMarkers <- getKit(kit = kit, what = "Sex.Marker")
    
    if(debug){
      print("Sex markers:")
      print(sexMarkers)
    }
    
    message("Removing sex markers from dataset:")
    # Loop through and remove all sex markers.
    for(i in seq(along = sexMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != sexMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2,
              " rows with Marker=", sexMarkers[i], ".")
      
    }
    
    if(!is.null(ref)){
      
      # Loop through and remove all sex markers.
      message("Removing sex markers from reference dataset:")
      for(i in seq(along = sexMarkers)){
        
        tmp1 <- nrow(ref)
        
        ref <- ref[ref$Marker != sexMarkers[i],]
        
        tmp2 <- nrow(ref)
        
        message("Removed ", tmp1 - tmp2,
                " rows with Marker=", sexMarkers[i], ".")
        
      }
      
    }
    
  } # End remove sex markers.
  
  # Remove quality sensors. 
  if(qs.rm){
    
    # Check if kit is provided.    
    if(is.null(kit)){
      
      message("No kit defined. Attempt to auto detect:")
      
      kit <-detectKit(data)[1]
      
      message("Using kit=", kit)
      
      # Check kit.  
      if(is.na(kit)){
        stop("No matching kit was found in the kit definition file.")
      }
      
    }
    
    message("Removing quality sensors defined in kit: ", kit, ".")
    
    # Get quality sensors.
    qsMarkers <- getKit(kit = kit, what = "Quality.Sensor")
    
    if(debug){
      print("Quality sensors:")
      print(qsMarkers)
    }
    
    # Loop through and remove all quality sensors.
    message("Removing quality sensors from dataset:")
    for(i in seq(along = qsMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != qsMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2,
              " rows with Marker=", qsMarkers[i], ".")
      
    }
    
    if(!is.null(ref)){
      
      # Loop through and remove all quality sensors.
      message("Removing quality sensors from reference dataset:")
      for(i in seq(along = qsMarkers)){
        
        tmp1 <- nrow(ref)
        
        ref <- ref[ref$Marker != qsMarkers[i],]
        
        tmp2 <- nrow(ref)
        
        message("Removed ", tmp1 - tmp2,
                " rows with Marker=", qsMarkers[i], ".")
        
      }
      
    }
    
  } # End remove quality sensors.

  # FILTER --------------------------------------------------------------------
  
  if(filter.allele){

    # SELECT METHOD -------------------------------------------------------------
    
    if(!add.missing.loci & !keep.na & !invert){
      # Fast method cannot be used if add.missingloci/keep.na/invert is TRUE.
      
      # 'FAST' METHOD -----------------------------------------------------------
      # NB! Discards all NA alleles/loci/samples.
      
      message("Using 'fast' allele filtering method.")
      message("This method discards NA alleles/loci/samples.")
      
      # Clean NA (both 'false' and true NA).
      naAllele <- length(data$Allele[data$Allele=="NA"])
      
      if(naAllele > 0){
        
        data$Allele[data$Allele == "NA"] <- NA
        
        message(naAllele, " \"NA\" in 'Allele' converted to NA")
        
      }
      
      # Check if NA in alleles.
      naAllele <- sum(is.na(data$Allele))
      if(naAllele > 0){
        
        data <- data[!is.na(data$Allele), ]
        
        message("Removed ", naAllele, " rows where Allele=<NA>")
        
      }
      
      # Get reference names.
      if("Sample.Name" %in% names(ref)){
        
        # Get reference names from reference dataset.
        refSampleNames <- unique(ref$Sample.Name)
        
      } else {
        
        # Get reference names from dataset.
        refSampleNames <- unique(data$Sample.Name)
        
      }
      
      if(word){
        refSampleNames <- paste("\\b", refSampleNames, "\\b", sep="")
        message("Word boundaries applied to reference dataset.")
      }
      
      # Add regex for exact or word matching.
      if(exact){
        refSampleNames <- paste("^", refSampleNames, "$", sep="")
        message("Exact matching applied to reference dataset.")
      }
      
      if(debug){
        print("ref samples:")
        print(refSampleNames)
        print("data samples:")
        print(unique(data$Sample.Name))
      }
      
      # Initiate boolean match vector to FALSE.
      matchingData <- is.na(data$Sample.Name)
      
      # Get reference sample i.e. use one reference for all samples.
      # NB! only used if no 'Sample.Name' column in ref.
      currentRef <- ref
      
      # Loop through all reference samples.
      for(s in seq(along = refSampleNames)){
        
        if("Sample.Name" %in% names(ref)){
          
          # Get current reference subset.
          selection <- grepl(refSampleNames[s], ref$Sample.Name,
                             ignore.case = ignore.case)
          currentRef <- ref[selection, ]
          
        }
        
        # Select matching samples.
        selectedSamples <- grepl(refSampleNames[s], data$Sample.Name,
                                 ignore.case = ignore.case)
        
        if(debug){
          print("Current ref:")
          print(refSampleNames[s])
          print("Selected samples:")
          print(unique(data[selectedSamples, ]$Sample.Name))
        }
        
        # Get current marker.
        refMarkers <- unique(currentRef$Marker)
        
        # Loop through all markers.
        for(m in seq(along=refMarkers)){
          
          # Get reference alleles.
          refAlleles <- currentRef$Allele[currentRef$Marker == refMarkers[m]]
          
          # Loop through all alleles.
          for(a in seq(along = refAlleles)){
            
            # Get matching alleles in data.
            mM <- data$Marker == refMarkers[m]
            mA <- data$Allele == refAlleles[a]
            currentMatch <- selectedSamples & mM & mA
            
            # 'Concatenate' booleans
            matchingData <- matchingData | currentMatch
            
          } # End of allele for-loop.
          
        } # End of marker for-loop.
        
      } # End of reference for-loop.
      
      # Create return data frame.
      res <- data[matchingData, ]
      
    } else {
      
      # 'SLOW' METHOD -----------------------------------------------------------
      # NB! Possible to keep NA alleles, add missing loci, and invert.
      
      message("Using 'slow' allele filtering method.")
      message("This method is required to keep NA alleles, add missing loci, or invert.")
      
      # Create an empty data frame to hold the result.
      res <- data.frame(t(rep(NA, length(data))))
      
      # Add column names.
      names(res) <- names(data)
      
      # Remove all NAs
      res  <- res[-1, ]
      
      if(debug){
        print("res:")
        print(res)
      }
      
      # Get reference names.
      if("Sample.Name" %in% names(ref)){
        
        # Get reference names from reference dataset.
        refSampleNames <- unique(ref$Sample.Name)
        
      } else {
        
        # Get reference names from dataset.
        refSampleNames <- unique(data$Sample.Name)
        
      }
      
      # Add regex for exact or word matching.
      if(exact){
        refSampleNames <- paste("^", refSampleNames, "$", sep="")
        message("Exact matching applied to reference dataset.")
      } else if(word){
        refSampleNames <- paste("\\b", refSampleNames, "\\b", sep="")
        message("Word boundaries applied to reference dataset.")
      }
      
      # Get reference sample i.e. use one reference for all samples.
      # NB! only used if no 'Sample.Name' column in ref.
      currentRef <- ref
      
      # Loop through all reference samples.
      for(r in seq(along = refSampleNames)){
        
        if("Sample.Name" %in% names(ref)){
          
          # Get current reference subset.
          selection <- grepl(refSampleNames[r], ref$Sample.Name,
                             ignore.case = ignore.case)
          currentRef <- ref[selection, ]
          
        }
        
        # Select matching samples.
        selectedSamples <- grepl(refSampleNames[r], data$Sample.Name,
                                 ignore.case = ignore.case)
        
        if(debug){
          print("Current ref:")
          print(refSampleNames[r])
          print("Selected samples:")
          print(unique(data[selectedSamples, ]$Sample.Name))
        }
        
        # Get selected samples.
        currentDataSubset <- data[selectedSamples, ]
        
        # Get sample names.
        dataSampleNames<- unique(currentDataSubset$Sample.Name)
        
        # Get current marker.
        refMarkers <- unique(currentRef$Marker)
        
        # Loop over all samples.
        for(s in seq(along=dataSampleNames)){
          
          # Get current sample
          currentData <- currentDataSubset[currentDataSubset$Sample.Name == dataSampleNames[s], ]
          
          # Loop through all markers.
          for(m in seq(along=refMarkers)){
            
            # Get reference alleles.
            refAlleles <- currentRef$Allele[currentRef$Marker==refMarkers[m]]
            
            # Select current marker.
            selection <- currentData$Marker == refMarkers[m]
            tmpDf <- currentData[selection, ]
            
            # dataAlleles is of length 0 if no matching marker.
            if(nrow(tmpDf) == 0 & add.missing.loci){
              
              # Add missing marker, allele will become NA in rbind.fill.
              tmpDf <- data.frame(Sample.Name = dataSampleNames[s],
                                  Marker = refMarkers[m],
                                  stringsAsFactors = FALSE)
              
              message("Missing marker ", refMarkers[m],
                      " added for sample ", dataSampleNames[s])
              
              
              # Attempt to fill additional information required by other functions.
              additionalColumns <- intersect(names(currentData),c("File.Name","File.Time"))

              # Add missing values.              
              for(a in seq(along=additionalColumns)){
                
                addValue <- unique(currentData[additionalColumns[a]])

                if(length(addValue) > 1){
                  message("Multiple candidates for missing information in column, ",
                          additionalColumns[a])
                  message("Using first candidate to fill data.frame: ",
                          paste(addValue, collapse = ", "))
                }
                
                # Add missing value.
                tmpDf[additionalColumns[a]] <- addValue[1]
                
              }

            } else {
              
              # Filter alleles and add to selection.
              if(invert){
                
                # Select peaks not in reference.
                selection <- selection & !currentData$Allele %in% refAlleles
                
              } else {
                
                # Select peaks matching reference.
                selection <- selection & currentData$Allele %in% refAlleles
                
              }
              
              # Get selected data.            
              tmpDf <- currentData[selection, ]
              
              # matching is of length 0 if no matching allele.
              if(nrow(tmpDf) == 0 & keep.na){
                
                # Add missing marker, allele will become NA in rbind.fill.
                tmpDf <- data.frame(Sample.Name = dataSampleNames[s],
                                    Marker = refMarkers[m],
                                    stringsAsFactors = FALSE)
                
                if(debug){
                  print(paste("NA kept for marker", refMarkers[m]))
                }
                
                # Attempt to fill additional information required by other functions.
                additionalColumns <- intersect(names(currentData),c("File.Name","File.Time"))
                
                # Add missing values.              
                for(a in seq(along=additionalColumns)){
                  
                  addValue <- unique(currentData[additionalColumns[a]])

                  if(length(addValue) > 1){
                    message("Multiple candidates for missing information in column, ",
                            additionalColumns[a])
                    message("Using first candidate to fill data.frame: ",
                            paste(addValue, collapse = ", "))
                  }
                  
                  # Add missing value.
                  tmpDf[additionalColumns[a]] <- addValue[1]
                  
                }

              }
              
            }
            
            # Combine result.
            res <- plyr::rbind.fill(res, tmpDf)
            
          } # End of marker for-loop.
          
        } # End of sample for-loop.
        
      } # End of reference for-loop.
      
    }
    
  } else {
    
    message("No allele filtering was performed.")
    
    res <- data
    
  } # End filter alleles.
  
  # Check if Dye is available.
  if("Dye" %in% names(res)){

    # Check for NA's in Dye.
    if(any(is.na(res$Dye))){
      
      # Check if kit is provided.    
      if(is.null(kit)){
        
        message("No kit defined. Attempt to auto detect:")
        
        kit <-detectKit(data)[1]
        
        message("Using kit=", kit)
        
        # Check kit.  
        if(is.na(kit)){
          stop("No matching kit was found in the kit definition file.")
        }
        
      }

      message("Detected NA in 'Dye' colum.")
      message("Adding Dye to result using kit ", kit, ".")

      # Fix broken dyes.      
      res <- addColor(data = res, kit = kit, need = "Dye",
                            overwrite = TRUE, ignore.case = TRUE,
                            debug = debug)
      
    }
    
  }
  
  # Add attributes to result.
  attr(res, which="kit") <- kit
  
  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")
  
  # RETURN --------------------------------------------------------------------
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
	return(res)
  
}
