# The ImportZirconFDT function takes a Boise State University Isotope Geology
# Lab LA-ICPMS final data table (FDT) and imports it cleans it up. REE and trace 
# element calculations are removed, and isotopic calculations from the Excel 
# UPbR reduction are preserved.
# Updated 2018.08.21 CH.

# INPUTS:  filepath = type file.choose() into the console 
#                     to determine the file path of csv to import
# OUTPUTS: DF       = cleaned up dataframe with only sample data
#          DF.std   = cleaned up dataframe with sample and standard data

ImportZirconFDT <- function(filepath){
  # read in csv as a data frame, replace all "#DIV/0!" with "NA"
  DF <- read.csv(filepath, na.strings = c("NA", "#DIV/0!")) 
  # Remove all columns that are all NA.
  DF <- DF[,colSums(is.na(DF)) < nrow(DF)] 
  # Remove all rows that have more than 15 NAs (likely an empty row or a very 
  # bad analysis).
  DF <- DF[rowSums(is.na(DF))<15,]
  # Remove duplicate U and Th concentrations and Th/U ratio.
  DF <- subset(DF, select = -c(X3, X4, X6))
  # Remove columns of repeated sample names and rows that only displays aTiO2.
  DF <- subset(DF, select = -c(X64, X80, X98, X132))
  # Remove chondrite normalized REE values and trace element ratios.
  # These calculations are done in the graphing templates.
  DF <- DF[, -c(55:81)]
  # Remove secondary standard correction data.
  DF <- DF[, -c(91:105)]
  
  # Rename columns.
  colnames(DF) <- c("analysis", "notes", "Pbstar",
                    
                    # Corrected isotope ratios
                    "Ratio_206Pb204Pb",
                    "Ratio_208Pb232Th", "Ratio_208Pb232Th_2sig", 
                    "Ratio_206Pb207Pb", "Ratio_206Pb207Pb_2sig", 
                    "Ratio_207Pb235U",  "Ratio_207Pb235U_2sig", 
                    "Ratio_206Pb238U",  "Ratio_206Pb238U_2sig", 
                    "ErrorCorrelation_1", 
                    "Ratio_238U206Pb",  "Ratio_238U206Pb_2sig", 
                    "Ratio_207Pb206Pb", "Ratio_207Pb206Pb_2sig", 
                    "ErrorCorrelation_2", 
                    
                    # Apparent ages
                    "Age_208Pb232Th", "Age_208Pb232Th_2sig", 
                    "Age_207Pb206Pb", "Age_207Pb206Pb_2sig", 
                    "Age_207Pb235U",  "Age_207Pb235U_2sig", 
                    "Age_206Pb238U",  "Age_206Pb238U_2sig", 
                    "PercentDiscordance",
                    
                    # Common Pb corrected ages
                    "Age_207PbCorrected_206Pb238U", 
                    "Age_207PbCorrected_206Pb238U_2sig", 
                    "Age_208PbCorrected_206Pb238U", 
                    "Age_208PbCorrected_206Pb238U_2sig",
                    
                    # Concentrations
                    "P", "Ti", "Y", "Zr", "Nb", "La", "Ce", "Pr", "Nd", "Sm", 
                    "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", 
                    "Ta", "Th", "U",
                    
                    # Ratios with primary standard calibration errors
                    "Ratio_CalibError_208Pb232Th", 
                    "Ratio_CalibError_208Pb232Th_2sig", 
                    "Ratio_CalibError_206Pb207Pb", 
                    "Ratio_CalibError_206Pb207Pb_2sig", 
                    "Ratio_CalibError_207Pb235U",  
                    "Ratio_CalibError_207Pb235U_2sig", 
                    "Ratio_CalibError_206Pb238U",  
                    "Ratio_CalibError_206Pb238U_2sig", 
                    "ErrorCorrelation_CalibError_1", 
                    "Ratio_CalibError_238U206Pb",  
                    "Ratio_CalibError_238U206Pb_2sig", 
                    "Ratio_CalibError_207Pb206Pb", 
                    "Ratio_CalibError_207Pb206Pb_2sig", 
                    "ErrorCorrelation_CalibError_2",
                    
                    # Apparent ages with primary standard calibration errors
                    "Age_CalibError_208Pb232Th", 
                    "Age_CalibError_208Pb232Th_2sig", 
                    "Age_CalibError_207Pb206Pb", 
                    "Age_CalibError_207Pb206Pb_2sig", 
                    "Age_CalibError_207Pb235U",  
                    "Age_CalibError_207Pb235U_2sig", 
                    "Age_CalibError_206Pb238U",  
                    "Age_CalibError_206Pb238U_2sig", 
                    "PercentDiscordance_CalibError",
                    
                    # Common Pb corrected ages with primary standard calibration
                    # errors
                    "Age_207PbCorrected_CalibError_206Pb238U", 
                    "Age_207PbCorrected_CalibError_206Pb238U_2sig", 
                    "Age_208PbCorrected_CalibError_206Pb238U", 
                    "Age_208PbCorrected_CalibError_206Pb238U_2sig",
                    
                    # Apparent ages with primary standard calibration errors
                    # with bias correction
                    "Age_CalibError_NoBias_206Pb238U",  
                    "Age_CalibError_NoBias_206Pb238U_2sig", 
                    "Age_CalibError_NoBias_PercentDiscordance",
                    "Age_CalibError_PbBias_206Pb238U",  
                    "Age_CalibError_PbBias_206Pb238U_2sig", 
                    "Age_CalibError_PbBias_PercentDiscordance",
                    "Age_CalibError_UBias_206Pb238U",  
                    "Age_CalibError_UBias_206Pb238U_2sig", 
                    "Age_CalibError_UBias_PercentDiscordance")
  
  # Remove rows 1-6 (headers).
  DF <- DF[-(1:6), ] 
  # Remove Notes and extra rows (deleted spots) at the bottom of the FDT.
  DF <- DF[-((which(DF == "Notes:", arr.ind = TRUE)[1]):(dim(DF)[1])), ]
  # Remove blank rows,
  DF <- subset(DF, analysis != "")  
  # Make analysis and notes columns into character strings.
  DF$analysis <- as.character(DF$analysis)
  DF$notes <- as.character(DF$notes)
  # Sort data frame alphabetically by analysis.
  DF <- DF[order(DF$analysis), ]
  # Reset rownames.
  rownames(DF) <- seq(length = nrow(DF))
  # Make factors numeric.
  indx <- sapply(DF, is.factor)
  DF[indx] <- lapply(DF[indx], function(x) as.numeric(as.character(x)))
  rm(indx)
  
  # Save this as a data frame of LA-ICPMS data of samples and standards.
  DF.std <- DF
  
  # Remove standards.
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST 610")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST610")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST 612")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST612")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "AUSZ2")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "AUS Z2")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "R33")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "PL")))

  # Return the cleaned up data frames.
  return(DF, DF.std) 
}