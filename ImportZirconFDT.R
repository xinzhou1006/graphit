# The ImportZirconFDT function takes a LA-ICPMS final data table and imports it 
# and cleans it up. 
# Updated 2018.07.25 CO.

# INPUTS:  filepath = type file.choose() into the console 
#                     to determine the file path of csv to import
# OUTPUTS: DF       = cleaned up dataframe

ImportZirconFDT <- function(filepath){
  # read in csv as a data frame, replace all "#DIV/0!" with "NA"
  DF <- read.csv(filepath, na.strings = c("NA", "#DIV/0!")) 
  # remove all columns that are all NA
  DF <- DF[,colSums(is.na(DF))<nrow(DF)] 
  # remove all rows that have more than 15 NAs 
  # (likely an empty row or a very bad analysis)
  DF <- DF[rowSums(is.na(DF))<15,]
  # Remove rows of calculations (chondrite normalization, ratios, temperature)
  DF <- DF[, 1:57]
  # Remove duplicate U and Th concentrations and Th/U ratio.
  DF <- DF[, -c(3, 4, 6)]
  # Rename columns.
  colnames(DF) <- c("analysis", "notes", "Pbstar","X_206Pb204Pb", 
                    "X_208Pb232Th", "X_208Pb232Th_2sig", "X_206Pb207Pb", 
                    "X_206Pb207Pb_2sig", "X_207Pb235U", "X_207Pb235U_2sig", 
                    "X_206Pb238U", "X_206Pb238U_2sig", "error_corr_1", 
                    "X_238U206Pb", "X_238U206Pb_2sig", "X_207Pb206Pb", 
                    "X_207Pb206Pb_2sig", "error_corr_2", "X_208Pb232Th_age", 
                    "X_208Pb232Th_age_2sig", "X_207Pb206Pb_age", 
                    "X_207Pb206Pb_age_2sig", "X_207Pb235U_age", 
                    "X_207Pb235U_age_2sig", "X_206Pb238U_age", 
                    "X_206Pb238U_age_2sig", "percent_disc", 
                    "X_206Pb238U_age_207Pbcorr", 
                    "X_206Pb238U_age_207Pbcorr_2sig", 
                    "X_206Pb238U_age_208Pbcorr", 
                    "X_206Pb238U_age_208Pbcorr_2sig", "P", "Ti", "Y", "Zr", 
                    "Nb", "La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", 
                    "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "Th", "U")
  # Remove rows 1-6 (headers).
  DF <- DF[-(1:6), ] 
  # Remove standards.
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST 610")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "NIST 612")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "AUSZ2")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "AUS Z2")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "R33")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "PL")))
  # Remove notes at bottom of FDT and blank rows,
  DF <- subset(DF, analysis != "")  # Remove blank rows.
  DF <- subset(DF, !(startsWith(as.character(analysis), "Notes:")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "Isotope ratio")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "Trace element conce")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "Sweep-by-sweep")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "Backgrounds were")))
  DF <- subset(DF, !(startsWith(as.character(analysis), "Ablation used")))
  # Reset rownames.
  rownames(DF) <- seq(length = nrow(DF))
  # Make analysis and notes columns into character strings.
  DF$analysis <- as.character(DF$analysis)
  DF$notes <- as.character(DF$notes)
  # Make factors numeric.
  indx <- sapply(DF, is.factor)
  DF[indx] <- lapply(DF[indx], function(x) as.numeric(as.character(x)))
  # Return a cleaned up data frame.
  return(DF) 
}