# The CalculateRatios function takes a LA-ICPMS final data table and 
# calculates geochemical ratios. 
# Updated 2018.07.25 CO.

# INPUTS:  dataframe = data frame containing LA-ICPMS data
# OUTPUTS: DF        = data frame with all values that were input into the
#                      function plus the calculated ratios.

CalculateRatios <- function(dataframe){ 
  # Initialize data frame, rename to make sure to include all input values.
  DF <- dataframe
  # Calculate REE and trace element ratios.
  DF$LaNd <- DF$La_CN/DF$Nd_CN  # La/Nd, LREE slope     
  DF$GdLu <- DF$Gd_CN/DF$Lu_CN  # Gd/Lu, HREE slope
  DF$GdYb <- DF$Gd_CN/DF$Yb_CN  # Gd/Yb (chondrite normalized) 
  DF$ZrHf <- DF$Zr/DF$Hf        # Zr/Hf
  DF$LuHf <- DF$Lu/DF$Hf        # Lu/Hf
  DF$HfY <- DF$Hf/DF$Y          # Hf/Y
  DF$YHo <- DF$Y/DF$Ho          # Y/Ho
  DF$ThU <- DF$Th/DF$U          # Th/U
  DF$ThY <- DF$Th/DF$Y          # Th/Y
  DF$NbTa <- DF$Nb/DF$Ta        # Nb/Ta
  DF$NbU <- DF$Nb/DF$U          # Nb/U
  DF$NbTh <- DF$Nb/DF$Th        # Nb/Th
  DF$DyYb <- DF$Dy_CN/DF$Yb_CN  # Dy/Yb
  DF$CeCestar <- DF$Ce_CN/sqrt(DF$La_CN*DF$Pr_CN)   # Ce/Ce*
  DF$EuEustar <- DF$Eu_CN/sqrt(DF$Sm_CN*DF$Gd_CN)   # Eu/Eu*
  DF$DyDystar <- DF$Dy_CN/(DF$La_CN^(4/13))*(DF$Yb_CN^(9/13))  # Dy/Dy*
  # Calculate total REE (not chondrite normalized).
  # NA (below detection limit) treated as 0.
  DF$sumREE   <- rowSums(DF[,c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy",
                               "Ho","Er","Tm","Yb","Lu")], na.rm=T) 
  # Round temperature values to one decimal place (useful for labeling).
  DF$temperature_r <- round(DF$temperature, digits = 1) 
  # Round 6/8 values to one decimal place (useful for labeling).
  DF$X_206Pb238U_age_r <- 
    round(DF$X_206Pb238U_age, digits = 1) 
  # Round 6/8-2sig values to one decimal place (useful for labeling).
  DF$X_206Pb238U_age_2sig_r <- 
    round(DF$X_206Pb238U_age_2sig, digits = 1) 
  return(DF)
}