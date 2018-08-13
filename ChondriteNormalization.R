# The ChondriteNormalization function takes a data frame of LA-ICPMS data and 
# creates new columns of chondrite normalized values.
# Updated 2018.07.25 CO.

# INPUTS:  dataframe = data frame containing non-normalized REE values
# OUTPUTS: DF.CN     = data frame with all values that were input into the
#                      function plus the chondrite normalized REE values for
#                      each analysis.

ChondriteNormalization <- function(dataframe){ 
  # Create data frame of REE names and associated chondrite ormalization values. 
  # Values from McDonough and Sun (1995)
  CN <- data.frame(element = c('La', 'Ce', 'Pr', 'Nd', 'Sm',
                               'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 
                               'Er', 'Tm', 'Yb', 'Lu'),
                   normalization.value = c(0.2370, 0.6120, 0.0950, 0.4670, 
                                           0.1530, 0.0580, 0.2055, 0.0374, 
                                           0.2540, 0.0566, 0.1655, 0.0255, 
                                           0.1700, 0.0254))
  # Initialize data frame, rename to make sure to include all input values.
  DF.CN <- dataframe
  # Loop through all rows of CN data frame.
  # Divide the column in the input data frame with the name of the element in 
  # the ith column of the CN data frame by the normalization value in the ith 
  # row of the CN dataframe. Store as a column in the input data frame with the 
  # column name as the element followed by _CN.
  for (i in 1:dim(CN)[1]){  
    DF.CN[[paste(CN[["element"]][i],"_CN",sep='')]] <- 
      DF.CN[,names(DF.CN) == CN[["element"]][i]] / 
      CN[["normalization.value"]][i]
  }
  # calculate (Pm)cn from (Nd)cn and (Sm)cn
  DF.CN$Pm_CN<-sqrt(DF.CN$Nd_CN*DF.CN$Sm_CN)  
  return(DF.CN)
}