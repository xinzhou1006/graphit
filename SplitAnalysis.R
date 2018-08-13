# The SplitAnalysis function takes the long analysis names from the LA-ICPMS 
# and splits them into sample name, mount size, and spot number. 
# Updated 2018.04.19 CO.

# INPUTS:  column.name = analysis column from LA-ICPMS output sheet 
#          split       = character to split strings by; 
#                        default is any number of spaces
# OUTPUTS: samp.name   = sample name
#          size        = mount size
#          spot        = spot number

SplitAnalysis <- function(column.name, split = ' +'){ 
  
  # make sure the analysis column is a vector of strings
  column.name <- as.character(column.name)
  sample.name <- vector(length = length(column.name))  # preallocate
  mount.size <- sample.name; spot.number <- sample.name  # preallocate
  
  # for loop to iterate over all samples and split all analysis strings
  for (i in 1:length(column.name)){  # for all columns
    # split string by split variable
    temp <- strsplit(x = column.name[i], split = split)
    # save the first value of the split string as the sample name
    sample.name[i] <- unlist(temp)[1] 
    # save the second value of the split string as the mount size
    mount.size[i] <- unlist(temp)[2] 
    # save the third value of the split string as the spot number
    spot.number[i] <- unlist(temp)[3]} 
  
  # return a list of sample name, mount size, and spot number
  return(list(samp.name = sample.name, size = mount.size, spot = spot.number))
} 
