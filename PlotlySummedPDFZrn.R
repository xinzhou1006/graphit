# The PlotlySummedPDFZrn function makes makes an interactive PDF plot of 
# LA-ICPMS data.
# Updated 2018.05.21 CO.

# INPUTS:  dataframe        = data frame containing ages, 
#                             in form DF, defaults to DF
# OUTPUTS: plotly.summedPDF = plot of 206Pb/238U LA-ICPMS ages 

PlotlySummedPDFZrn <- function(dataframe = DF) {
  
  # rename dataframe for ease of using the code below without rewriting.     
  DF <- dataframe
  
  # Find max and min ages of y axis.
  max.age <- max(DF$Age) + 5 * median(DF$Age2sig)
  min.age <- min(DF$Age) - 5 * median(DF$Age2sig)
  
  # Use compound.prob function to create a summed pdf from 0 Ma to 500 Ma
  # INPUTS:      ages = a vector of ages
  #              sigs = a vector of 1 sigma uncertainties
  # OUTPUTS:     P    = probability values
  compound.prob <- function(ages, sigs){
    # Divide 2 sigma uncertainties to get 1 sigma uncertainties.
    sigs1 <- sigs / 2  
    x <- seq(min(min.age), max(max.age), length = ((max.age - min.age) * 100))  
    interval <- matrix(0, nrow = length(x), ncol = length(ages))
    for (i in 1:length(ages)) {
      interval[, i] <- dnorm(x, ages[i], 
                             sigs1[i]) / length(ages) * mean(diff(x))
    }
    P <- data.frame(x = x,probability = apply(interval, 1, sum))
    return(P)
  }
  
  # Calculate PDFs of LA-ICPMS data using the compound.prob function above.
  P <- compound.prob(DF$Age, DF$Age2sig) 
  # Scale the column by the max value in the column so the y axis will have a 
  # max value of 1.
  P$probability.scaled <- P$probability / max(P$probability) 
  
  # Make plot.
  plot.summedPDF <- plot_ly(P, 
                            x = ~x, 
                            y = ~probability.scaled, 
                            name = "Age",
                            type = 'scatter', 
                            mode = 'lines', 
                            line = list(dash = "solid", color = "black")) %>%
    layout(xaxis = list(title = "Age (Ma)"), 
           yaxis = list(title = "Relative Probability", 
                        showticklabels = F))
  
  # Return plot.
  return(plot.summedPDF)
}
