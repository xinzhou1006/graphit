# The PlotlyBivariateZrn function makes makes an interactive bivariate 
# scatter plot of zrn LA-ICPMS data colored by temperature.
# Updated 2018.08.22 CH.

# INPUTS:  dataframe      = data frame containing values for x and y, 
#                           in form DF, defaults to DF
#          x.value        = variable to plot on x axis, in form EuEustar
#          x.name         = character string to label x axis, in form "Eu/Eu*"
#          y.value        = variable to plot on y axis, in form U
#          y.name         = character string to label y axis, in form "U (ppm)"
#          colorby        = variable by which to color the points on the plot, 
#                           in form sample, defaults to temperature
#          colorby.name   = character string to label color bar, 
#                           in form "Age (Ma)", defaults to "Temperature (°C)"
#          symbolby       = variable by which to select a symbol for the points
#                           on the plot, in form U, defaults to sample
# OUTPUTS: bivariate.plotly.zrn = graph of x.value vs. y.value

PlotlyBivariateZrn <- function(dataframe = DF,  
                               x.value, 
                               x.name, 
                               y.value, 
                               y.name, 
                               colorby = temperature, 
                               colorby.name = "Temperature (°C)",
                               symbolby = sample) {
  
  # get column from data frame associated with column name input into function
  x.value <- eval(substitute(x.value), dataframe, parent.frame())
  y.value <- eval(substitute(y.value), dataframe, parent.frame())
  colorby <- eval(substitute(colorby), dataframe, parent.frame())
  symbolby <- eval(substitute(symbolby), dataframe, parent.frame())
  
  # Make plot.
  bivariate.plotly.zrn <- plot_ly(data = dataframe, 
                                  x = ~x.value,
                                  y = ~y.value,
                                  name = "sample.size.spot",
                                  type = "scatter", 
                                  mode = "markers",
                                  marker = list(size = 12, 
                                                line = list(color = "black", 
                                                            width = 1)),
                                  color = ~colorby, 
                                  colors = rev(brewer.pal
                                               (n = 11, name = "Spectral")),
                                  symbol = ~symbolby,
                                  symbols = c('circle',
                                              'circle',
                                              'square',
                                              'triangle-up',
                                              'diamond'),
                                  hoverinfo = 'text',
                                  text = ~paste(sample.size.spot,
                                                '<br>LA-ICPMS age: ',
                                                Age_round,
                                                ' ± ',
                                                Age2sig_round,
                                                ' Ma<br>Temperature: ',
                                                temperature_r, '°C',
                                                sep = '')) %>%
    colorbar(title = colorby.name) %>%
    layout(xaxis = list(title = x.name),
           yaxis = list(title = y.name))
  
  # Return plot.
  return (bivariate.plotly.zrn)
} 

# Example interactive bivariate plot with TIMS using the function above
# example.plot <- PlotlyBivariateZrn(dataframe = DF,
#                                    x.value = U,
#                                    x.name = "U (ppm)",
#                                    y.value = sumREE,
#                                    y.name = "Total REE (ppm)")
# example.plot # display the plot
