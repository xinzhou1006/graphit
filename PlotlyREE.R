# The PlotlyREE function makes an interactive chondrite-normalized REE plot 
# with plotly. 
# Updated 2018.08.13 CH.

# INPUTS:  dataframe    = data frame containing REE values, defaults to DF
#          sample.name  = same name for titling plot, in form "14WZ3-2"
#          start.sample = first sample to plot, in form "14WZ3-2 L 183"
#          end.sample   = last sample to plot, in form "14WZ3-2 L 188"
# OUTPUTS: REEgraph     = plotly line graph of REEs

PlotlyREE <- function(dataframe = DF, 
                      start.sample,
                      end.sample){ 
  
  # Set up for REEplot function.
  # rename dataframe for ease of using the code below without rewriting.     
  DF <- dataframe
  # Make REE data frame where each sample is stored in a column
  DF.REE <- data.frame(t(subset(DF, select = c(La_CN, Ce_CN, Pr_CN, Nd_CN, 
                                               Pm_CN, Sm_CN, Eu_CN, Gd_CN, 
                                               Tb_CN, Dy_CN, Ho_CN, Er_CN, 
                                               Tm_CN, Yb_CN, Lu_CN, 
                                               temperature_r)))) 
  # Name DF.REE columns with size and spot info from DF.
  colnames(DF.REE) <- t(DF$sample.size.spot) 
  # Name DF.REE rows with REE labels.
  DF.REE$REE <- c("La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy",
                  "Ho", "Er", "Tm", "Yb", "Lu", "Zr-in-ttn T") 
  # Create a vector of spacing for REE based on ionic radius (Shannon, 1976)
  DF.REE$ionic.radius <- c(1.160, 1.143, 1.126, 1.109, 1.093, 1.079, 1.066,
                           1.053, 1.040, 1.027, 1.015, 1.004, 0.994, 0.985,
                           0.977, NA) 
  # Rearrange colums so it goes position, ionic radius, all the samples.
  DF.REE <- DF.REE[c((ncol(DF.REE) - 1), (ncol(DF.REE)), 1:(ncol(DF.REE) - 2))] 
  # Save column names from DF.REE (needed for binding temperature color codes 
  # later).
  column.labels <- colnames(DF.REE) 
  
  # Assign rgb color codes to temperature values.
  # Save the temperature row from DF.REE as a column in a new dataframe.
  DF.REE.temp <- data.frame(t(DF.REE["temperature_r",])) 
  DF.REE.temp$temperature_r <- 
    as.numeric(as.character(DF.REE.temp$temperature_r))
  # Add arbitrary min and max temperatures (sets palette at about the same  
  # values as other palettes in the rmd document).
  DF.REE.temp["REE","temperature_r"] <- min(DF$temperature_r)
  DF.REE.temp["ionic.radius","temperature_r"] <- max(DF$temperature_r) 
  # Preallocate color value column.
  DF.REE.temp[,"colorvalue"] <- NA
  # Use leaflet package function colorNumeric to create function that gets the 
  # rgb color codes for the the given palette and range.
  pal <- colorNumeric(palette = rev(brewer.pal(n = 9, name = "Spectral")), 
                      domain = DF.REE.temp$temperature_r)
  # Given the palette and domain above, assign color codes based on 
  # temperature_r values and save in data frame.
  DF.REE.temp$colorvalue <- pal(DF.REE.temp$temperature_r) 
  # Make a data frame of transposed DF.REE.temp for coloring REE plots.
  DF.REE.color <- data.frame(t(DF.REE.temp))
  # Rename columns in DF.REE.color to match DF.REE.
  names(DF.REE.color) <- column.labels 
  # Remove temperature column (keeps ionic radius numeric).
  DF.REE <- DF.REE[-c(16), ] 
  # clean up workspace, remove extra values and data frames
  # rm(column.labels, DF.REE.temp) 
  
  # Set up for plotting.     
  # Get column index based on name for start of samples to plot.
  start_col <- which(colnames(DF.REE) == start.sample) 
  # Get column index based on name for end of samples to plot.
  end_col <- which(colnames(DF.REE) == end.sample) 
  
  # Use paste logic to generate a string used to work around plotly and rmd issues
  # with for loops.
  # Make a plot of samples from start.sample to end.sample.
  for(n in start_col:end_col) { 
    # On the first iteration, make the starting string with the plot_ly command.
    if(n == start_col) { 
      select.String <- paste('plot_ly(data = DF.REE, x = ~DF.REE[, "ionic.radius"], y = ~DF.REE[, start_col], name = start.sample, type = "scatter", mode = "lines", line = list(color = DF.REE.color["colorvalue",start.sample], width = 3), hoverinfo = "text", text = ~paste(colnames(DF.REE[', n, ']),"<br>Zr-in-ttn T: ", DF.REE.color["temperature_r",', n, '],"°C"))', sep='') 
      # On every other iteration, add traces for each subsequent spot.
    } else {
      select.String <-paste(select.String,' %>% add_trace(y = ~(DF.REE[,', n, ']), mode = "lines", name = colnames(DF.REE[', n, ']), line = list(color = DF.REE.color["colorvalue",', n, '], width = 3), hoverinfo = "text", text = ~paste(colnames(DF.REE[', n, ']),"<br>Zr-in-ttn T: ", DF.REE.color["temperature_r",', n, '],"°C"))', sep = '') 
    }
  }
  # Set up the layout of the plot.     
  REEgraph <- eval(parse(text=select.String)) %>% 
    layout(xaxis = list(title = 'REE', 
                        autorange = 'reversed', 
                        tickmode = 'array', 
                        tickvals = ~ionic.radius, 
                        ticktext = ~REE),
           yaxis = list(title = 'Sample/Chondrite', 
                        tickmode = 'auto', 
                        type = 'log'))
  return (REEgraph)
}