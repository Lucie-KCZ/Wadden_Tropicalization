###################################
### Community Temperature Index ###
###################################

# Authors list - To be completed
# Lucie Kuczynski, Helmut Hillebrand

# L. Kuczynski
# lucie.kuczynski@hotmail.com
# June, 2024
# Last edit: July 2024

# This script processes biodiversity data from the Wadden Sea to calculate the experienced temperatures per species. 
# It merges temperature data with species occurrence data.

# Clean out your work environment (be cautious, this will remove all objects from the workspace)
for(i in 1:10) gc(reset = TRUE)
rm(list = ls())


# MISC. FUNCTIONS

# Function to calculate the Community Temperature Index (CTI)
get_CTI <- function(community_matrix, STI, infos_col){
  
  # Check for a STI column
  if(!'STI' %in% colnames(STI)) {
    stop('No "STI" column identified in the STI data')
  }
  
  # Calculate relative abundances
  # Apply the function to each row of the community matrix (excluding info columns)
  # For each row, divide each species abundance by the total abundance to get relative abundances
  relative_abundances <- t(apply(community_matrix[, -infos_col], 1, function(x) x / sum(x)))
  
  # Calculate CTI for each sample
  # Apply the function to each row of relative abundances
  # For each row, calculate the mean of the product of relative abundances and species thermal indices (STI)
  output <- apply(relative_abundances, 1, function(z) stats::weighted.mean(x = STI$STI, w = z, na.rm = TRUE))
  
  # Combine CTI results with the original information columns
  output <- data.frame(community_matrix[, infos_col], CTI = output)
  
  # Return the final CTI DataFrame
  return(output)
}

# Plot the global trends
plot_trend <- function(CTI){
  # Set plot parameters
  par(mar = c(10, 7, 2, 2), pty = 's')
  
  CTI_limits <- c(floor(min(CTI$CTI, na.rm = T)), 
                  ceiling(max(CTI$CTI, na.rm = T)))
  year_limits <- range(CTI$Year)
  
  # Create empty plot
  plot(x = 1, y = 1, xlim = year_limits, ylim = CTI_limits, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  # Add axes
  axis(side = 1, tcl = -0.5, lwd = 2, las = 2, at = seq(year_limits[1], year_limits[2], by = 5), cex.axis = 2.5, padj = 0.5)
  axis(side = 2, tcl = -0.5, lwd = 2, las = 1, at = seq(CTI_limits[1], CTI_limits[2], by = 1), cex.axis = 2.5)
  
  # Add axis names
  mtext('Year', side = 1, padj = -1, line = 9, cex = 2.5)
  mtext('CTI', side = 2, padj = -1, line = 3, cex = 2.5)
  
  # Add observed data lines
  by(CTI, CTI$Station, function(x) lines(x$CTI ~ x$Year, col = 'snow2', lwd = 1, lty = 1))
  
  # Load necessary libraries
  library(nlme)
  
  # Fit linear mixed-effects model
  model <- lme(fixed = CTI ~ Year, random = ~ 1 | Station, method = "REML", data = CTI)
  
  # Print model summary
  print(summary(model))
  
  # Get R-squared value for the model
  library(MuMIn)
  print(r.squaredGLMM(model))
  
  # Get predicted values and confidence intervals
  library(ggeffects)
  predictions <- as.data.frame(ggpredict(model, "Year", nsim = 999, type = 'sim'))
  
  # Plot confidence intervals and model predictions
  polygon(y = c(predictions$conf.low, rev(predictions$conf.high), predictions$conf.low[1]),
          x = c(predictions$x, rev(predictions$x), predictions$x[1]), 
          col = ifelse(summary(model)$tTable[2, 5] > .05, 'slategray1', 'violetred1'), 
          border = ifelse(summary(model)$tTable[2, 5] > .05, 'slategray1', 'violetred1'))
  lines(predictions$x, predictions$predicted, lwd = 3,           
        col = ifelse(summary(model)$tTable[2, 5] > .05, 'slategray3', 'violetred4'))
}

# Assess processes' strength
get_processes <- function(community_matrix, STI, CTI, infos_col, all_outputs = T, log = T) {
  
  # Check for required columns in the community_matrix DataFrame
  if(any(!c('Station', 'Year') %in% colnames(community_matrix))) {
    stop('Update the column names in community_matrix to have "Station" and "Year"')
  }
  
  # Initialize empty data frames to store results
  trends <- NULL
  process_strength <- NULL
  
  # Loop through each unique station in the CTI data
  for(station in unique(CTI$Station)) {
    # Print the station being processed
    print(paste('Processing:', station))
    
    # Subset the community matrix for the given station
    community_station <- community_matrix[community_matrix$Station == station, ]
    
    # Remove species with zero abundance across all years
    community_station <- 
      community_station[, -(which(apply(community_station[, -infos_col], 2, sum) == 0) + max(infos_col))]
    
    # Calculate trends in species abundances over time
    if(log){
      trends_station <- as.list(
        as.data.frame(apply(community_station[, -infos_col], 2, 
                            function(x) t(summary(lm(log(x+1) ~ community_station$Year))$coefficients[2, c(1, 4)]))))
      
    } else {
      trends_station <- as.list(
        as.data.frame(apply(community_station[, -infos_col], 2, 
                            function(x) t(summary(lm(x ~ community_station$Year))$coefficients[2, c(1, 4)]))))
    }
    trends_station <- do.call(rbind, trends_station)
    trends_station <- data.frame(
      species = rownames(trends_station), 
      estimate = trends_station[, 1], 
      pvalue = trends_station[, 2], 
      trend = ifelse(trends_station[, 1] > 0, 'increase', 'decrease'))
    
    # Add species thermal index (STI) to the trends data
    trends_station <- merge(trends_station, STI, by = 'species', all.x = TRUE, all.y = FALSE)
    
    # Calculate the average CTI for the station
    avg_CTI_station <- mean(CTI[CTI$Station == station, 'CTI'], na.rm = TRUE)
    
    # Determine the process (tropicalisation, borealisation, etc.)
    trends_station$difference <- trends_station$STI - avg_CTI_station
    trends_station$process <- 
      ifelse(trends_station$trend == 'increase' & trends_station$difference > 0, 'tropicalisation', 
             ifelse(trends_station$trend == 'increase' & trends_station$difference < 0, 'borealisation', 
                    ifelse(trends_station$trend == 'decrease' & trends_station$difference > 0, 'deborealisation', 
                           ifelse(trends_station$trend == 'decrease' & trends_station$difference < 0, 'detropicalisation', NA))))
    
    # Calculate the absolute strength of each process
    absolute_strength_station <- do.call(rbind, as.list(by(
      trends_station, trends_station$process, function(x) sum(abs((x$STI - avg_CTI_station) * x$estimate)))))
    
    # Calculate the relative strength of each process
    process_strength_station <- data.frame(
      Station = station,
      process = rownames(absolute_strength_station), 
      strength = 100 * round(absolute_strength_station / sum(absolute_strength_station, na.rm = TRUE), 3))
    
    # Add the results for the station to the overall results
    trends_station <- data.frame(Station = station, trends_station)
    trends <- rbind(trends, trends_station)
    process_strength <- rbind(process_strength, process_strength_station)
    
    # Clean up temporary variables
    rm(community_station, trends_station, avg_CTI_station, absolute_strength_station, process_strength_station)
  }
  
  if(all_outputs){
    output <- list(trends = trends, process_strength = process_strength)
  } else {
    output <- process_strength
  }
  # Return the final results
  return(output)
}

# Plot species role in the different processes
plot_species <- function(processes) {
  # Load necessary libraries
  require(scales)
  require(ggplot2)
  
  # Remove rows with NA values from the processes DataFrame
  processes <- na.omit(processes)
  
  # Set plot parameters
  par(mar = c(5, 3, 2, 5), oma = c(1, 7, 1, 1), pty = 's', mfrow = c(1, 2))
  
  # Define limits for the scatterplot
  estimate_limits <- c(floor(min(processes$estimate, na.rm = TRUE)), 
                       ceiling(max(processes$estimate, na.rm = TRUE)))
  STI_limits <- c(floor(min(processes$STI, na.rm = TRUE)), 
                  ceiling(max(processes$STI, na.rm = TRUE)))
  
  # Create an empty plot for the scatterplot
  plot(x = 1, y = 1, xlim = STI_limits, ylim = estimate_limits, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  # Customize x-axis
  axis(side = 1, tcl = -0.5, lwd = 2, las = 1, cex.axis = 2.5, padj = .5)
  mtext('STI (°C)', side = 1, line = 4, cex = 2.5)
  
  # Customize y-axis
  axis(side = 2, tcl = -0.5, lwd = 2, las = 2, cex.axis = 2.5)
  mtext('Trends in log-abundances', side = 2, padj = -1, line = 4, cex = 2.5)
  
  # Add a horizontal line at y = 0
  abline(h = 0, lty = 3, lwd = 2)
  
  # Define colors based on the process type
  process_colors <- alpha(
    ifelse(processes$process == 'tropicalisation', 'tomato3', 
           ifelse(processes$process == 'borealisation', 'deepskyblue3', 
                  ifelse(processes$process == 'detropicalisation', 'maroon4', 'sienna1'))), .3)
  
  # Add points to the scatterplot
  points(y = processes$estimate, x = processes$STI, cex = log(1.5 * abs(processes$difference)),
         pch = 21, bg = process_colors)
  
  # Second plot: barplot
  bar_positions <- barplot(sort(table(process_colors), decreasing = TRUE), 
                           col = names(sort(table(process_colors), decreasing = TRUE)), 
                           horiz = TRUE, axes = FALSE, names.arg = NA)
  
  # Customize x-axis of the barplot
  axis(side = 1, font.axis = 1, cex.axis = 2.5, padj = .5)
  
  # Add labels to the bars
  text(x = sort(table(process_colors), decreasing = TRUE),
       y = bar_positions, pos = 2, cex = 2,
       labels = names(sort(table(processes$process), decreasing = TRUE)))
}

# MACROZOOBENTHOS (MZB) DATA 
# Read the community data for Macrozoobenthos, excluding the first column
community <- read.csv('../data/mzb_abundances_wide.csv')[, -1]

# Rename the 2nd and 3rd columns to 'Station' and 'Year'
colnames(community)[2:3] <- c('Station', 'Year')

# Load the local Species Thermal Index (STI) data for Macrozoobenthos
load('../data/mzb_local_STI.RData', v = TRUE)

# Calculate the Community Temperature Index (CTI) for Macrozoobenthos
mzb_CTI <- get_CTI(community_matrix = community, STI = mzb_local_STI, infos_col = 1:5)

# Plot the trend of the CTI over time for Macrozoobenthos
plot_trend(mzb_CTI)

# Calculate the processes (tropicalisation, borealisation, etc.) for Macrozoobenthos
mzb_processes <- get_processes(community_matrix = community, STI = mzb_local_STI, 
                               CTI = mzb_CTI, infos_col = 1:5)

# Plot species trends x STI 
plot_species(processes = mzb_processes$trends)

# PHYTOPLANKTON (PPKT) DATA
# Read the community data for Phytoplankton, excluding the first column
community <- read.csv('../data/ppkt_wide.csv')[, -1]

# Rename the 2nd and 3rd columns to 'Station' and 'Year'
colnames(community)[2:3] <- c('Station', 'Year')

# Load the local Species Thermal Index (STI) data for Phytoplankton
load('../data/ppkt_local_STI.RData', v = TRUE)

# Calculate the Community Temperature Index (CTI) for Phytoplankton
ppkt_CTI <- get_CTI(community_matrix = community, STI = ppkt_local_STI, infos_col = 1:5)

# Plot the trend of the CTI over time for Phytoplankton
plot_trend(ppkt_CTI)

# Calculate the processes (tropicalisation, borealisation, etc.) for Phytoplankton
ppkt_processes <- get_processes(community_matrix = community, STI = ppkt_local_STI, 
                                CTI = ppkt_CTI, infos_col = 1:5)

# Plot species trends x STI 
plot_species(processes = ppkt_processes$trends)
