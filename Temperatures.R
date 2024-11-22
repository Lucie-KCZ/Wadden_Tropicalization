########################################
###  Temperatures in the Wadden Sea  ###
########################################

# Authors:
# Lucie Kuczynski, Helmut Hillebrand

# Script by: Lucie Kuczynski
# Email: lucie.kuczynski@hotmail.com
# Created: October 2024
# Last edited: October 2024

# Clear the environment and set display precision
rm(list = ls())
options(digits = 10)

# Load necessary libraries
library(lubridate)

# Function to convert date-time string to numeric date format (year + proportion of the year elapsed)
convert_datetime_to_numeric <- function(datetime_str) {
  message(sprintf("Processing datetime: %s", datetime_str))
  
  # Parse the date-time string
  datetime_parsed <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  
  # Check for valid parsing
  if (is.na(datetime_parsed)) {
    warning(sprintf("Invalid date-time format for input '%s'.", datetime_str))
  }
  
  # Extract the year and calculate the proportion of the year
  year_value <- year(datetime_parsed)
  total_minutes_in_year <- ifelse(leap_year(datetime_parsed), 366 * 24 * 60, 365 * 24 * 60)
  start_of_year <- ymd_hm(paste0(year_value, "-01-01 00:00"))
  minutes_elapsed <- as.numeric(difftime(datetime_parsed, start_of_year, units = "mins"))
  proportion_of_year_elapsed <- minutes_elapsed / total_minutes_in_year
  
  # Return numeric date (year + proportion)
  return(year_value + proportion_of_year_elapsed)
}

# Function to extract the week number as a decimal representation of the year
extract_week_decimal <- function(datetime_str) {
  message(sprintf("Processing datetime: %s", datetime_str))
  
  # Parse the date-time string
  datetime_parsed <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  
  # Check for valid parsing
  if (is.na(datetime_parsed)) {
    stop(sprintf("Invalid date-time format for input '%s'.", datetime_str))
  }
  
  # Extract the year and week, return as year + week proportion
  year_value <- year(datetime_parsed)
  week_number <- week(datetime_parsed)
  return(year_value + (week_number / 52))
}

# Load temperature data from CSV
temperatures <- read.csv(file = '~/Documents/Work/Data/Data_Oldenburg/DYNACOM/SPZ_Environment/Temperatures/Temperatures.csv', dec = ',')

# Apply the date conversion functions
temperatures$Timing <- vapply(temperatures$Time, convert_datetime_to_numeric, numeric(1))
temperatures$Week <- vapply(temperatures$Time, extract_week_decimal, numeric(1))

# Calculate weekly averages for columns 2 to 7, grouped by week
weekly_avg <- as.data.frame(do.call(rbind, as.list(by(
  temperatures, temperatures$Week, FUN = function(x) apply(x[, 2:7], 2, mean, na.rm = TRUE), simplify = TRUE))))

# Set Time as row names and reset row names
weekly_avg$Time <- as.numeric(rownames(weekly_avg))
rownames(weekly_avg) <- NULL

# Optionally, write the results to a CSV file
write.csv(x = weekly_avg, file = '../data/wkly_temperatures.csv')

# # Load weekly average data from CSV for further analysis
# weekly_avg <- read.csv('../data/wkly_temperatures.csv')[, -1]

# Define temperature and year limits for the plot
temperatures_limits <- c(floor(min(weekly_avg[, -7], na.rm = TRUE)), ceiling(max(weekly_avg[, -7], na.rm = TRUE)))
year_limits <- range(weekly_avg$Time)

# Set plot layout and margins
par(mfrow = c(2, 3), mar = c(5, 5, 5, 2), pty = 's', oma = c(5, 5, 5, 5))

# Loop over the columns (1 to 6) and generate plots for each
for(i in 1:6){
  plot(x = 1, y = 1, xlim = year_limits, ylim = temperatures_limits, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  # Add axes and labels
  axis(side = 1, tcl = -0.5, lwd = 2, las = 2, cex.axis = 2.5, padj = 0.5)
  axis(side = 2, tcl = -0.5, lwd = 2, las = 1, cex.axis = 2.5)
  
  if(i > 3) mtext('Year', side = 1, padj = -.5, line = 9, cex = 2)
  mtext(colnames(weekly_avg)[i], side = 3, padj = .5, cex = 2)
  
  # Plot observed data
  lines(weekly_avg[, i] ~ weekly_avg$Time, lwd = 3, col = '#6b9080')
  points(weekly_avg[, i] ~ weekly_avg$Time, pch = 21, bg = "#a4c3b2", cex = 1.5)
}

# Load necessary libraries for GLS modeling
library(nlme)        # For generalized least squares modeling
library(rcompanion)  # For calculating Nagelkerke's pseudo R-squared
library(reshape2)    # For reshaping data and working with matrices

# Define columns for pairwise modeling
predictor_columns <- sort(colnames(weekly_avg)[1:6])

# Initialize results data frame
model_results <- data.frame(predictor = character(), response = character(), estimate = numeric(), R2 = numeric())

# Loop through unique pairwise combinations of columns
for (i in 1:(length(predictor_columns) - 1)) {
  for (j in (i + 1):length(predictor_columns)) {
    
    predictor_var <- predictor_columns[i]
    response_var <- predictor_columns[j]
    
    # Print progress
    print(paste("Processing:", predictor_var, "and", response_var))
    
    # GLS model with AR1 correlation
    model_formula <- as.formula(paste(response_var, "~", predictor_var))
    model <- gls(model_formula, data = weekly_avg, correlation = corAR1(form = ~ 1), na.action = na.omit, method = 'ML')
    
    # Extract model results
    model_summary <- summary(model)
    coefficient_estimate <- model_summary$tTable[2, "Value"]
    nagelkerke_R2 <- nagelkerke(model)$Pseudo.R.squared.for.model.vs.null[3]
    
    # Append to results data frame
    model_results <- rbind(model_results, data.frame(predictor = predictor_var, response = response_var, estimate = coefficient_estimate, R2 = nagelkerke_R2))
  }
}

# Display results table
knitr::kable(model_results, caption = "GLS Model Estimates and Nagelkerke Pseudo R-Squared for Unique Pairwise Combinations")

# Plot heatmap of estimates and R² values
ggplot(model_results, aes(predictor, response, fill = estimate)) + 
  geom_tile(color = "black") + 
  scale_fill_gradient(low = "#ffd166", high = "#ff006e", limit = c(min(model_results$estimate), max(model_results$estimate)), space = "Lab", name = "Estimate") +
  geom_text(aes(label = sprintf("%.2f", R2)), color = "white", size = 5) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = NULL, title = "Heatmap of GLS Estimates with R²")
