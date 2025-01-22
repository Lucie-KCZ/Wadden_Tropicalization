# ============================================================================
# Title: Comprehensive Analysis of Tropicalization in Marine Communities
# Author: Lucie Kuczynski
# Date: January 2024
# 
# Description: This script combines the three steps of tropicalization analysis:
# 1. Temperature extraction from MODIS data
# 2. Species Temperature Index (STI) calculation
# 3. Community Temperature Index (CTI) analysis and process identification
#
# Required packages: terra, nlme, MuMIn, ggeffects, scales, ggplot2
# ============================================================================

# Clear environment and load required packages
rm(list = ls())
for(i in 1:10) gc(reset = TRUE)

# Install required packages if not already installed
required_packages <- c("terra", "nlme", "MuMIn", "ggeffects", "scales", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# =============================================================================
# Setup: Define paths and parameters
# =============================================================================

# Set your data name (e.g., "mzb" or "ppkt")
data_name <- "your_data_name"

# Define directory structure
base_dir <- "~/your/project/directory"  # MODIFY THIS PATH
input_dir <- file.path(base_dir, "data")
output_dir <- file.path(base_dir, "output")
modis_dir <- file.path("your/path/to/MODIS/data")  # MODIFY THIS PATH

# Create output directory if it doesn't exist
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define file paths
community_file <- file.path(input_dir, paste0(data_name, "_wide.csv"))
avg_sst_file <- file.path(modis_dir, "average_sst.tif")
temperature_output <- file.path(output_dir, paste0(data_name, "_temperatures.csv"))
sti_output <- file.path(output_dir, paste0(data_name, "_STI.csv"))

# =============================================================================
# Step 1: Temperature Extraction
# =============================================================================

message("Starting temperature extraction...")

# Source the temperature extraction function
source("extract_temperatures.R")

# Get year range from community data
community_data <- read.csv(community_file)
year_range <- range(community_data$year, na.rm = TRUE)

# Extract temperatures
updated_community <- extract_temperature_for_communities(
  community = community_data,
  data_dir = modis_dir,
  avg_file = avg_sst_file,
  output_csv = temperature_output,
  year_range = year_range
)

# Verify output
if(!file.exists(temperature_output)) {
  stop("Temperature extraction failed: output file not created")
}

message("Temperature extraction complete.")

# =============================================================================
# Step 2: Calculate Species Temperature Index (STI)
# =============================================================================

message("Starting STI calculation...")

# Source the STI calculation function
source("compute_STI.R")

# Calculate STI
sti_results <- compute_STI(
  data_name = data_name,
  input_dir = dirname(temperature_output),
  output_dir = output_dir
)

# Check results
if(is.null(sti_results)) {
  stop("STI calculation failed")
}

message("STI calculation complete.")

# =============================================================================
# Step 3: Community Temperature Index (CTI) Analysis
# =============================================================================

message("Starting CTI analysis...")

# Source the CTI analysis functions
source("compute_CTI.R")

# Load the temperature-updated community data
community <- read.csv(temperature_output)
computed_STI <- read.csv(sti_output)

# Ensure correct column names
if(!"Station" %in% colnames(community)) {
  colnames(community)[grep("station", tolower(colnames(community)))] <- "Station"
}
if(!"Year" %in% colnames(community)) {
  colnames(community)[grep("year", tolower(colnames(community)))] <- "Year"
}

# Identify info columns (non-species columns)
info_cols <- which(colnames(community) %in% 
                     c("SampleID", "Station", "Year", "long", "lat", 
                       "temperature", "actual"))

# Calculate CTI
observed_CTI <- get_CTI(
  community_matrix = community,
  STI = computed_STI,
  infos_col = info_cols,
  occu = TRUE  # Set to FALSE if using abundances instead of presence/absence
)

# Create output directory for plots if it doesn't exist
plots_dir <- file.path(output_dir, "plots")
if(!dir.exists(plots_dir)) dir.create(plots_dir)

# Save CTI trend plot
pdf(file.path(plots_dir, paste0(data_name, "_CTI_trend.pdf")), width = 10, height = 8)
plot_trend(observed_CTI)
dev.off()

# Analyze tropicalization processes
inferred_processes <- get_processes(
  community_matrix = community,
  STI = computed_STI,
  CTI = observed_CTI,
  infos_col = info_cols,
  all_outputs = TRUE,
  log = TRUE
)

# Save species trends plot
pdf(file.path(plots_dir, paste0(data_name, "_species_trends.pdf")), width = 12, height = 6)
plot_species(inferred_processes$trends)
dev.off()

# Save process strength results
write.csv(inferred_processes$process_strength,
          file.path(output_dir, paste0(data_name, "_process_strength.csv")),
          row.names = FALSE)

# =============================================================================
# Summary Statistics
# =============================================================================

# Create summary of results
summary_stats <- list(
  n_species = nrow(computed_STI),
  n_stations = length(unique(community$Station)),
  year_range = range(community$Year),
  mean_CTI = mean(observed_CTI$CTI, na.rm = TRUE),
  CTI_trend = coef(lm(CTI ~ Year, data = observed_CTI))[2],
  dominant_process = with(inferred_processes$process_strength,
                          process[which.max(strength)])
)

# Save summary statistics
saveRDS(summary_stats,
        file = file.path(output_dir, paste0(data_name, "_summary_stats.rds")))

# Print summary to console
cat("\nAnalysis Summary:\n")
cat("----------------\n")
cat(sprintf("Number of species: %d\n", summary_stats$n_species))
cat(sprintf("Number of stations: %d\n", summary_stats$n_stations))
cat(sprintf("Year range: %d-%d\n", 
            summary_stats$year_range[1], 
            summary_stats$year_range[2]))
cat(sprintf("Mean CTI: %.2f°C\n", summary_stats$mean_CTI))
cat(sprintf("CTI trend: %.3f°C/year\n", summary_stats$CTI_trend))
cat(sprintf("Dominant process: %s\n", summary_stats$dominant_process))

message("Analysis complete. Results saved in: ", output_dir)

# =============================================================================
# Error Checking and Validation
# =============================================================================

# Function to check for potential issues in the results
check_results <- function() {
  warnings <- character()
  
  # Check for missing temperatures
  temp_na <- sum(is.na(community$temperature))
  if(temp_na > 0) {
    warnings <- c(warnings, 
                  sprintf("%d records have missing temperatures", temp_na))
  }
  
  # Check for species without STI
  species_without_sti <- sum(is.na(computed_STI$STI))
  if(species_without_sti > 0) {
    warnings <- c(warnings, 
                  sprintf("%d species have no STI value", species_without_sti))
  }
  
  # Check for stations with few observations
  station_counts <- table(community$Station)
  few_obs_stations <- sum(station_counts < 5)
  if(few_obs_stations > 0) {
    warnings <- c(warnings, 
                  sprintf("%d stations have fewer than 5 observations", 
                          few_obs_stations))
  }
  
  # Print warnings if any
  if(length(warnings) > 0) {
    cat("\nWarnings:\n")
    cat("--------\n")
    cat(paste("-", warnings), sep = "\n")
  } else {
    cat("\nNo major issues detected in the results.\n")
  }
}

# Run validation checks
check_results()
