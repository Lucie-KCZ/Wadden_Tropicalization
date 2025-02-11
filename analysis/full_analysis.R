# -------------------------------------------------------------------
# Script: Full analyses from raw community data to processes 
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski
#
# Date:    July 2024
# Updated: February 2025
#
# Description:
# This script orchestrates a three-step tropicalization analysis workflow:
# 1) Temperature extraction from MODIS data for each community record,
# 2) Calculation of Species Temperature Index (STI),
# 3) Community Temperature Index (CTI) analysis and process inference.
#
# Required packages: 
#   terra (>= 1.8-10)
#   nlme (>= 3.1-166)
#   MuMIn (>= 1.48.4)
#   ggeffects (>= 2.1.0)
#   scales (>= 1.3.0)
#   ggplot2 (>= 3.5.1)
# -------------------------------------------------------------------
#
#
# -----------------------------------------------------------------------------
# 1. Environment Setup and Package Loading
# -----------------------------------------------------------------------------

# Clear the environment and run garbage collection to ensure a clean workspace
rm(list = ls())
for(i in 1:10) gc(reset = TRUE)

# Identify packages required for this workflow and install them if missing.
# This ensures that anyone running this script has all dependencies.
required_packages <- c("terra", "nlme", "MuMIn", "ggeffects", "scales", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required packages, hiding messages to keep the console output clean.
invisible(lapply(required_packages, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 2. Setup: Define Paths and Parameters
# -----------------------------------------------------------------------------

# Set a short, descriptive name for your dataset (e.g., "mzb" or "ppkt").
data_name <- "your_data"

# Define your base directory and organize inputs and outputs below it.
# Modify these paths according to your local or remote file system.
base_dir <- "~/your/project/directory"  # MODIFY THIS PATH
input_dir <- file.path(base_dir, "data")
output_dir <- file.path(base_dir, "output")
modis_dir <- file.path("your/path/to/MODIS/data")  # MODIFY THIS PATH

# Create the output directory if it does not already exist to store results.
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define the file paths used throughout the analysis for reading/writing data.
community_file <- file.path(input_dir, paste0(data_name, "_wide.csv"))
avg_sst_file <- file.path(modis_dir, "average_sst.tif")
temperature_output <- file.path(output_dir, paste0(data_name, "_temperatures.csv"))
sti_output <- file.path(output_dir, paste0(data_name, "_STI.csv"))

# -----------------------------------------------------------------------------
# 3. Step 1: Temperature Extraction
# -----------------------------------------------------------------------------

message("Starting temperature extraction...")

# Source the external R script that contains the function for temperature extraction.
# This function will read MODIS data and associate each sample (community record)
# with the corresponding temperature values.
source("extract_temperatures.R")

# Read the main community data to determine the range of years needed for extraction.
community_data <- read.csv(community_file)
year_range <- range(community_data$year, na.rm = TRUE)

# Call the temperature extraction function. It outputs a CSV with updated temperature
# information for each community record.
updated_community <- extract_temperature_for_communities(
  community = community_data,
  data_dir = modis_dir,
  avg_file = avg_sst_file,
  output_csv = temperature_output,
  year_range = year_range
)

# Ensure that the extraction process produced the expected output file.
if(!file.exists(temperature_output)) {
  stop("Temperature extraction failed: output file not created")
}

message("Temperature extraction complete.")


# -----------------------------------------------------------------------------
# 4. Step 2: Calculate Species Temperature Index (STI)
# -----------------------------------------------------------------------------

message("Starting STI calculation...")

# Source the STI calculation function. It uses the temperature-updated community file
# to compute the average experienced temperature for each species.
source("compute_STI.R")

# Perform the STI calculation and store the results in memory.
sti_results <- compute_STI(
  data_name = data_name,
  input_dir = dirname(temperature_output),
  output_dir = output_dir
)

# If the function returns NULL, it indicates that the STI calculation did not complete.
if(is.null(sti_results)) {
  stop("STI calculation failed")
}

message("STI calculation complete.")


# -----------------------------------------------------------------------------
# 5. Step 3: Community Temperature Index (CTI) Analysis
# -----------------------------------------------------------------------------

message("Starting CTI analysis...")

# Source the scripts that handle CTI-related computations and plotting of trends.
source("compute_CTI.R")

# Load the temperature-updated community data and the newly computed STI values.
community <- read.csv(temperature_output)
computed_STI <- read.csv(sti_output)

# Standardize column names for station and year if they differ in case (e.g. "Station" vs "station").
if(!"Station" %in% colnames(community)) {
  colnames(community)[grep("station", tolower(colnames(community)))] <- "Station"
}
if(!"Year" %in% colnames(community)) {
  colnames(community)[grep("year", tolower(colnames(community)))] <- "Year"
}

# Identify information/metadata columns in the community dataset.
# This helps separate them from the species columns.
info_cols <- which(colnames(community) %in%
                     c("SampleID", "Station", "Year", "long", "lat",
                       "temperature", "actual"))

# Compute the CTI by integrating species presence/absence or abundance data
# with the previously computed STI values.
observed_CTI <- get_CTI(
  community_matrix = community,
  STI = computed_STI,
  infos_col = info_cols,
  occu = TRUE  # Set to FALSE if you want to use abundance data instead of presence/absence
)

# Create a directory for plots if it does not exist, so that CTI trend graphs can be saved.
plots_dir <- file.path(output_dir, "plots")
if(!dir.exists(plots_dir)) dir.create(plots_dir)

# Generate and save a PDF that visualizes the annual trend of CTI.
pdf(file.path(plots_dir, paste0(data_name, "_CTI_trend.pdf")), width = 10, height = 8)
plot_trend(observed_CTI)
dev.off()

# Use the get_processes function to infer which ecological processes (e.g., gains, losses)
# drive the observed changes in CTI over time.
inferred_processes <- get_processes(
  community_matrix = community,
  STI = computed_STI,
  CTI = observed_CTI,
  infos_col = info_cols,
  all_outputs = TRUE,
  log = TRUE
)

# Plot species-level trends underlying the CTI changes and save to PDF.
pdf(file.path(plots_dir, paste0(data_name, "_species_trends.pdf")), width = 12, height = 6)
plot_species(inferred_processes$trends)
dev.off()

# Save the quantitative results of process strengths to a CSV for future reference.
write.csv(inferred_processes$process_strength,
          file.path(output_dir, paste0(data_name, "_process_strength.csv")),
          row.names = FALSE)


# -----------------------------------------------------------------------------
# 6. Summary Statistics
# -----------------------------------------------------------------------------

# Create a quick summary of the final results, including:
# - Number of species analyzed
# - Number of stations sampled
# - Overall time span
# - Mean CTI and annual CTI trend
# - Most dominant ecological process inferred
summary_stats <- list(
  n_species = nrow(computed_STI),
  n_stations = length(unique(community$Station)),
  year_range = range(community$Year),
  mean_CTI = mean(observed_CTI$CTI, na.rm = TRUE),
  CTI_trend = coef(lm(CTI ~ Year, data = observed_CTI))[2],
  dominant_process = with(inferred_processes$process_strength,
                          process[which.max(strength)])
)

# Save the summary statistics as an RDS object for easy loading in future analyses.
saveRDS(summary_stats,
        file = file.path(output_dir, paste0(data_name, "_summary_stats.rds")))

# Print key summary information in the console.
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


# -----------------------------------------------------------------------------
# 7. Error Checking and Validation
# -----------------------------------------------------------------------------

#' Check the final results for potential issues in the dataset.
#'
#' This function scans the updated community data and computed STI values to
#' detect common problems, such as missing temperatures or species lacking STI.
#' It also checks for stations with very few observations, which might bias
#' the analysis.
#'
#' @return Prints warnings in the console if any issues are detected. 
#'         Prints a message indicating no issues were found otherwise.
#'
#' @examples
#' check_results()
check_results <- function() {
  warnings <- character()
  
  # Check whether there are any records missing temperature data.
  temp_na <- sum(is.na(community$temperature))
  if(temp_na > 0) {
    warnings <- c(warnings,
                  sprintf("%d records have missing temperatures", temp_na))
  }
  
  # Check if any species in the computed STI table do not have an STI value.
  species_without_sti <- sum(is.na(computed_STI$STI))
  if(species_without_sti > 0) {
    warnings <- c(warnings,
                  sprintf("%d species have no STI value", species_without_sti))
  }
  
  # Verify that each station has a reasonable number of records.
  # Stations with fewer than 5 observations might be too sparse for reliable analysis.
  station_counts <- table(community$Station)
  few_obs_stations <- sum(station_counts < 5)
  if(few_obs_stations > 0) {
    warnings <- c(warnings,
                  sprintf("%d stations have fewer than 5 observations",
                          few_obs_stations))
  }
  
  # Print any warnings found; otherwise, indicate that the checks are passed.
  if(length(warnings) > 0) {
    cat("\nWarnings:\n")
    cat("--------\n")
    cat(paste("-", warnings), sep = "\n")
  } else {
    cat("\nNo major issues detected in the results.\n")
  }
}

# Call the result-checking function to validate the main outputs.
check_results()
