# -------------------------------------------------------------------
# Script: Extract Temperature for Community Records
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski, Helmut Hillebrand
#
# Date:    January 2025
# Updated: January 2025
#
# Description:
# This script provides a function to merge community data (with columns
# for year, latitude, and longitude) and MODIS SST rasters for each year.
# It assigns a temperature value to each community record by extracting
# raster values based on the record's location and year. If a given year
# is missing from the dataset, the script falls back to an overall
# average SST. It then flags whether the temperature is from actual yearly
# data or the overall average.
#
# NOTE: If you rename columns in the community or change the raster paths,
#       be sure to adjust the arguments in the function accordingly.
# NOTE: Please remove or comment out the environment-clearing lines below
#       if you do not want them to run automatically.
# -------------------------------------------------------------------

# Clean out your work environment (be cautious if you keep them active)
for(i in 1:10) gc(reset = TRUE)
# rm(list = ls())


#' Extract Temperature Data for Community Records
#'
#' This function loads community data and corresponding MODIS SST raster data, 
#' then extracts sea surface temperature (SST) values for each community record 
#' based on its spatial location and year. If data for the specific year is unavailable, 
#' the function assigns the overall average temperature. It also flags whether 
#' the extracted value comes from actual yearly data or from the overall average.
#'
#' @param community_file Character. Path to the CSV file containing community data. 
#'   The CSV must include columns: \code{year}, \code{lat}, and \code{long}, among others.
#' @param data_dir Character. Directory path where yearly TIFF files are stored.
#' @param avg_file Character. File path to the overall average SST GeoTIFF.
#' @param output_csv Character. Path where the updated community CSV will be saved.
#' @param year_range Integer vector of length 2. The range of years to consider 
#'   for extracting actual yearly temperature data (e.g., \code{c(2002, 2010)}).
#'
#' @return A data frame identical to the community input but with two additional columns:
#'   \itemize{
#'     \item \code{temperature}: Numeric. The extracted SST value for the record's location and year 
#'           (or overall average if yearly data is unavailable).
#'     \item \code{actual}: Logical. \code{TRUE} if the temperature value comes from the 
#'           actual yearly raster, \code{FALSE} if it is the overall average.
#'   }
#'   The function also saves the updated data frame to the specified \code{output_csv} file.
#'
#' @examples
#' \dontrun{
#'   community_file <- "../data/mzb_abundances_wide.csv"
#'   data_dir <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/AquaMODIS"
#'   avg_file <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/average_sst.tif"
#'   output_csv <- "../data/mzb_abundances_temperatures.csv"
#'   year_range <- c(2002, 2010)
#'
#'   updated_community <- extract_temperature_for_communities(
#'                         community_file, data_dir, avg_file, output_csv, year_range)
#' }
#'
#' @import terra
#' @export
extract_temperature_for_communities <- function(community, data_dir, avg_file, output_csv, year_range) {
  require(terra)
  
  # Load the overall average raster
  avg_raster <- rast(avg_file)
  
  # List all TIFF files and filter by specified year range using regex after underscore
  all_files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
  fnames <- basename(all_files)
  
  # Create regex pattern for years within year_range after an underscore
  years_seq <- seq(year_range[1], year_range[2])
  pattern <- paste0("_(?:", paste(years_seq, collapse="|"), ")")
  files_in_range <- all_files[grepl(pattern, fnames)]
  
  # Compute yearly average rasters for each year in the specified range
  yearly_rasters <- list()
  for (yr in years_seq) {
    year_files <- files_in_range[grepl(paste0("_", yr), basename(files_in_range))]
    if (length(year_files) > 0) {
      ras_stack <- rast(year_files)
      yearly_rasters[[as.character(yr)]] <- mean(ras_stack, na.rm = TRUE)
    }
  }
  
  # Prepare columns in community data for temperature and flag
  community$temperature <- NA
  community$actual <- FALSE
  
  # Extract temperature for each community record
  for (i in 1:nrow(community)) {
    sample_year <- community$year[i]
    location <- c(community$long[i], community$lat[i])
    point_matrix <- matrix(location, nrow = 1)
    
    if (as.character(sample_year) %in% names(yearly_rasters)) {
      extracted <- extract(yearly_rasters[[as.character(sample_year)]], point_matrix)
      temp_val <- if(ncol(extracted) >= 2) extracted[,2] else extracted[,1]
      community$temperature[i] <- temp_val
      community$actual[i] <- TRUE
    } else {
      extracted <- extract(avg_raster, point_matrix)
      temp_val <- if(ncol(extracted) >= 2) extracted[,2] else extracted[,1]
      community$temperature[i] <- temp_val
      community$actual[i] <- FALSE
    }
  }
  
  # Reorder columns so that 'temperature' and 'actual' become the 6th and 7th columns
  desired_order <- c(1:5, ncol(community)-1, ncol(community), 6:(ncol(community)-2))
  community <- community[, desired_order]
  
  # Save the updated community data to a new CSV
  write.csv(community, file = output_csv, row.names = FALSE)
  
  # Print professional messages
  message(sprintf("Temperature extraction complete. Data saved to '%s'.", output_csv))
  na_count <- sum(is.na(community$temperature))
  if (na_count > 0) {
    message(sprintf("Note: %d records had missing temperature data due to unmatch locations.", na_count))
  } else {
    message("All records successfully matched with actual temperature data.")
  }
  
  return(community)
}


# -------------------------------------------------------------------
# Example Usage
# -------------------------------------------------------------------

# 1. Define File Paths and Parameters
# # Set paths to input files, output files, and specify the year range for analysis.
# data_name <- "ppkt"
# community_file <- paste0("../data/", data_name, "_wide.csv")  # Path to community CSV file
# # Load community data
# community <- read.csv(community_file)[, -1]
# # Rename key columns - if ppkt
# if(data_name == "ppkt") colnames(community)[3:5] <- c("year", "long", "lat")
# # Define the range of years to consider for yearly data
# year_range <- range(community$year)

# data_dir <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/AquaMODIS"  # Directory with yearly TIFF files
# avg_file <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/average_sst.tif"  # Path to overall average SST raster
# output_csv <- paste0("../data/", data_name, "_temperatures.csv")  # Desired output CSV path

# 2. Run the Temperature Extraction Function
# # This will load community data, compute or extract temperatures for each record,
# # and save the updated community data with new temperature information.
# updated_community <- extract_temperature_for_communities(
#   community,
#   data_dir,
#   avg_file,
#   output_csv,
#   year_range
# )

# 3. Explore or Save Results
# The 'updated_community' data frame now contains two additional columns:
# - temperature: SST value for each record's location and time
# - actual: Flag indicating if the temperature is from actual yearly data (TRUE)
#           or an overall average (FALSE).
# You can proceed with further analysis using 'updated_community' or inspect the saved CSV.


