# -------------------------------------------------------------------
# Script: Extract Temperature for Community Records
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski
#
# Date:    January 2025
# Updated: February 2025
#
# Description:
# This script provides a function to merge community data (with columns
# for year, latitude, longitude, and optionally month) and MODIS SST rasters.
# It assigns a temperature value to each community record by extracting
# raster values based on the record's location, year, and month (if provided).
# If monthly data is unavailable, it falls back to yearly average, and if
# yearly data is missing, it uses the overall average SST. It then flags
# whether the temperature is from actual monthly/yearly data or the overall average.
#
# Required packages:
#   terra (>= 1.8-10)
# -------------------------------------------------------------------

#' Extract Temperature Data for Community Records
#'
#' This function loads community data and corresponding MODIS SST raster data, 
#' then extracts sea surface temperature (SST) values for each community record 
#' based on its spatial location, year, and optionally month. If specific monthly
#' or yearly data is unavailable, the function falls back to yearly average or
#' overall average temperature respectively.
#'
#' @param community_data Data frame containing community data with required columns:
#'   year, lat, long, and optionally month for temporal resolution.
#' @param data_dir Character. Directory path where monthly TIFF files are stored.
#' @param avg_file Character. File path to the overall average SST GeoTIFF.
#' @param output_csv Character. Path where the updated community CSV will be saved.
#' @param year_range Integer vector of length 2. The range of years to consider.
#' @param month_col Character. Optional name of the month column. Default is NULL.
#' @param month_format Character. Format of month values: "short" (Jan, Feb), 
#'   "full" (January, February), or "numeric" (1-12). Default is "short".
#' @param show_progress Boolean. To show a progression bar. Default is TRUE.  
#'
#' @return A data frame identical to the community input but with two additional columns:
#'   \itemize{
#'     \item \code{temperature}: Numeric. The extracted SST value.
#'     \item \code{actual}: Character. "monthly" if from monthly data, "yearly" if from
#'           yearly average, or "overall" if from overall average.
#'   }
#'   The function also saves the updated data frame to the specified \code{output_csv} file.
#'
#' @examples
#' \dontrun{
#'   # Read your community data
#'   community_data <- read.csv("../data/mzb_abundances_wide.csv")
#'   
#'   # Set up paths and parameters
#'   data_dir <- "/path/to/modis/data"
#'   avg_file <- "/path/to/average_sst.tif"
#'   output_csv <- "../data/mzb_abundances_temperatures.csv"
#'   year_range <- c(2000, 2024)
#'
#'   # Without month information
#'   updated_community <- extract_temperature_for_communities(
#'     community_data, data_dir, avg_file, output_csv, year_range)
#'
#'   # With month information
#'   updated_community <- extract_temperature_for_communities(
#'     community_data, data_dir, avg_file, output_csv, year_range,
#'     month_col = "month", month_format = "short")
#' }
#'
#' @import terra
#' @export
extract_temperature_for_communities <- function(community_data, data_dir, avg_file, 
                                                output_csv, year_range, 
                                                month_col = NULL, 
                                                month_format = "short", 
                                                show_progress = TRUE) {
  # Check for required packages to ensure the environment is properly set up
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required. Please install it with install.packages('terra')")
  }
  
  # Validate that the essential columns exist and that the provided paths and parameters are correct
  if (!all(c("year", "lat", "long") %in% colnames(community_data))) {
    stop("Community data must contain columns: 'year', 'lat', and 'long'")
  }
  
  # If a month column is specified, ensure it exists in the provided data
  if (!is.null(month_col) && !(month_col %in% colnames(community_data))) {
    stop("Specified month column '", month_col, "' not found in community data")
  }
  
  # Confirm that the average SST file and data directory exist
  if (!file.exists(avg_file)) {
    stop("Average SST file not found: ", avg_file)
  }
  
  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir)
  }
  
  # Validate year range to avoid unexpected errors or incomplete data
  if (length(year_range) != 2 || !is.numeric(year_range)) {
    stop("year_range must be a numeric vector of length 2")
  }
  
  # Convert month labels (in short, full text, or numeric form) to a numeric month for file matching
  month_to_num <- function(month_val, format) {
    if (format == "numeric") return(as.integer(month_val))
    
    # Create a lookup table to map month names to their numeric equivalent
    month_lookup <- switch(format,
                           "short" = setNames(1:12, c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           "full" = setNames(1:12, c("January", "February", "March", "April", "May", "June",
                                                     "July", "August", "September", "October", "November", "December")),
                           stop("Unsupported month_format")
    )
    month_lookup[month_val]
  }
  
  # Helper to format single-digit months with a leading zero (e.g., "01" for January)
  pad_num <- function(x) sprintf("%02d", as.integer(x))
  
  # Load the overall average raster to use when neither monthly nor yearly data is available
  avg_raster <- terra::rast(avg_file)
  
  # Gather all TIFF files in the data directory for monthly and yearly extractions
  all_files <- list.files(data_dir, pattern = "\\.tif$", full.names = TRUE)
  
  # Create an environment to cache raster objects (avoiding repeated disk reads for performance)
  raster_cache <- new.env()
  
  # Initialize columns to store extracted temperature data and a flag indicating data source
  community_data$temperature <- NA_real_
  community_data$actual <- "overall"
  
  if(show_progress) {
    # Construct a progress bar to inform the user about the ongoing extraction process
    pb <- txtProgressBar(min = 0, max = nrow(community_data), style = 3)
  }
  
  # Loop through each community record to extract temperature values
  for (i in seq_len(nrow(community_data))) {
    year <- community_data$year[i]
    
    # Prepare the spatial coordinate for this record as a matrix (terra::extract requirement)
    location <- c(community_data$long[i], community_data$lat[i])
    point_matrix <- matrix(location, nrow = 1)
    
    # Attempt to use monthly data first if month information is available
    if (!is.null(month_col)) {
      month_num <- month_to_num(community_data[[month_col]][i], month_format)
      # Build a file-search pattern reflecting year and month naming
      month_pattern <- sprintf("TERRA_MODIS.%s%s\\d+_%s%s\\d+\\.L3m\\.MO\\.SST",
                               year, pad_num(month_num), year, pad_num(month_num))
      
      # Retrieve all matching monthly files
      matching_files <- grep(month_pattern, all_files, value = TRUE)
      
      # If at least one file matches, we attempt extraction
      if (length(matching_files) > 0) {
        cache_key <- paste0(year, "_", month_num)
        
        # Load and cache the monthly raster if it isn't already in the cache
        if (!exists(cache_key, envir = raster_cache)) {
          raster_cache[[cache_key]] <- terra::rast(matching_files[1])
        }
        
        # Extract the SST value for the current record
        extracted <- terra::extract(raster_cache[[cache_key]], point_matrix)
        # Some rasters may return multiple columns, so we index accordingly
        temp_val <- if (ncol(extracted) >= 2) extracted[,2] else extracted[,1]
        
        # Update the community_data with the extracted temperature if it's valid
        if (!is.na(temp_val)) {
          community_data$temperature[i] <- temp_val
          community_data$actual[i] <- "monthly"
          if(show_progress) setTxtProgressBar(pb, i)
          next  # Move to the next record if monthly data was successfully used
        }
      }
    }
    
    # If monthly data was not used (unavailable or returned NA), try using yearly data
    year_pattern <- sprintf("TERRA_MODIS\\.%s", year)
    matching_year_files <- grep(year_pattern, all_files, value = TRUE)
    
    if (length(matching_year_files) > 0) {
      cache_key <- as.character(year)
      
      # Cache the yearly mean raster so we don't re-compute it for each record
      if (!exists(cache_key, envir = raster_cache)) {
        year_stack <- terra::rast(matching_year_files)
        # We take the mean of all monthly rasters for that year as the yearly average
        raster_cache[[cache_key]] <- terra::mean(year_stack, na.rm = TRUE)
      }
      
      extracted <- terra::extract(raster_cache[[cache_key]], point_matrix)
      temp_val <- if (ncol(extracted) >= 2) extracted[,2] else extracted[,1]
      
      if (!is.na(temp_val)) {
        community_data$temperature[i] <- temp_val
        community_data$actual[i] <- "yearly"
        if(show_progress) setTxtProgressBar(pb, i)
        next  # Use yearly value if successfully extracted
      }
    }
    
    # If neither monthly nor yearly data were available or valid, fall back to the overall average
    extracted <- terra::extract(avg_raster, point_matrix)
    temp_val <- if (ncol(extracted) >= 2) extracted[,2] else extracted[,1]
    community_data$temperature[i] <- temp_val
    
    if(show_progress) setTxtProgressBar(pb, i)
  }
  if(show_progress) close(pb)
  
  # Write the updated data (with temperature and actual columns) to a CSV file
  write.csv(community_data, file = output_csv, row.names = FALSE)
  
  # Print a summary for the user to know how the data was populated
  message(sprintf("Temperature extraction complete. Data saved to '%s'.", output_csv))
  monthly_count <- sum(community_data$actual == "monthly")
  yearly_count <- sum(community_data$actual == "yearly")
  overall_count <- sum(community_data$actual == "overall")
  na_count <- sum(is.na(community_data$temperature))
  
  message(sprintf(
    "Summary:\n  Monthly data: %d records\n  Yearly averages: %d records\n  Overall averages: %d records",
    monthly_count, yearly_count, overall_count
  ))
  
  if (na_count > 0) {
    warning(sprintf("%d records had missing temperature data.", na_count))
  }
  
  return(community_data)
}

# -------------------------------------------------------------------

# Example Usage
# 
# # 1. Define File Paths and Parameters
# # We specify paths to input community CSV files and the MODIS SST data,
# # and also determine a range of years to consider for yearly averages.
#
# data_name <- "mzb"
# community_file <- paste0("../data/", data_name, "_wide.csv")  # Path to community CSV file
# 
# # Load community data, omitting the first column if it’s an index
# community <- read.csv(community_file)[, -1]
# 
# # Rename key columns for 'ppkt' if necessary (ensures consistent col names in data)
# if(data_name == "ppkt") colnames(community)[3:5] <- c("year", "long", "lat")
# 
# # Define the range of years to consider for yearly data
# year_range <- range(community$year)
# 
# data_dir <- "/Users/lucie/Documents/Work/Academia/Data/TerraModis/data/processed/AquaMODIS"  # Directory with yearly TIFF files
# avg_file <- "/Users/lucie/Documents/Work/Academia/Data/TerraModis/data/processed/average_sst.tif"  # Path to overall average SST raster
# output_csv <- paste0("../data/", data_name, "_temperatures.csv")  # Desired output CSV path
# 
# # 2. Run the Temperature Extraction Function
# # The function merges spatial community data with SST values for each record,
# # attempting monthly, then yearly, then overall average data.
# updated_community <- extract_temperature_for_communities(
#   community,
#   data_dir,
#   avg_file,
#   output_csv,
#   year_range
# )
# 
# # 3. Explore or Save Results
# # The 'updated_community' now has two additional columns: 'temperature' and 'actual'.
# # 'temperature' is the SST value, while 'actual' indicates if that value was 
# # derived from monthly, yearly, or overall data.
# # You can use this updated data frame for further analysis.
