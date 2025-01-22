# -------------------------------------------------------------------
# Script: Compute Species Thermal Index (STI) and Occurrences
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski, Helmut Hillebrand
#
# Date:    June 2024
# Updated: January 2025
#
# Description:
# This script reads an updated community file that includes both
# biodiversity (species) columns and a temperature column. It then
# computes the Species Thermal Index (STI) for each species, defined as
# the mean experienced temperature across all rows where the species
# is present. The script also reports how many occurrences (records) are
# used to calculate each STI, enabling users to assess data completeness.
#
# NOTE: The script checks for core metadata columns (e.g., SampleID,
#       StationID, year, lat, long, temperature, actual). Warnings
#       guide the user if files or columns are missing, ensuring
#       clarity for non-expert R users.
# NOTE: Please remove or comment out the environment-clearing lines below
#       if you do not want them to run automatically.
# -------------------------------------------------------------------

# Clean out your work environment (be cautious if you keep them active)
for(i in 1:10) gc(reset = TRUE)
# rm(list = ls())


# -------------------------------------------------------------------
# Function: compute_STI
# -------------------------------------------------------------------

#' Compute Species Thermal Index (STI) and Number of Occurrences
#'
#' This function reads an updated community dataset that includes temperature columns,
#' computes the mean experienced temperature (STI) for each species, and also counts
#' the number of occurrences (records) used in the calculation.
#'
#' @param data_name Character string indicating the name of the taxa studied,
#'   used in file paths (e.g., "mzb", "ppkt").
#' @param input_dir Character string specifying the directory path where the updated
#'   community CSV file is located. The file must be named 
#'   \code{"\{data_name\}\_abundances\_temperatures.csv"} or \code{"\{data_name\}\_temperatures.csv"}
#'   based on your naming convention.
#' @param output_dir Character string specifying the directory where the STI results
#'   will be saved as \code{"\{data_name\}\_STI.csv"}.
#'
#' @details
#' The updated community file is expected to contain the following columns:
#' \itemize{
#'   \item \code{SampleID, StationID, year, long, lat, temperature, actual}
#'   \item Additional columns for species abundances (one column per species).
#' }
#'
#' \strong{Warnings and Checks:}  
#' This function will issue warnings if:
#' \itemize{
#'   \item The input file does not exist.
#'   \item The input data frame is empty or missing critical columns (including the required metadata columns).
#'   \item No species columns (i.e., columns other than metadata + temperature).
#'   \item The identified species columns are not numeric.
#' }
#'
#' The function calculates STI for each species as the mean \code{temperature}
#' of all rows where that species' abundance is > 0. It also counts how many
#' such rows contributed to the STI (the \code{n_occ}).
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{species}: The species name (column name from the community data).
#'   \item \code{STI}: The mean temperature for that species across all occurrences.
#'   \item \code{n_occ}: The number of occurrences (rows) used to compute \code{STI}.
#' }
#'
#' The function also writes these results to \code{"\{data_name\}\_STI.csv"} in \code{output_dir}.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Suppose we have "mzb_abundances_temperatures.csv" in "../data/"
#' # and we want to compute STI for "mzb" data.
#'
#' compute_STI(
#'   data_name = "mzb",
#'   input_dir = "../data",
#'   output_dir = "../data"
#' )
#' }
#'
#' @export
compute_STI <- function(data_name,
                                        input_dir = "../data",
                                        output_dir = "../data") {
  
  # Build full paths
  input_file <- file.path(input_dir, paste0(data_name, "_temperatures.csv"))
  existing_file <- input_file[file.exists(input_file)]
  
  # File existence checks
  if (length(existing_file) == 0) {
    warning(sprintf(
      "No input file found for '%s'. Tried:\n%s\nCheck your file name or path.\n",
      data_name, paste(input_file, collapse = "\n")
    ))
    return(invisible(NULL))
  }
  if (length(existing_file) > 1) {
    warning(sprintf(
      "Multiple matching files found for '%s'. Using the first:\n%s\n",
      data_name, existing_file[1]
    ))
    existing_file <- existing_file[1]
  }
  
  # Construct output file path
  output_file <- file.path(output_dir, paste0(data_name, "_STI.csv"))
  
  # Load the updated community data
  community <- read.csv(existing_file, stringsAsFactors = FALSE)
  
  # Basic checks on the loaded data
  if (nrow(community) == 0) {
    warning(sprintf(
      "The data file '%s' appears to be empty (0 rows). Nothing to process.\n",
      basename(existing_file)
    ))
    return(invisible(NULL))
  }
  
  # Required columns
  required_cols <- c("SampleID", "StationID", "year", "long", "lat", "temperature", "actual")
  missing_required <- setdiff(required_cols, colnames(community))
  if (length(missing_required) > 0) {
    warning(sprintf(
      "The following required columns are missing from your data: %s\n",
      paste(missing_required, collapse = ", ")
    ))
    return(invisible(NULL))
  }
  
  # Identify species columns
  known_cols <- required_cols
  species_cols <- setdiff(colnames(community), known_cols)
  
  if (length(species_cols) == 0) {
    warning("No species columns detected. We expected at least one abundance column.\n",
            "Check your input file or adjust 'required_cols' if your file uses different names.\n")
    return(invisible(NULL))
  }
  
  # Prepare results data frame
  results <- data.frame(
    species = species_cols,
    STI = NA_real_,
    n_occ = NA_integer_
  )
  
  # Compute STI for each species
  for (i in seq_along(species_cols)) {
    sp_col <- species_cols[i]
    
    # Check numeric
    if (!is.numeric(community[[sp_col]])) {
      warning(sprintf(
        "Column '%s' is not numeric. Coercing to numeric may produce NAs.\n",
        sp_col
      ))
    }
    
    present_idx <- which(as.numeric(community[[sp_col]]) > 0)
    
    if (length(present_idx) == 0) {
      # No occurrences for this species
      results$STI[i] <- NA
      results$n_occ[i] <- 0
    } else {
      # Extract the temperatures for rows where the species is present
      temp_vals <- community$temperature[present_idx]
      results$STI[i] <- mean(temp_vals, na.rm = TRUE)
      results$n_occ[i] <- length(temp_vals[!is.na(temp_vals)])
    }
  }
  
  # Write results
  write.csv(results, file = output_file, row.names = FALSE)
  
  message(sprintf("STI computation complete for '%s' data.", data_name))
  message(sprintf("Results saved to '%s'.", output_file))
  
  # Additional summary warnings
  total_species <- nrow(results)
  species_no_occ <- sum(results$n_occ == 0, na.rm = TRUE)
  if (species_no_occ > 0) {
    warning(sprintf(
      "%d out of %d species had no occurrences (>0) in the dataset. Their STI is NA.\n",
      species_no_occ, total_species
    ))
  }
  
  return(results)
}


# -------------------------------------------------------------------
# Example Usage
# -------------------------------------------------------------------
# 1. Define 'data_name' and input/output directories
# data_name <- "ppkt"
# input_dir <- "../data"
# output_dir <- "../data"

# 2. Call the function
# results <- compute_STI(
#   data_name = data_name,
#   input_dir = input_dir,
#   output_dir = output_dir
# )

# 3. 'results' now contains columns:
#    - species: species name
#    - STI: average experienced temperature
#    - n_occ: number of occurrences for that species
#
# The function also saves a CSV file with these columns to 'output_dir'.
#
# Notes:
# - The script checks for the columns: "SampleID", "StationID", "year", "long", "lat",
#   "temperature", "actual". If any are missing, it will warn and exit.
# - If your file uses different metadata column names, edit 'required_cols'.
# - Warnings in the console will guide you if the file is missing, columns are incorrect,
#   or there's unexpected data structure.
# -------------------------------------------------------------------
