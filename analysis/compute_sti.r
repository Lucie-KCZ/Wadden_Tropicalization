# -------------------------------------------------------------------
# Script: Species Thermal Index (STI)
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski
#
# Date:    July 2024
# Updated: February 2025
#
# Description:
# This script reads an updated community file that includes both
# biodiversity (species) columns and a temperature column. It then
# computes the Species Thermal Index (STI) for each species, defined as
# the mean experienced temperature across all rows where the species
# is present.
#
# Required packages: None
# -------------------------------------------------------------------

#' Compute Species Thermal Index (STI) and Number of Occurrences
#'
#' This function computes the mean experienced temperature (STI) for each species
#' and counts the number of occurrences used in the calculation.
#'
#' @param data_name Character string indicating the name of the taxa studied.
#' @param input_dir Character string specifying the directory path containing the input file.
#' @param output_dir Character string specifying the directory for saving results.
#' @param show_progress Boolean. To show a progression bar. Default is TRUE.
#' 
#' @details
#' The input file must be named "{data_name}_temperatures.csv" and contain:
#' \itemize{
#'   \item Required columns: \code{SampleID}, \code{StationID}, \code{year}, \code{long}, \code{lat}, \code{temperature}, \code{actual}
#'   \item Additional columns for species abundances
#' }
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{species}: The species name
#'   \item \code{STI}: Mean temperature across all occurrences
#'   \item \code{n_occ}: Number of occurrences used
#' }
#'
#' @export
compute_STI <- function(data_name, input_dir = "../data", output_dir = "../data", show_progress = TRUE) {
  # Build the full path to the input data file
  input_file <- file.path(input_dir, paste0(data_name, "_temperatures.csv"))
  
  # Verify that the expected input file exists before proceeding
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }
  
  # Construct the path where results will be saved
  output_file <- file.path(output_dir, paste0(data_name, "_STI.csv"))
  
  # Read the community data, which should include both metadata and species columns
  community <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # Check if the community data has any rows
  if (nrow(community) == 0) {
    stop("The input file appears to be empty (0 rows)")
  }
  
  # Define the metadata columns required for proper STI computation
  required_cols <- c("SampleID", "StationID", "year", "long", "lat", "temperature", "actual")
  missing_required <- setdiff(required_cols, colnames(community))
  
  # If any required columns are missing, stop and alert the user
  if (length(missing_required) > 0) {
    stop(sprintf(
      "Missing required columns: %s",
      paste(missing_required, collapse = ", ")
    ))
  }
  
  # Identify which columns correspond to species by removing the known metadata columns
  species_cols <- setdiff(colnames(community), required_cols)
  
  # Ensure that there is at least one species column
  if (length(species_cols) == 0) {
    stop("No species columns detected")
  }
  
  # Prepare a data frame to store STI results for each species
  results <- data.frame(
    species = species_cols,
    STI = NA_real_,
    n_occ = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  if(show_progress) {
    # Create a progress bar to show the computation status across species
    pb <- txtProgressBar(min = 0, max = length(species_cols), style = 3)
  }
  
  # Loop over each species column and calculate the STI
  for (i in seq_along(species_cols)) {
    sp_col <- species_cols[i]
    
    # Convert species column to numeric if it is not already, since abundance data may be stored as text
    if (!is.numeric(community[[sp_col]])) {
      warning(sprintf("Converting column '%s' to numeric", sp_col))
      community[[sp_col]] <- as.numeric(community[[sp_col]])
    }
    
    # Identify all rows where the species is present (abundance > 0)
    present_idx <- which(community[[sp_col]] > 0)
    
    # If the species is never present, store NA and 0 occurrences
    if (length(present_idx) == 0) {
      results$STI[i] <- NA
      results$n_occ[i] <- 0
    } else {
      # Extract the temperature values for all occurrence records and compute the mean
      temp_vals <- community$temperature[present_idx]
      results$STI[i] <- mean(temp_vals, na.rm = TRUE)
      # Count how many non-missing temperature values were actually used
      results$n_occ[i] <- length(temp_vals[!is.na(temp_vals)])
    }
    
    if(show_progress) setTxtProgressBar(pb, i)
  }
  if(show_progress) close(pb)
  
  # Save the STI results to a CSV file for future reference or analysis
  write.csv(results, file = output_file, row.names = FALSE)
  
  # Print a summary to inform the user that the process is complete
  message(sprintf("STI computation complete for '%s' data", data_name))
  message(sprintf("Results saved to '%s'", output_file))
  
  # Warn if any species had zero occurrences
  species_no_occ <- sum(results$n_occ == 0, na.rm = TRUE)
  if (species_no_occ > 0) {
    warning(sprintf(
      "%d out of %d species had no occurrences",
      species_no_occ, nrow(results)
    ))
  }
  
  return(results)
}

# -------------------------------------------------------------------

# Example Usage
# # 1. Define 'data_name' and input/output directories
# data_name <- "mzb"
# input_dir <- "../data"
# output_dir <- "../data"
#
# # 2. Call the function
# results <- compute_STI(
#   data_name = data_name,
#   input_dir = input_dir,
#   output_dir = output_dir
# )
#
# # 3. 'results' now contains columns:
# #    - species: species name
# #    - STI: average experienced temperature
# #    - n_occ: number of occurrences for that species
# #
# # The function also saves a CSV file with these columns to 'output_dir'.
# #
# # Notes:
# # - The script checks for the columns: "SampleID", "StationID", "year", "long", "lat",
# #   "temperature", "actual". If any are missing, it will warn and exit.
# # - If your file uses different metadata column names, edit 'required_cols'.
# # - Warnings in the console will guide you if the file is missing, columns are incorrect,
# #   or there's unexpected data structure.
