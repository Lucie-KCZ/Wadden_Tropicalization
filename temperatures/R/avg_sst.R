# -------------------------------------------------------------------
# Script: Compute Average SST Raster
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski
#
# Date:    January 2025
# Updated: January 2025
#
# Description:
# This script defines a function to read multiple GeoTIFF files from
# a specified directory, compute their mean using the terra package,
# and write out the resulting average raster. Useful for combining
# monthly or yearly SST files into a single composite (e.g., an overall
# mean across time).
#
# NOTE: Adjust the filtering (e.g., by year) as needed to select only
#       specific files from the directory.
# -------------------------------------------------------------------

#' Compute an Average SST Raster from Multiple GeoTIFF Files
#'
#' This function reads all `.tif` files (or a specified subset) from
#' a directory, builds a \code{SpatRaster} stack using \pkg{terra},
#' computes the mean across all layers, and saves the output as a new
#' GeoTIFF file.
#'
#' @param data_dir Character. Path to the directory containing input
#'   GeoTIFF files (e.g., processed monthly SST).
#' @param output_dir Character. Directory where the resulting average
#'   raster will be saved.
#' @param output_file Character. Filename (including path) for the
#'   output GeoTIFF. Typically something like
#'   \code{file.path(output_dir, "average_sst.tif")}.
#' @param pattern Regex pattern to match `.tif` files. Defaults to
#'   `\\.tif$`, but can be altered to filter specific filenames.
#'
#' @details
#' Steps:
#' \enumerate{
#'   \item Lists all GeoTIFF files in \code{data_dir} matching
#'         \code{pattern}.
#'   \item Builds a \code{SpatRaster} collection (\pkg{terra}) of
#'         these files without loading all data into memory.
#'   \item Computes the mean across layers using \code{mean(rast_stack, na.rm=TRUE)}.
#'   \item Writes the output raster to \code{output_file}.
#' }
#'
#' @return Invisibly returns the output file path, for convenience. Also
#'   writes the resulting mean raster to disk as a GeoTIFF.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   data_dir   <- "/path/to/processed/AquaMODIS"
#'   output_dir <- "/path/to/processed"
#'   output_file <- file.path(output_dir, "average_sst.tif")
#'
#'   # Compute average across all .tif in data_dir
#'   compute_average_sst(data_dir, output_dir, output_file)
#' }
#'
#' @import terra
#' @export
compute_average_sst <- function(data_dir,
                                output_dir,
                                output_file,
                                pattern = "\\.tif$") {
  # Make sure terra is loaded
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required but not installed.")
  }
  library(terra)
  
  # List all .tif files in data_dir
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop(sprintf("No TIFF files found in '%s' matching pattern '%s'.", data_dir, pattern))
  }
  
  # Optionally, create output_dir if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Build a SpatRaster stack
  rast_stack <- rast(files)
  
  # Compute mean across all layers
  mean_raster <- mean(rast_stack, na.rm = TRUE)
  
  # Write the output raster
  writeRaster(mean_raster, filename = output_file, overwrite = TRUE)
  cat("Average SST raster saved to", output_file, "\n")
  
  invisible(output_file)
}


# -------------------------------------------------------------------
# Example Usage
# -------------------------------------------------------------------

# You can define the directories and output file, then call the function:
data_dir   <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/AquaMODIS"
output_dir <- "/Users/lucie/Documents/Work/Data/TerraModis/data/processed"
output_file <- file.path(output_dir, "average_sst.tif")

# Filter to .tif files in data_dir and compute mean
compute_average_sst(data_dir, output_dir, output_file)

# -------------------------------------------------------------------
