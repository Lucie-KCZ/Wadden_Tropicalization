# -------------------------------------------------------------------
# Script: Extracting Data from NetCDF Files
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski, Helmut Hillebrand, to be completed
#
# Date:    January 2025
# Updated: January 2025
#
# Description:
# This script provides two main functions to handle NetCDF data:
# 1) `extract_netcdf()` for extracting a variable (or variables) from a single .nc file,
#    converting them to .tif raster format, and optionally cropping to a bounding box.
# 2) `process_netcdf_directory()` for processing multiple .nc files in parallel.
#
# NOTE: Please remove or comment out the environment-clearing lines if undesired.
# -------------------------------------------------------------------

# Clean the environment (be cautious if you keep them active)
for (i in 1:10) gc(reset = TRUE)
rm(list = ls())

library(ncdf4)      # for handling NetCDF files
library(raster)     # for raster operations
library(tidyverse)  # for data manipulation
library(terra)      # modern replacement for some raster functionalities


# -------------------------------------------------------------------
#' Extract Data from a Single NetCDF File
#'
#' This function reads a specified NetCDF (\code{.nc}) file, extracts the desired variable(s),
#' converts them into GeoTIFF raster(s), and optionally crops to a bounding box. The output
#' files are saved in the provided \code{output_dir}.
#'
#' @param nc_file Character. Path to the input NetCDF file (\code{.nc}).
#' @param output_dir Character. Directory where the resulting \code{.tif} files will be saved.
#' @param var_name Character or \code{NULL}. Name of the variable to extract. If \code{NULL},
#'   all variables in the file are processed.
#' @param verbose Logical. Whether to print status messages (\code{TRUE}) or remain silent (\code{FALSE}).
#' @param bbox A named list of four elements specifying the bounding box for cropping,
#'   e.g., \code{list(minlon=..., maxlon=..., minlat=..., maxlat=...)}. If \code{NULL}, no cropping is performed.
#'
#' @return A named list of file paths for the generated raster files. The function also writes
#'   the rasters to \code{output_dir} in GeoTIFF format.
#'
#' @details
#' \enumerate{
#'   \item Opens the NetCDF file (\code{nc_file}) via \pkg{ncdf4}.
#'   \item Identifies target variable(s) \code{var_name}. If none is provided, all variables are processed.
#'   \item Converts each 2D variable into a \pkg{raster} object, infers georeferencing from \code{lon} and \code{lat} dims.
#'   \item If \code{bbox} is provided, crops the resulting raster to those coordinates.
#'   \item Saves each extracted raster to \code{output_dir}, e.g., \code{"basename_variable.tif"}.
#' }
#'
#' @examples
#' \dontrun{
#'   nc_file <- "path/to/my_file.nc"
#'   out_dir <- "path/to/output"
#'   # bounding box for Wadden Sea
#'   wadden_bbox <- list(minlon = 3.368063, maxlon = 8.913279,
#'                       minlat = 53.198493, maxlat = 55.975668)
#'
#'   # Extract 'sst' variable, crop to wadden_bbox
#'   extract_netcdf(nc_file, out_dir, var_name = "sst",
#'                  verbose = TRUE, bbox = wadden_bbox)
#' }
#'
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom raster raster extent crs writeRaster crop
#' @importFrom tools file_path_sans_ext
#' @import progress
#' @export
extract_netcdf <- function(nc_file,
                           output_dir,
                           var_name = NULL,
                           verbose = TRUE,
                           bbox = NULL) {
  
  # Required packages (progress is used for status output)
  if (!requireNamespace("progress", quietly = TRUE)) {
    install.packages("progress")
  }
  library(progress)
  
  # Helper to print status if verbose
  status_msg <- function(msg) {
    if (verbose) cat(paste0("[INFO] ", msg, "\n"))
  }
  
  # Validate bbox
  if (!is.null(bbox)) {
    if (!is.list(bbox) || length(bbox) != 4) {
      stop("bbox must be a list with 4 elements: c(minlon, maxlon, minlat, maxlat)")
    }
    status_msg(sprintf("Filtering for area: Lon [%.2f to %.2f], Lat [%.2f to %.2f]",
                       bbox$minlon, bbox$maxlon, bbox$minlat, bbox$maxlat))
  }
  
  # Create output directory if needed
  status_msg("Creating output directory...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Prepare base name for output
  base_name <- tools::file_path_sans_ext(basename(nc_file))
  status_msg(paste("Processing file:", base_name))
  
  # Open NetCDF
  status_msg("Opening NetCDF file...")
  nc <- ncdf4::nc_open(nc_file)
  
  # Identify which variable(s) to read
  if (is.null(var_name)) {
    var_names <- names(nc$var)
  } else {
    var_names <- var_name
  }
  status_msg(paste("Found", length(var_names), "variables to process"))
  
  output_files <- list()
  
  # Process each variable
  for (var in var_names) {
    status_msg(paste("\nProcessing variable:", var))
    
    # Read variable data
    status_msg("Reading variable data...")
    data <- ncdf4::ncvar_get(nc, var)
    
    # Only handle 2D data in this script
    if (length(dim(data)) == 2) {
      status_msg("Creating raster...")
      r <- raster::raster(t(data))
      
      # If dims are named lon/lat, set projection
      if ("lon" %in% names(nc$dim) && "lat" %in% names(nc$dim)) {
        status_msg("Setting geographic projection...")
        lons <- ncdf4::ncvar_get(nc, "lon")
        lats <- ncdf4::ncvar_get(nc, "lat")
        
        # Convert 0-360 longitudes to -180..180 if needed
        if (max(lons, na.rm = TRUE) > 180) {
          status_msg("Converting longitudes from 0-360 to -180..180 format...")
          lons <- ((lons + 180) %% 360) - 180
        }
        
        raster::extent(r) <- raster::extent(min(lons), max(lons),
                                            min(lats), max(lats))
        raster::crs(r) <- "+proj=longlat +datum=WGS84"
        
        # Crop if bbox is supplied
        if (!is.null(bbox)) {
          status_msg("Cropping raster to specified extent...")
          r <- raster::crop(r, raster::extent(bbox$minlon,
                                              bbox$maxlon,
                                              bbox$minlat,
                                              bbox$maxlat))
        }
      }
      
      # Write out as GeoTIFF
      status_msg("Saving raster file...")
      raster_file <- file.path(output_dir, paste0(base_name, "_", var, ".tif"))
      raster::writeRaster(r, raster_file, format = "GTiff", overwrite = TRUE)
      output_files[[paste0(var, "_raster")]] <- raster_file
    }
  }
  
  # Close NetCDF
  ncdf4::nc_close(nc)
  status_msg("Processing complete!")
  return(output_files)
}


# -------------------------------------------------------------------
#' Process Multiple NetCDF Files in Parallel
#'
#' This function looks for all NetCDF (\code{.nc}) files in a given \code{input_dir}, then
#' runs \code{\link{extract_netcdf}} on each file in parallel. It can filter files by a
#' pattern (e.g., only those matching \code{\\.nc\$}), optionally crop to a bounding box,
#' and select a specific variable to extract.
#'
#' @param input_dir Character. Directory containing NetCDF files.
#' @param output_dir Character. Directory where extracted GeoTIFFs will be saved.
#' @param pattern Character. Regex pattern to match files in \code{input_dir}.
#'   Defaults to \code{"\\\\.nc\$"} for NetCDF files.
#' @param bbox Optional bounding box (see \code{\link{extract_netcdf}}).
#' @param var_name Optional. Name of the variable in the NetCDF to extract.
#' @param n_cores Integer. Number of cores to use for parallel processing. If \code{NULL},
#'   uses \code{detectCores() - 1}.
#' @param verbose Logical. Whether to print progress messages.
#' @param rm_NA Logical, not currently used in the function but could be used for removing
#'   or masking NA values. Retained for potential future extension.
#'
#' @return A named list of lists, where each element corresponds to one NetCDF file processed,
#'   containing the file name, status, and resulting output from \code{extract_netcdf}.
#'
#' @details
#' Steps:
#' \enumerate{
#'   \item Identify all matching \code{.nc} files in \code{input_dir}.
#'   \item Create a parallel cluster (\code{parLapplyLB}) to process each file.
#'   \item For each file, call \code{\link{extract_netcdf}} with the desired bounding box
#'         and variable name.
#'   \item Collect results, including any errors, and return them as a list.
#' }
#'
#' @examples
#' \dontrun{
#'   wadden_bbox <- list(
#'     minlon = 3.368063,
#'     maxlon = 8.913279,
#'     minlat = 53.198493,
#'     maxlat = 55.975668
#'   )
#'
#'   # Process all .nc files in raw/AquaMODIS folder,
#'   # convert to GeoTIFF in processed/AquaMODIS folder,
#'   # extracting 'sst' variable for bounding box wadden_bbox.
#'   results <- process_netcdf_directory(
#'     input_dir  = "/path/to/data/raw/AquaMODIS",
#'     output_dir = "/path/to/data/processed/AquaMODIS",
#'     pattern    = "\\.nc$",
#'     bbox       = wadden_bbox,
#'     var_name   = "sst",
#'     n_cores    = 4,
#'     verbose    = TRUE
#'   )
#'
#'   # Check how many files succeeded vs. failed:
#'   # lapply(results, function(x) x$status)
#' }
#'
#' @importFrom parallel makeCluster stopCluster parLapplyLB detectCores clusterExport clusterEvalQ
#' @importFrom progress progress_bar
#' @importFrom ncdf4 nc_open
#' @importFrom raster raster
#' @importFrom tools file_path_sans_ext
#' @export
process_netcdf_directory <- function(input_dir,
                                     output_dir,
                                     pattern = "\\.nc$",
                                     bbox = NULL,
                                     var_name = NULL,
                                     n_cores = NULL,
                                     verbose = TRUE,
                                     rm_NA = TRUE) {
  
  required_packages <- c("parallel", "progress", "ncdf4", "raster",
                         "tidyverse", "terra")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # List all .nc files
  nc_files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
  n_files <- length(nc_files)
  if (n_files == 0) {
    stop("No NetCDF files found in the input directory matching the pattern")
  }
  
  # Decide how many cores to use
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  n_cores <- min(n_cores, n_files)
  
  if (verbose) {
    cat(sprintf("[INFO] Found %d files to process\n", n_files))
    cat(sprintf("[INFO] Using %d cores\n", n_cores))
  }
  
  # Optional progress bar
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :current/:total files (:percent) - est. :eta",
      total = n_files,
      width = 80
    )
    progress_fn <- function(n) {
      pb$tick()
    }
  }
  
  # Create cluster
  cl <- parallel::makeCluster(n_cores, type = "FORK")
  
  # Export extract_netcdf to cluster environment
  parallel::clusterExport(cl, c("extract_netcdf"), envir = environment())
  
  # Make sure all required packages are loaded on each core
  parallel::clusterEvalQ(cl, {
    library(ncdf4)
    library(raster)
    library(tidyverse)
    library(terra)
    library(progress)
  })
  
  # Process files in parallel
  results <- tryCatch({
    if (verbose) {
      parallel::parLapplyLB(cl, nc_files, function(file) {
        result <- try(extract_netcdf(file, output_dir,
                                     var_name  = var_name,
                                     verbose   = FALSE,
                                     bbox      = bbox,
                                     # rm_NA param not used inside extract_netcdf but kept for extension
                                     rm_NA     = rm_NA))
        progress_fn(1)
        return(list(
          file   = basename(file),
          result = result,
          status = !inherits(result, "try-error"),
          error  = if (inherits(result, "try-error")) result else NULL
        ))
      })
    } else {
      parallel::parLapplyLB(cl, nc_files, function(file) {
        result <- try(extract_netcdf(file, output_dir,
                                     var_name  = var_name,
                                     verbose   = FALSE,
                                     bbox      = bbox,
                                     rm_NA     = rm_NA))
        return(list(
          file   = basename(file),
          result = result,
          status = !inherits(result, "try-error"),
          error  = if (inherits(result, "try-error")) result else NULL
        ))
      })
    }
  }, finally = {
    parallel::stopCluster(cl)
  })
  
  # Summarize results
  successful <- sum(sapply(results, function(x) x$status))
  failed <- n_files - successful
  
  if (verbose) {
    cat(sprintf("\n[INFO] Processing complete:\n"))
    cat(sprintf("       - Successfully processed: %d files\n", successful))
    cat(sprintf("       - Failed: %d files\n", failed))
    
    if (failed > 0) {
      cat("\nFailed files:\n")
      for (result in results) {
        if (!result$status) {
          cat(sprintf("- %s: %s\n", result$file, result$error))
        }
      }
    }
  }
  
  # Return as a named list
  names(results) <- sapply(results, function(x) x$file)
  return(results)
}


# -------------------------------------------------------------------
# Example Usage
# -------------------------------------------------------------------

# Example bounding box for the Wadden Sea
wadden_bbox <- list(
  minlon = 3.368063,
  maxlon = 8.913279,
  minlat = 53.198493,
  maxlat = 55.975668
)

# Single file extraction
# result <- extract_netcdf(
#   nc_file     = "/path/to/my_file.nc",
#   output_dir  = "/path/to/output",
#   var_name    = "sst",
#   verbose     = TRUE,
#   bbox        = wadden_bbox
# )

# Multiple files in a directory
results <- process_netcdf_directory(
  input_dir  = "/Users/lucie/Documents/Work/Data/TerraModis/data/raw/AquaMODIS",
  output_dir = "/Users/lucie/Documents/Work/Data/TerraModis/data/processed/AquaMODIS",
  pattern    = "\\.nc$",
  bbox       = wadden_bbox,
  var_name   = "sst",
  n_cores    = 4,
  verbose    = TRUE
)
