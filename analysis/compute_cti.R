# -------------------------------------------------------------------
# Script: Community Temperature Index (CTI) 
# -------------------------------------------------------------------
# Authors:
#   Lucie Kuczynski
#
# Date:    July 2024
# Updated: February 2025
#
# Description:
# A comprehensive set of functions for processing biodiversity data and
# calculating the experienced temperatures per species (Community Temperature
# Index - CTI). The script provides functionality to merge temperature data
# with species occurrence data, compute CTI, and analyze processes like
# tropicalization and borealization.
#
# Required packages: 
#   nlme (>= 3.1-166)
#   MuMIn (>= 1.48.4)
#   ggeffects (>= 2.1.0)
#   scales (>= 1.3.0)
# -------------------------------------------------------------------

#' Function: get_CTI
#' Calculate the Community Temperature Index (CTI)
#'
#' Given a community data frame (species x samples) and a data frame of species thermal indices (STI),
#' this function computes the CTI for each sample. CTI is computed by taking the (weighted) mean
#' of each species' STI, weighted by its relative abundance in the sample.
#'
#' @param community_matrix A data frame containing the community data. It must have:
#'   \itemize{
#'     \item Several columns of species abundances or occurrences.
#'     \item Additional "info" columns (station, year, etc.) specified by \code{infos_col}.
#'   }
#' @param STI A data frame of species thermal indices. Must contain at least the columns:
#'   \itemize{
#'     \item \code{species}: species names matching the column names in \code{community_matrix}.
#'     \item \code{STI}: numeric values of species' thermal indices.
#'   }
#' @param infos_col Integer vector indicating which columns in \code{community_matrix}
#'   are non-species columns (e.g., \code{c(1:5)}).
#' @param occu Logical indicating whether to treat abundance data as presence/absence (\code{TRUE})
#'   or use raw abundances (\code{FALSE}). Default is \code{TRUE} (presence/absence).
#'
#' @return A data frame with the original "info" columns plus a new column \code{CTI}, indicating
#'   the Community Temperature Index for each row (sample).
#'
#' @details
#' Steps:
#' \enumerate{
#'   \item Optionally convert abundances to presence/absence.
#'   \item Compute relative abundance for each species in each sample.
#'   \item Use \link[stats]{weighted.mean} with species STI as \code{x} and
#'         relative abundance as \code{w} to compute CTI.
#'   \item Return the original info columns plus \code{CTI}.
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose community_matrix has columns 1:5 as info and 6:10 as species.
#' # Suppose STI has columns "species" and "STI".
#' # CTI_result <- get_CTI(community_matrix, STI, infos_col = 1:5, occu = TRUE)
#' }
#'
#' @export
get_CTI <- function(community_matrix, STI, infos_col, occu = TRUE){
  
  if(!"STI" %in% colnames(STI)) {
    stop('No "STI" column identified in the STI data frame.')
  }
  if(!"species" %in% colnames(STI)) {
    stop('No "species" column identified in the STI data frame. Need to match your community column names.')
  }
  
  # Optionally convert abundance to presence/absence
  if (occu) {
    community_matrix[, -infos_col] <- ifelse(community_matrix[, -infos_col] > 0, 1, 0)
  }
  
  # Compute relative abundance by row
  relative_abundances <- t(apply(
    community_matrix[, -infos_col],
    1,
    function(x) x / sum(x, na.rm = TRUE)
  ))
  
  # If any row sums to zero (all species 0), handle division by zero
  row_sums <- apply(community_matrix[, -infos_col], 1, sum, na.rm = TRUE)
  zero_rows <- which(row_sums == 0)
  if (length(zero_rows) > 0) {
    warning(sprintf(
      "%d row(s) in community_matrix have zero total abundance. Their CTI will be NA.\n", 
      length(zero_rows)
    ))
  }
  
  # Create a named vector of STI for each species
  # => This ensures we match species columns to the correct STI
  species_to_STI <- setNames(STI$STI, STI$species)
  
  # Weighted mean by row
  # If a species from community_matrix isn't in STI, weighted.mean will see NA for that weight.
  # You could do an intersection if needed, but let's proceed to handle NA if not matched.
  output_CTI <- apply(relative_abundances, 1, function(z) {
    # Weighted mean of species STI by relative abundance
    # Attempt to align each column with a species name
    sp_names <- colnames(relative_abundances)  # species columns in the community
    w <- z  # weights
    x <- species_to_STI[sp_names]  # STI values in the same order as z
    
    # Weighted mean ignoring NA
    wm <- stats::weighted.mean(x, w, na.rm = TRUE)
    return(wm)
  })
  
  # Combine CTI results with the original info columns
  result_df <- data.frame(
    community_matrix[, infos_col],
    CTI = output_CTI
  )
  
  return(result_df)
}



# -------------------------------------------------------------------

#' Function: plot_trend
#' Plot Linear Mixed-Effects Trend of CTI Over Time
#'
#' Given a data frame containing columns \code{CTI}, \code{Year}, and \code{Station},
#' this function fits a mixed-effects model with CTI ~ Year + (1|Station) and plots
#' both the raw data (light lines per station) and the model fit plus confidence intervals.
#'
#' @param CTI A data frame containing at least \code{CTI}, \code{Year}, and \code{Station}.
#'
#' @return No direct return value; a plot is produced.
#'
#' @importFrom nlme lme
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom ggeffects ggpredict
#' @export
plot_trend <- function(CTI) {
  
  
  if(!require(nlme)) { install.packages("nlme"); library(nlme) }
  if(!require(MuMIn)) { install.packages("MuMIn"); library(MuMIn) }
  if(!require(ggeffects)) { install.packages("ggeffects"); library(ggeffects) }
  
  # Making sure the right colnames are used
  colnames(CTI) <- tolower(colnames(CTI))
  colnames(CTI)[grep('station', colnames(CTI))] <- 'station'
  
  required_cols <- c("cti", "year", "station")
  missing_cols <- setdiff(required_cols, colnames(CTI))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "Missing columns in 'CTI' data for plot_trend: %s\nPlot may fail or produce errors.",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Setup plot area
  CTI_limits <- c(floor(min(CTI$cti, na.rm = TRUE)),
                  ceiling(max(CTI$cti, na.rm = TRUE)))
  year_limits <- range(CTI$year, na.rm = TRUE)
  
  plot(x = 1, y = 1, xlim = year_limits, ylim = CTI_limits, type = 'n', axes = FALSE,
       xlab = '', ylab = '')
  axis(side = 1, tcl = -0.5, lwd = 2, las = 2,
       at = seq(year_limits[1], year_limits[2], by = 5), cex.axis = 1.2, padj = 0.5)
  axis(side = 2, tcl = -0.5, lwd = 2, las = 1,
       at = seq(CTI_limits[1], CTI_limits[2], by = 1), cex.axis = 1.2)
  mtext('Year', side = 1, line = 3, cex = 1.5)
  mtext('CTI', side = 2, line = 3, cex = 1.5)
  
  # Draw light lines per station
  by(CTI, CTI$station, function(x) {
    lines(x$cti ~ x$year, col = 'snow2', lwd = 1, lty = 1)
  })
  
  # Fit LME model
  if (!requireNamespace("nlme", quietly = TRUE) ||
      !requireNamespace("MuMIn", quietly = TRUE) ||
      !requireNamespace("ggeffects", quietly = TRUE)) {
    warning("Packages 'nlme', 'MuMIn', and 'ggeffects' are required for full plotting.")
    return(invisible(NULL))
  }
  
  model <- tryCatch(
    lme(fixed = cti ~ year, random = ~1 | station, data = CTI, method = "REML"),
    error = function(e) {
      warning("Failed to fit LME model. Check your CTI data. Returning incomplete plot.")
      return(NULL)
    }
  )
  if (is.null(model)) return(invisible(NULL))
  
  print(summary(model))
  print(r.squaredGLMM(model))
  
  # Predict with ggeffects
  predictions <- as.data.frame(ggpredict(model, "year", nsim = 999, type = 'simulate'))
  
  # Plot polygon for confidence intervals
  pcol <- ifelse(summary(model)$tTable[2,5] > 0.05, '#778da9', '#ae2012')
  lcol <- ifelse(summary(model)$tTable[2,5] > 0.05, '#415a77', '#9b2226')
  
  polygon(
    y = c(predictions$conf.low, rev(predictions$conf.high), predictions$conf.low[1]),
    x = c(predictions$x, rev(predictions$x), predictions$x[1]),
    col = pcol,
    border = pcol
  )
  
  lines(predictions$x, predictions$predicted, lwd = 3, col = lcol)
}



# -------------------------------------------------------------------

#' Function: get_processes
#' Assess Tropicalisation/Borealisation Processes
#'
#' This function analyzes species trends relative to their thermal index (STI)
#' and the average CTI, labeling changes as \code{tropicalisation}, \code{borealisation}, etc.
#'
#' @param community_matrix A data frame containing species abundance data plus
#'   columns named \code{Station} and \code{Year}. 
#' @param STI A data frame with columns \code{species} and \code{STI}.
#' @param CTI A data frame output from \code{\link{get_CTI}}, containing columns
#'   \code{Station} and \code{CTI}.
#' @param infos_col Integer vector specifying which columns in \code{community_matrix}
#'   are non-species columns.
#' @param all_outputs Logical. If \code{TRUE}, returns a list with \code{trends} (per-species regression
#'   results) and \code{process_strength}. If \code{FALSE}, returns only the \code{process_strength} table.
#' @param log Logical. If \code{TRUE}, log-abundance trends are computed via \code{lm(log(x+1) ~ Year)}.
#'
#' @return Either a list with \code{trends} and \code{process_strength}, or just
#'   \code{process_strength} depending on \code{all_outputs}.
#'
#' @details
#' Steps:
#' \enumerate{
#'   \item For each station, remove species absent across all years.
#'   \item Fit a linear model of species abundance vs. \code{Year}, capturing the slope.
#'   \item Merge in species STI and compute the difference from average CTI at that station.
#'   \item Label processes as \code{tropicalisation}, \code{borealisation}, etc., based on slope sign and difference.
#'   \item Compute absolute and relative strengths of each process per station.
#' }
#'
#' @export
get_processes <- function(community_matrix, STI, CTI, infos_col, all_outputs = TRUE, log = TRUE) {
  
  # Quick checks
  # Colnames in community_matrix
  colnames(community_matrix)[infos_col] <- tolower(colnames(community_matrix)[infos_col])
  colnames(community_matrix)[grep('station', colnames(community_matrix))] <- 'station'
  
  required_cols <- c("year", "station")
  missing_cols <- setdiff(required_cols, colnames(CTI))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "Missing columns in 'community_matrix' data for get_processes: %s\nMay fail or produce errors.",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Colnames in CTI
  colnames(CTI) <- tolower(colnames(CTI))
  colnames(CTI)[grep('station', colnames(CTI))] <- 'station'
  
  required_cols <- c("cti", "year", "station")
  missing_cols <- setdiff(required_cols, colnames(CTI))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "Missing columns in 'CTI' data for get_processes: %s\nMay fail or produce errors.",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Merge species with STI
  if(!"species" %in% colnames(STI) || !"STI" %in% colnames(STI)) {
    stop('STI data frame must have columns "species" and "STI".')
  }
  
  trends <- NULL
  process_strength <- NULL
  
  for (station in unique(CTI$station)) {
    message(sprintf("Processing station: %s", station))
    community_station <- community_matrix[community_matrix$station == station, ]
    
    # Remove species with zero sum across all years
    no_abundance_cols <- which(apply(community_station[, -infos_col], 2, sum, na.rm = TRUE) == 0)
    if (length(no_abundance_cols) > 0) {
      # columns beyond max(infos_col)
      no_abundance_cols <- no_abundance_cols + max(infos_col)
      community_station <- community_station[, -no_abundance_cols, drop = FALSE]
      warning(sprintf(
        "At station '%s', removed %d species columns with total abundance = 0.\n",
        station, length(no_abundance_cols)
      ))
    }
    
    # Calculate trends in species abundance over time
    if (log) {
      # log(x+1) approach
      slope_func <- function(x) {
        model <- lm(log(x + 1) ~ community_station$year)
        c(estimate = summary(model)$coefficients[2, 1],
          pvalue = summary(model)$coefficients[2, 4])
      }
    } else {
      slope_func <- function(x) {
        model <- lm(x ~ community_station$year)
        c(estimate = summary(model)$coefficients[2, 1],
          pvalue = summary(model)$coefficients[2, 4])
      }
    }
    
    slope_mat <- apply(community_station[, -infos_col], 2, slope_func)
    slope_df <- data.frame(
      species = colnames(community_station)[-infos_col],
      estimate = slope_mat["estimate", ],
      pvalue = slope_mat["pvalue", ]
    )
    slope_df$trend <- ifelse(slope_df$estimate > 0, "increase", "decrease")
    
    # Merge with STI
    slope_df <- merge(slope_df, STI, by = 'species', all.x = TRUE)
    
    # Average CTI for the station
    avg_CTI_station <- mean(CTI$cti[CTI$station == station], na.rm = TRUE)
    
    # Difference from average CTI
    slope_df$difference <- slope_df$STI - avg_CTI_station
    
    # Assign process
    slope_df$process <- with(slope_df, ifelse(
      trend == 'increase' & difference > 0, 'tropicalisation',
      ifelse(trend == 'increase' & difference < 0, 'borealisation',
             ifelse(trend == 'decrease' & difference > 0, 'deborealisation',
                    ifelse(trend == 'decrease' & difference < 0, 'detropicalisation', NA)
             )
      )
    ))
    
    # Absolute strength
    abs_strength <- tapply(
      X = abs((slope_df$STI - avg_CTI_station) * slope_df$estimate),
      INDEX = slope_df$process,
      FUN = sum,
      na.rm = TRUE
    )
    abs_strength[is.na(abs_strength)] <- 0
    
    # Relative strength
    rel_strength <- 100 * round(abs_strength / sum(abs_strength, na.rm = TRUE), 3)
    proc_strength_df <- data.frame(
      Station = station,
      process = names(rel_strength),
      strength = rel_strength
    )
    
    slope_df <- data.frame(Station = station, slope_df)
    trends <- rbind(trends, slope_df)
    process_strength <- rbind(process_strength, proc_strength_df)
  }
  
  if (all_outputs) {
    rownames(process_strength) <- NULL
    list(trends = trends, process_strength = process_strength)
  } else {
    process_strength
  }
}



# -------------------------------------------------------------------

#' Function: plot_species
#' Plot Species Trends by Their Thermal Index
#'
#' Creates a scatterplot of \code{STI} vs. species abundance slope, colored by
#' the identified process (tropicalisation, borealisation, etc.), plus a simple
#' bar chart of process frequencies.
#'
#' @param processes A data frame typically from \code{get_processes(..., all_outputs=TRUE)$trends}.
#'
#' @return No direct return; a side-by-side plot is produced.
#'
#' @importFrom scales alpha
#' @importFrom ggplot2 ggplot aes geom_point theme
#' @export
plot_species <- function(processes) {
  
  if(!require(scales)) { install.packages("scales"); library(scales) }
  
  processes <- na.omit(processes)
  
  estimate_limits <- c(
    floor(min(processes$estimate, na.rm = TRUE)),
    ceiling(max(processes$estimate, na.rm = TRUE))
  )
  STI_limits <- c(
    floor(min(processes$STI, na.rm = TRUE)),
    ceiling(max(processes$STI, na.rm = TRUE))
  )
  
  op <- par(mar = c(5, 3, 2, 5), oma = c(1, 7, 1, 1), pty = 's', mfrow = c(1, 2))
  on.exit(par(op), add = TRUE)
  
  # Scatterplot: x = STI, y = estimate
  plot(1, 1, xlim = STI_limits, ylim = estimate_limits, type = 'n', axes = FALSE,
       xlab = '', ylab = '')
  axis(side = 1, tcl = -0.5, lwd = 2, las = 1, cex.axis = 1.2, padj = .5)
  mtext('STI (°C)', side = 1, line = 3, cex = 1.5)
  axis(side = 2, tcl = -0.5, lwd = 2, las = 2, cex.axis = 1.2)
  mtext('Trends in log-abundances', side = 2, line = 3, cex = 1.5)
  abline(h = 0, lty = 3, lwd = 2)
  
  # Color by process
  color_map <- c(
    'tropicalisation'   = '#DB3938',
    'borealisation'     = '#4C7AB0',
    'detropicalisation' = '#8E6898',
    'deborealisation'   = '#ED9C2F'
  )
  
  if (requireNamespace("scales", quietly = TRUE)) {
    process_colors <- scales::alpha(color_map[processes$process], 0.3)
  } else {
    process_colors <- color_map[processes$process]
  }
  
  cex_val <- log(1.5 * abs(processes$difference) + 1)
  points(x = processes$STI, y = processes$estimate,
         pch = 21, bg = process_colors, cex = cex_val)
  
  # Barplot
  freq_table <- table(processes$process)
  sorted_freq <- sort(freq_table, decreasing = TRUE)
  bar_positions <- barplot(sorted_freq,
                           col = color_map[names(sorted_freq)],
                           horiz = TRUE, axes = FALSE, names.arg = NA)
  axis(side = 1, cex.axis = 1.2)
  text(x = sorted_freq, y = bar_positions, pos = 2, cex = 1.2,
       labels = names(sorted_freq))
}



# -------------------------------------------------------------------

# Example Usage
# # 1. Define 'data_name' and input/output directories
# data_name <- "mzb"
# input_dir <- "../data"
# output_dir <- "../data"
# 
# # 2. Load and Process Data
# # (Adjust file names and paths as needed for your data structure)
# 
# # Read your wide community data - CSV
# community <- read.csv(file.path(input_dir, paste0(data_name, "_wide.csv")), stringsAsFactors = FALSE)[, -1]
# colnames(community)[2:3] <- c("Station", "Year")
# 
# # Read your local Species Thermal Index (STI) data - CSV produced from 'compute_STI.R'
# computed_STI <- read.csv(file.path(input_dir, paste0(data_name, "_STI.csv")))
# 
# # 3. Calculate the Community Temperature Index (CTI)
# observed_CTI <- get_CTI(
#   community_matrix = community,
#   STI = computed_STI,
#   infos_col = 1:5,  # columns for SampleID, Station, Year, etc.
#   occu = TRUE       # presence/absence (TRUE) or abundance-based (FALSE)
# )
# 
# # 4. Plot the CTI Trend Over Time
# plot_trend(observed_CTI)
# 
# # 5. Assess Processes (e.g., tropicalisation, borealisation)
# inferred_processes <- get_processes(
#   community_matrix = community,
#   STI = computed_STI,
#   CTI = observed_CTI,
#   infos_col = 1:5,
#   all_outputs = TRUE,
#   log = TRUE
# )
# 
# # 6. Plot Species Trends vs. STI
# plot_species(inferred_processes$trends)
# 
# Notes:
# - Make sure the columns "Station" and "Year" are present in your community data.
# - For the STI data frame, ensure it has 'species' and 'STI'.
# - The script can warn or stop if required columns are missing or data is improperly formatted.
# - Adjust 'infos_col' to match the number of non-species columns in your CSV.
