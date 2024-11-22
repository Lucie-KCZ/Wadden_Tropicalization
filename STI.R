#######################################
### Experienced Temperature per Sp. ###
#######################################

# Authors list - To be completed
# Lucie Kuczynski, Helmut Hillebrand

# L. Kuczynski
# lucie.kuczynski@hotmail.com
# June, 2024
# Last edit: July 2024

# This script processes biodiversity data from the Wadden Sea to calculate the experienced temperatures per species. 
# It merges temperature data with species occurrence data.

# Clean out your work environment (be cautious, this will remove all objects from the workspace)
for(i in 1:10) gc(reset = TRUE)
rm(list = ls())

# Structure of the script:
# L61-118: Macrozoobenthos data
# L119-132: Phytoplankton data

# MISC. FUNCTIONS
# Get the n last characters of a string
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

# Function to local STIs and total abundances of each species
local_STI <- function(community_matrix, temperatures, infos_col, plot = F) {
  # temperatures: DataFrame with columns "Station", "Year", and "Temperature"
  # community_matrix: DataFrame with columns "Station", "Year", and occurrence data
  # infos_col: Indices of columns in community_matrix which are not occurrence data
  
  # Check for required columns in the temperatures DataFrame
  if(any(!c('Station', 'Year', 'Temperature') %in% colnames(temperatures))) {
    stop('Update the column names in temperatures to have "Station", "Year", and "Temperature"')
  }
  
  # Check for required columns in the community_matrix DataFrame
  if(any(!c('Station', 'Year') %in% colnames(community_matrix))) {
    stop('Update the column names in community_matrix to have "Station" and "Year"')
  }
  
  # Convert occurrence data to presence/absence (1/NA)
  community_matrix[, -infos_col] <- apply(community_matrix[, -infos_col], 2, function(x) ifelse(x > 0, 1, NA))
  
  # Merge temperature data with community matrix
  data <- merge(x = temperatures, y = community_matrix, by = c('Station', 'Year'), all.x = FALSE, all.y = TRUE)
  
  # Calculate mean experienced temperatures for each species
  STI <- apply(X = data[, -c(1, 1 + infos_col)], MARGIN = 2, 
                            FUN = function(x) mean(x * data$Temperature, na.rm = T))
  
  # Plot the distribution of the STI for EACH species
  if(plot) apply(X = data[, -c(1, 1 + infos_col)], MARGIN = 2, FUN = function(x) try(hist(x * data$Temperature), silent = T))
  
  output <- data.frame(species = names(STI), STI = STI) ; rownames(output) <- NULL
  
  return(output)
}

# MACROZOOBENTHOS (MZB) DATA 
# Import temperature data
temperatures <- read.csv('~/Documents/Work/Data/Data_Oldenburg/Wadden Sea/MZB/MZB_NLWKN_Begleit_gesamt_2020.csv')

# Keep only relevant columns (Station, Year, Temperature)
temperatures <- temperatures[, c(1, 2, 18)]

# Extract the year from the date column
temperatures$Year <- as.numeric(sub(".*?([0-9]{4}).*", "\\1", temperatures$Datum))

# Rename columns
colnames(temperatures) <- c('Station', 'Date', 'Temperature', 'Year')

# Import community data
community <- read.csv('../data/mzb_abundances_wide.csv')[, -1]
colnames(community)[2:3] <- c('Station', 'Year')

# Initialize an empty DataFrame to store the processed temperature data
output <- data.frame()

# Loop over each unique SampleID in the community data
for(sample_id in unique(community$SampleID)) {
  # Extract the year from the SampleID
  year <- substrRight(sample_id, 4)
  
  # Extract the station name by removing the year part from the SampleID
  station <- gsub(paste0('_', year), '', sample_id)
  
  # Filter the temperatures data for the specific station and year
  temperature_sample <- temperatures[temperatures$Station == station & temperatures$Year == year, ]
  
  # Calculate the mean temperature for the specific station and year
  if(nrow(temperature_sample) != 0) {
    temp_mean <- mean(temperature_sample$Temperature, na.rm = TRUE)
    output_sample <- data.frame(Station = station, Year = year, Temperature = temp_mean)
  } else {
    output_sample <- data.frame(Station = station, Year = year, Temperature = NA)
  }
  
  # Append the results to the output DataFrame
  output <- rbind(output, output_sample)
}

# Replace the temperatures DataFrame with the processed output
temperatures <- output

# Calculate local STIs
mzb_local_STI <- local_STI(temperatures = temperatures, community_matrix = community, infos_col = 1:5)

# Save the data
save(mzb_local_STI, file = '../data/mzb_local_STI.RData')

# Cleaning up
rm(temperatures, temperature_sample, community, output, 
   sample_id, mzb_local_STI, station, year, temp_mean, 
   output_sample)

# PHYTOPLANKTON (PPKT) DATA
# Import temperature data
temperatures <- unique(read.csv(sep = ';', dec = ',',
  '~/Documents/Work/Data/Data_Oldenburg/Wadden Sea/PPKT/pp_wadden_env_newdata_10062022.csv')[, c(3, 6, 13)])
colnames(temperatures) <- c('Station', 'Year', 'Temperature')

# Import community data
community <- read.csv('../data/ppkt_wide.csv')[, -1]
colnames(community)[2:3] <- c('Station', 'Year')

ppkt_local_STI <- local_STI(temperatures = temperatures, community_matrix = community, infos_col = 1:5, plot = T)

save(ppkt_local_STI, file = '../data/ppkt_local_STI.RData')





