# Import the data
temperatures <- read.csv('TOREPLACE') 
community <- read.csv('TOREPLACE')

# Define the function to compute STI
local_STI <- function(community_matrix, temperatures, infos_col) {
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
  
  output <- data.frame(species = names(STI), STI = STI) ; rownames(output) <- NULL
  
  return(output)
}

# Compute STI on your data
obs_local_STI <- local_STI(temperatures = temperatures, community_matrix = community, infos_col = 1:5)

# Compute species occurrences
species_occurrences <- apply(ifelse(community[, -infos_col] > 0, 1, 0), 2, sum)
species_occurrences <- data.frame(species = names(species_occurrences), nb_occurrence = species_occurrences)

# Merge STI and occurrences
toshare_STI <- merge(x = obs_local_STI, y = species_occurrences, by = 'species', all.x = T)

# Save it in your working directory
# If you're not sure where your working directory is: getwd()
save(toshare_STI, file = 'toshare_NAME.RData')