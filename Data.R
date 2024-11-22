########################################
### Data for Trop. in the Wadden Sea ###
########################################

# Authors list - To be completed
# Lucie Kuczynski,Helmut Hillebrand

# L. Kuczynski
# lucie.kuczynski@hotmail.com
# May, 2024
# last edit: June 2024

for(i in 1:10) gc(reset = T) ; rm(list = ls())

# PATHS WILL LIKLEY CHANGE! 

# Structure of the script

# L26-56: Macrozoobenthos data
# L60-115: Phytoplankton data

# MISC. FUNCTIONS



# MACROZOOBENTHOS (MZB) DATA 
# Importing time series
mzb <- na.omit(read.csv(
  '/Users/lucie/Documents/Work/Data/Data_Oldenburg/Wadden Sea/MZB/MARISCO_MZB_raw.csv')[, -5])

# Saving in the local data folder
write.csv(x = mzb, file = '../data/mzb_abundances.csv')

# Converting into wide format using the labdsv library
# Creating a sample ID based on the station and the year
mzb$SampleID <- paste(mzb$StationID, mzb$year, sep = '_')

# Saving sample infos (StationID, year, lat and long, SampleID)
infos <- unique(mzb[, c(1, 3, 5:7)])

# Creating community matrix
# For the labdsv function:
# the first column is the sample ID / year
# the second column is the taxon ID
# and the third sample is the abundance of that taxon in that sample
mzb <- labdsv::matrify(mzb[, c(7, 2, 4)])

# Adding the sample ID
mzb <- data.frame(SampleID = rownames(mzb), mzb)

# Adding all sample informations
mzb <- merge(x = infos, y = mzb, by = 'SampleID', all.x = F, all.Y = T)

# Saving file
write.csv(x = mzb, file = '../data/mzb_abundances_wide.csv')

for(i in 1:10) gc(reset = T) ; rm(list = ls())

# PHYTOPLANKTON (PPKT) DATA
# For now, at least, not integrating NL data
# No coordinates available...

# Importing time series
DE_PPKT <- unique(read.csv(
  '/Users/lucie/Documents/Work/Data/Data_Oldenburg/Wadden Sea/PPKT/D_PPKT_abund_biov_carbon_updated_1.csv'))[, c(1, 2, 10, 13)]
# NL_PPKT <- unique(read.csv(
#   '/Users/lucie/Documents/Work/Data/Data_Oldenburg/Wadden Sea/PPKT/NL_PPKT_abund_biov_carbon_updated_1.csv'))[, c(1, 2, 9, 12)]

# Cheking species names
DE_PPKT <- DE_PPKT[-grep(DE_PPKT$Identified_taxon, pattern = 'indet.'), ]
DE_PPKT <- DE_PPKT[-grep(DE_PPKT$Identified_taxon, pattern = 'Unidentified'), ]

# NL_PPKT$Identified_taxon <- gsub(pattern = 'Amphora [1]', replacement = 'Amphora', x = NL_PPKT$Identified_taxon)
# NL_PPKT$Identified_taxon <- gsub(pattern = 'Amphidoma acuminata [3]', replacement = 'Amphidoma acuminata', x = NL_PPKT$Identified_taxon)

# Uniformizing colnames
colnames(DE_PPKT) <- 
  # colnames(NL_PPKT) <- 
  c('StationID', 'Year', 'Taxa', 'Concentration')

# Getting the year instead of the full date
DE_PPKT$Year <- substr(DE_PPKT$Year, 7, 10)
# NL_PPKT$Year <- substr(NL_PPKT$Year, 7, 10)

# Saving file
write.csv(x = DE_PPKT, file = '../data/ppkt.csv')

# Converting into wide format using the labdsv library
# Creating a sample ID based on the station and the year
DE_PPKT$SampleID <- paste(DE_PPKT$StationID, DE_PPKT$Year, sep = '_')

# Importing the coordinates of the sites
coords <- na.omit(read.csv('/Users/lucie/Documents/Work/Data/Data_Oldenburg/Wadden Sea/PPKT/ppkt.coords.csv'))[, -1]

# Saving sample infos (StationID, year, lat and long, SampleID)
infos <- unique(DE_PPKT[, c(1:2, 5)])
infos <- merge(infos, coords, all.x = T, all.y = F)

# Creating community matrix
# For the labdsv function:
# the first column is the sample ID / year
# the second column is the taxon ID
# and the third sample is the abundance of that taxon in that sample
DE_PPKT <- labdsv::matrify(DE_PPKT[, c(5, 3, 4)])

# Adding the sample ID
DE_PPKT <- data.frame(SampleID = rownames(DE_PPKT), DE_PPKT)

# Adding all sample informations
DE_PPKT <- merge(x = infos, y = DE_PPKT, by = 'SampleID', all.x = F, all.Y = T)

# Saving file
write.csv(x = DE_PPKT, file = '../data/ppkt_wide.csv')















































