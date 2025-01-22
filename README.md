# ReadMe: Tropicalization and Borealization in the Wadden Sea

## Rationale
Ecological communities are experiencing shifts in species composition where some species are going extinct (“losers”) while other species are colonizing (“winners”). These reshufflings are usually not random in relation to species’ thermal niches. In particular, cold-adapted species tend to be losers whereas warm-adapted species tend to be winners (e.g., Devictor et al., 2008; Duque et al., 2015; Li et al., 2016; Tayleur et al., 2016; Santangeli & Lehikoinen, 2017), resulting in the tropicalization of a community (i.e., on average, more warm-dwelling species relative to cold-dwelling species). However, tropicalization can be decomposed into two non-mutually exclusive processes: tropicalization per se (increase in abundance of warm-dwelling species) and deborealization (decrease in abundance of cold-dwelling species). Conversely, a community can experience an increase in cold-adapted species, which can similarly be split into borealization and detropicalization.

One way to infer these processes is to compute the Community Temperature Index (CTI), which is the mean of Species Temperature Indices (STI) (i.e., measures of each species’ thermal preference) weighted by their relative abundance or biomass, and to investigate its variation. One can then relate STIs to population trends as in Figure 1 (from McLean et al. 2021).

![](../../figures_ReadMe/FourProc.png)

## How to Compute Species Temperature Index?
Species Temperature Index is typically computed as the mean temperature experienced across a species’ geographic range. By doing so, we consider the full range of temperatures at which a species can survive rather than trying to pinpoint a single optimum temperature. This can be applied to a species’ entire global range or just a subset of its distribution (e.g., a local population), recognizing that intraspecific variation may exist.

A key consideration is which temperature records to use. With time series data, one might include all “ever experienced” temperatures or just some historical baseline (e.g., the first five years). This choice will reflect slightly different aspects of a species’ thermal environment.

## Seasonal Variation in Temperature
In the Wadden Sea, temperature likely has stronger seasonal variation than spatial variation. Analyzing seasonal SST (e.g., winter vs. summer) might reveal:

-   Seasonal species: STI~winter~ \< Temperaure~mean~ $\approx$ STI~global~ \< STI~summer~
-   Non seasonal generalist species: STI~winter~ $\approx$ Temperaure~mean~ \approx STI~global~ $\approx$ STI~summer~
-   Non seasonal cold-adapted species: STI~winter~ $\approx$ STI~global~ $\approx$ STI~summer~ \< Temperaure~mean~
-   Non seasonal warm-adapted species: Temperaure~mean~ \< STI~winter~ $\approx$ STI~global~ $\approx$ STI~summer~


These patterns can help identify whether shifts are due to true tropicalization/borealization or changes in species phenology (e.g., only in summer).

![](../../figures_ReadMe/DecisionTree.png)

## Scripts Overview
 
We have three main scripts in R, each addressing a different step of the analysis:

1. **Extract Temperature for Community Records**  
   Filename: `extract_temperatures.R`

   Purpose:
   - Reads a community CSV (with columns `year`, `lat`, `long`).
   - Merges with a folder of yearly MODIS SST `.tif` files (in `data_dir`).
   - For each row in the community:
     - Extracts SST from the correct yearly raster if available.
     - Falls back to an overall average raster if not available.
     - Appends two new columns: `temperature` (numeric) and `actual` (logical).
   - Saves a new CSV with these columns appended.

   Example:
   - Input: `mzb_wide.csv`  
   - Output: `mzb_temperatures.csv`


2. **Compute Species Thermal Index (STI) and Occurrences**  
   Filename: `compute_STI.R`

   Purpose:
   - Takes an “updated” community CSV that includes a `temperature` column.
   - Identifies species columns (beyond core metadata like `SampleID`, `StationID`, `year`, `long`, `lat`, `temperature`, `actual`).
   - For each species, computes:
     - STI = mean(`temperature` across all rows where abundance > 0).
     - n_occ = count of occurrences used for that STI.
   - Writes a `"_STI.csv"` file with columns: `species`, `STI`, `n_occ`.

   Example:
   - Input: `mzb_temperatures.csv`  
   - Output: `mzb_STI.csv`


3. **Community Temperature Index (CTI)**  
   Filename: e.g., `CTI_script.R`

   Purpose:
   - Defines functions for:
     - `get_CTI()`: Weighted mean temperature per sample based on species STIs.
     - `plot_trend()`: Mixed-effects modeling of CTI ~ Year + (1|Station) + plot the global relationship
     - `get_processes()`: Classify species as “tropicalising,” “borealising,” etc.
     - `plot_species()`: Scatterplot of trends vs. STI + bar chart of process frequencies.

   Workflow:
   - Input: a wide community data set with species columns and a computed STI file.
   - Output: derived CTI, model summaries, process classifications, and plots.

![](../../figures_ReadMe/Presentation1.jpg)

### How to Use

1. **Extract Temperatures**  
   - Run `extract_temperatures.R` with your chosen `community_file`, `data_dir`, `avg_file`, etc.
   - Produces an updated CSV like `mzb_temperatures.csv`.

2. **Compute STI**  
   - Run `compute_STI.R` on the updated CSV (with `data_name` specifying “mzb” or similar).
   - Produces `mzb_STI.csv`.

3. **Run CTI Analysis**  
   - Use `CTI_script.R` (or similarly named script).
   - `get_CTI()` -> compute each sample’s CTI.
   - `plot_trend()` -> view time trends in CTI via linear mixed models.
   - `get_processes()` -> break down species slopes into tropicalisation vs. borealisation, etc.
   - `plot_species()` -> visualize species’ position in the thermal gradient and process distribution.

Dependencies and Warnings
-------------------------

- **R Packages**: 
  - `terra` for raster extraction.
  - `nlme`, `MuMIn`, `ggeffects` for the mixed-effects modeling.
  - `scales` or `ggplot2` for plotting.
- **File Names**: 
  - The scripts assume certain filenames (e.g., `{data_name}_wide.csv`, `{data_name}_temperatures.csv`). Adjust if yours differ.
- **Metadata Columns**: 
  - Many scripts expect columns named `SampleID`, `StationID`, `year`, `lat`, `long`, `temperature`, `actual`. If yours differ, rename or tweak the code.
- **Environment Clearing**: 
  - Each script begins by clearing the R environment (using `rm(list=ls())`). Comment this out if undesired.

By following these scripts in order, you can:
1. Merge your community data with actual/average SST,
1. Compute each species’ average temperature usage (STI),
1. Compute/plot the community’s CTI over time, and
1. Assess processes like tropicalisation or borealisation in the Wadden Sea.
