# ReadMe: Tropicalization and Borealization in the Wadden Sea

🚨 11/02/2025: HAS TO BE UPDATED (analytical sections)

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


