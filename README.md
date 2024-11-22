# Tropicalization and Borealization in the Wadden Sea

## Rationale

Ecological communities are experiencing shift in species communities in which some species are going extinct ('**losers**') while other species are colonizing the community ('**winners**'). These reshufflings are usual not random in regards to species thermal niche. In particular, cold-adapted species tend to be loosers while warm-adapted species tend to be winners (e.g. Devictor et al., 2008; Duque et al., 2015; Li et al., 2016; Tayleur et al., 2016; Santangeli & Lehikoinen, 2017) resulting in the **tropicalization** of a community (i.e., on average more warm-dwelling species relate to cold-dwelling species). However, the tropicalization can be decomposed into two non-mutually exclusive processes: tropicalization *per se* (i.e., increase in abundance of warm-dwelling species) and **deborealization** (i.e., decrease in abundance of cold-dwelling species). On the other hand, a community can experience an increase in the relative abundance/biomass of cold-adapted species. Similarly, this can be attributed to two non mutually exclusive processes: **borealization** (i.e., increase in cold-adapted species) and **detropicalization** (i.e., decrease in warm-adapted species). On way to infer these processes is to compute the **Community Temperature Index** (CTI) which is the mean of **Species Temperature Index** (STI) (i.e., a measure of their thermal preferences) weighted by the relative abundance/biomass and to investigate its variation. In particular, one can relate the STIs to population trends as in Figure 1, from McLean et al. 2021.

![](../figures_ReadMe/FourProc.png)

### How to compute Species Temperature Index?

Species Temperature Index is usually computed as the mean temperature experienced across the geographical distribution. By doing so, rather than trying to identify the optimum temperature (i.e., temperature at which the maximal fitness/abundance is observed) allows to consider the full range of temperature at which a species can survive (even suboptimal temperatures). The STI can be infered for the entire species distribution or only a subset. In the latter case, it will reflect the population thermal niche which consider the intraspecific variation in thermal preferences (e.g., due to genetic variation across populations or phenotypic plasticity). **One (unresolved) question is which temperature one should use? Given the fact we usually use time series for ecological data that we coupled with time series in temperatures, we have information on temporal dynamics in temperature. Should we use all ever experienced temperature or rather 'historical' temperatures (e.g., first year, first five years, ...)?**

! FIGURE A LA MAIN sur Fqcy x TEMP

#### Seasonal variation in temperature rather than yearly

In the Wadden Sea, it is unlikely that will have string spatial patterns in climate change, which will likely results in no spatial patterns in tropicalization/borealization. However, there is a strong seasonality which is likely to have an impact on the seasonal patterns. One way to highlight these is to compute seasonal STI based on the temperature experienced only during the winter versus the temperature experienced only during the summer. Thus, we will be able to identify species having a strong seasonality in their abundances (i.e., phenology) versus species that are rather generalist (i.e., no strong phenology).

-   Seasonal species: STI~winter~ \< Temperaure~mean~ $\approx$ STI~global~ \< STI~summer~
-   Non seasonal generalist species: STI~winter~ $\approx$ Temperaure~mean~ \approx STI~global~ $\approx$ STI~summer~
-   Non seasonal cold-adapted species: STI~winter~ $\approx$ STI~global~ $\approx$ STI~summer~ \< Temperaure~mean~
-   Non seasonal warm-adapted species: Temperaure~mean~ \< STI~winter~ $\approx$ STI~global~ $\approx$ STI~summer~

! FIGURE A LA MAIN sur STI seasonal

### Community level patterns

#### Global scale

If we are ignoring the seasonality of species, we can simply reproduce the figure by McLean et al. 2021. One can also compute the strength of each process as follow:

! ADD FORMULA AND EXPLAINATION

#### Incorporating species phenology

Changes in CTI can be due to non seasonal species (Cf. **Global scale** section) but also resulting from variation in seasonal species abundances. Thus an increase in CTI (i.e., increase in the relative abundance of warm-dwelling species) can result in the increase in a seasonal species during summer (i.e. **summerization**).

![](../figures_ReadMe/DecisionTree.png)

## Script structure
