# Tracing mining-related tree cover loss to final demand of EU industries

This repository contains the R scripts and datasets used to reproduce the results from the paper *Mining-related tree cover loss embodied in EU consumption*.

## Repository Structure

The repository is structured as follows:

**`R/000_main.R`** – This script orchestrates the analysis pipeline, listing all steps in the process:
1. **Deforestation extension**: Scripts to allocate forest loss within mining areas to respective commodities.
2. **Running the GLORIA EE-MRIO model**: Scripts to conduct the input-output analysis to trace deforestation impacts along the supply chain.
3. **Visualisations**: Scripts to generate the figures included in the manuscript and supplementary information.

### Important Notes
- **Geospatial processing scripts**: Scripts for processing forest cover maps (Hansen et al., 2013) and calculating forest loss within mining areas (Maus et al., 2022) are not included here and will be made available in a separate publication (Maus et al., 2024/2025, in preparation). However, pre-processed datasets are provided in `data/forest_loss`.
  
- **Data restrictions**: Due to copyright limitations, detailed data from the S&P Metals and Mining Database (formerly SNL) cannot be shared. We provide summarised datasets in `data/SNL` to support the analysis without breaching data usage agreements.

- **Memory and storage requirements**: Running the GLORIA MRIO model requires significant memory and storage capacity. For users with limited computational resources, pre-aggregated (country-level) results are available in `data/results`. You can directly use files such as `agg_price_results_hc.csv`. If you wish to run the sector-level analysis, you can download the necessary data from Zenodo at [https://doi.org/10.5281/zenodo.13911608](https://doi.org/10.5281/zenodo.13911608) and store them in the `data/results` directory.


## References

Hansen, M. et al. (2013): *High-resolution Global Maps of 21st-century Forest Cover Change*. Science, 342(6160), pp. 850–853. [DOI](https://doi.org/10.1126/science.1244693).

Lenzen, M. et al. (2017): *The Global MRIO Lab – Charting the World Economy*. Economic Systems Research, 29(2), pp. 158–186. [DOI](https://doi.org/10.1080/09535314.2017.1301887).

Lenzen, M. et al. (2022): *Implementing the Material Footprint to Measure Progress towards Sustainable Development Goals 8 and 12*. Nature Sustainability, 5(2), pp. 157–166. [DOI](https://doi.org/10.1038/s41893-021-00811-6).

Maus, V. et al. (2022): *An Update on Global Mining Land Use*. Scientific Data, 9(1), pp. 1–11. [DOI](https://doi.org/10.1038/s41597-022-01547-4).
