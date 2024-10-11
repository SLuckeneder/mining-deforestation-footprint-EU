

# create folder structure
if (!dir.exists("data/parsed")){dir.create("data/parsed")}
if (!dir.exists("data/results")){dir.create("data/results")}
if (!dir.exists("figures")){dir.create("figures")}
if (!dir.exists("figures/checks")){dir.create("figures/checks")}
if (!dir.exists("figures/presentations")){dir.create("figures/presentations")}


# 1 Deforestation extension -------------------------------------------------

# Compute annual deforestation within mining polygons
# --> this is done in Maus et al. 2024 (in preparation), hence just read and tidy the data
source("R/011_setup_forest_loss.R")

# 4 approaches to allocate deforestation to 9 metal extraction sectors (for the analysis, we use price allocation)
source("R/012_forest_loss_mass_allocation.R")
source("R/013_forest_loss_price_allocation.R")
source("R/014_forest_loss_primary_allocation.R")
source("R/015_forest_loss_equal_allocation.R")

# 2 run GLORIA --------------------------------------------------------------

# Storing and processing the data requires substantial memory and storage capacity!

# download and store GLORIA data 
source("R/020_download_GLORIA.R")

# Parser for MRIO ingredients (Z, y, x, L’, e)
source("R/021_parser_ZYx.R")
source("R/022_parser_L.R")

source("R/023_parser_E_deforestation_mass.R")
source("R/024_parser_E_deforestation_price.R")
source("R/025_parser_E_deforestation_equal.R")
source("R/026_parser_E_deforestation_primary.R")

# Parser domestic extraction (taken from GLORIA material extension)
source("R/027_parser_domestic_extraction.R")

# Run deforestation footprints (using 4 different allocation methods, set manually in the code and re-run the model)
source("R/028_run_EEMRIO_forest_loss.R")


# 3 Visualisations ----------------------------------------------------------

### Figure 1 Forest loss global map + zoom-ins
# requires forest loss data processed in step 1 Deforestation extension
source("R/031_Figure-1.R")

### Figure 2: Global metal production (A), forest loss by commodity (B), direct intensities (C) 
# requires GLORIA domestic extraction data processed in 027_parser_domestic_extraction.R
# requires forest loss data processed in step 1 Deforestation extension
source("R/032_Figure-2.R")

### Figure 3: Sankey diagram with EU as consumption region (A), EU forest loss FP by country of impact (B), EU forest loss FP sectoral composition (C)
# requires model results computed in step 2 run GLORIA 
# If the user skipped running the models, it is possible to continue with the ready-to-use results datasets in the data folder.
# adjust to your output file or use the file in comment 
results_agg_price <- "results/agg_price_2001-2019_2024-10-08 14:49:30.353889.csv" # "agg_price_results_hc.csv" 
results_agg_mass <- "results/agg_mass_2001-2019_2024-10-08 18:06:25.887588.csv" # "agg_mass_results_hc.csv" 
results_agg_equal <- "results/agg_equal_2001-2019_2024-10-09 11:54:50.346649.csv" # "agg_equal_results_hc.csv" 
results_agg_primary <- "results/agg_primary_2001-2019_2024-10-09 16:06:50.373638.csv" # "agg_primary_results_hc.csv" 
# for sector-level results, please run sector-level models or download https://doi.org/10.5281/zenodo.13911608 and store in data/results folder
source("R/033_Figure-3.R")

### Figure 4: 
# requires forest loss data processed in step 1 Deforestation extension
# requires GLORIA domestic extraction data processed in 027_parser_domestic_extraction.R
# requires model results computed in step 2 run GLORIA or pre-processed results files
source("R/034_Figure-4.R")

### Supplementary information
source("R/035_Figure-S5-S8.R")
source("R/036_Figure-S10-S12.R")
source("R/037_Figure-S14-S16.R")
source("R/038_Figure-S17.R")
source("R/039_Table-S1.R")

