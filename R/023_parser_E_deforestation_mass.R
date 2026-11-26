
library(data.table)
library(dplyr)
library(Matrix)

years <- c(2001:2019)

path_E <- "data/forest_loss/"
file_E <- "extension_forest_loss_mass_allocation.csv"
path_parsed <- "data/parsed"

path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
labels <- read.csv(file.path(path_parsed_nfs, "labels.csv"))
satellites <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 6)

# Divide coal into "Hard coal" and "Lignite and peat" according to the country's main coal sector
lignite <- c("ALB", "ARM", "AUT", "BLR", "BIH", "BGR", "BDI", "CZE", "CSK", "DNK", "EST", "FIN", "GEO", "DEU", "GRC", "HUN", 
             "IRL", "ITA", "KSV", "KGZ", "LAO", "LVA", "LTU", "MNE", "MKD", "ROU", "RWA", "SRB", "YUG", "SVK", "SVN", "SWE", 
             "THA", "TUR", "UZB")

for(yr in years){
  
  print(year)
  
  # Read deforestation extension and arrange in the same way as GLORIA satellites
  if(! file.exists(file.path(path_parsed, "extensions", paste0("e_forest_loss_mass_", yr, ".RData")))){
    
    
    # create extension structure using original GLORIA labels
    labels <- labels %>% dplyr::select(Region_acronyms, Sector_names, type, Lfd_Nr) %>%
      dplyr::mutate(unid = paste(Sector_names, Region_acronyms))
    
    # read forest loss
    file_e <- file.path(path_E, file_E)
    e <- read.csv(file_e)
    e <- e %>% dplyr::filter(year == yr) %>%
      dplyr::filter(commodity != "unknown") %>%
      dplyr::mutate(commodity = paste(commodity, "ores")) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Lead/Zinc/Silver ores", "Lead/zinc/silver ores", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Iron Ore ores", "Iron ores", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Coal ores" & country_isoa3 %in% lignite, "Lignite and peat", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Coal ores" & ! country_isoa3 %in% lignite, "Hard coal", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Bauxite ores", "Aluminium ore", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "Other non-ferrous ores n.e.c. ores", "Other non-ferrous ores", commodity)) %>%
      dplyr::mutate(commodity = ifelse(commodity == "U3O8 ores", "Uranium ores", commodity))
    
    e <- e %>% dplyr::mutate(unid = paste(commodity, country_isoa3))
    
    # join forest loss and labels for extension structure and extract as vector
    e <- labels %>% 
      dplyr::left_join(e) %>%
      dplyr::mutate(forest_loss_km2 = ifelse(is.na(forest_loss_km2), 0, forest_loss_km2)) %>%
      dplyr::mutate(forest_loss_km2 = ifelse(type == "i", forest_loss_km2, 0)) %>%
      dplyr::select(forest_loss_km2) 
    
    e <- Matrix::Matrix(data.matrix(e), 
                        dimnames = list(paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)), "forest_loss_km2"))
    
    save(e, file = file.path(path_parsed, paste0("e_forest_loss_mass_", yr, ".RData")))
    
  } 
}
