

library(data.table)
library(dplyr)
library(Matrix)

years <- c(1990:2019)

path_E_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-compiled/TQ/"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-intermediate/"
if(!dir.exists(file.path(path_parsed_nfs, "extensions"))){dir.create(file.path(path_parsed_nfs, "extensions"))}

regions <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 1)
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 2)
n_regions <- nrow(regions)
f_regions <- regions %>% slice(rep(1:n(), each = nrow(sectors)))
f_sectors <- sectors %>% slice(rep(1:n(), n_regions))
labels <- cbind(f_regions[, c(2,3)], f_sectors) 

satellites <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 6)

for(year in years){
  
  print(year)
  
  # Read material extension and aggregate to 4 main categories
  if(! file.exists(file.path(path_parsed_nfs, "extensions", paste0("e_material_4cat_", year, ".RData")))){
    
    file_e <- list.files(path= path_E_nfs, pattern = paste0("TQ_", year), full.names = T)
    e <- readRDS(file_e)
    e <- Matrix::Matrix(data.matrix(e),
                        dimnames = list(paste0(satellites$Sat_head_indicator,"_", satellites$Sat_indicator),
                                        paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr))))
    
    mat <- c(rep("Biomass", 328), rep("Metallic minerals", 15), rep("Non-metallic minerals", 14), rep("Fossil fuels", 10))
    e_material_4cat <- t(e[c(1:367),])
    dimnames(e_material_4cat)[[2]] <- mat
    agg <- function(x) {
      x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
      return(x)
    }
    e_material_4cat <- agg(e_material_4cat)
    # sum(e_material_4cat) # ca 100 billion tonnes sounds good
    save(e_material_4cat, file = file.path(path_parsed_nfs, "extensions", paste0("e_material_4cat_", year, ".RData")))
    
  }
  
}







