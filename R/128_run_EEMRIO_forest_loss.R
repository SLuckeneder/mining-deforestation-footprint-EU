library(data.table)
library(dplyr)
library(Matrix)

years <- c(2001:2019) # if country_aggregates == FALSE, run only for single years!
allocation <- "primary_plantation" # run for "price", "price_plantation", "mass", "mass_plantation","equal", "equal_plantation", "primary" and "primary_plantation"

country_aggregates <- TRUE # FALSE only necessary for "price" allocation
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-compiled/"
path_parsed <- "data/parsed/"
path_results <- "data/results/"
path_intermediate_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-intermediate/forest_loss"
if(!dir.exists(path_intermediate_nfs)){dir.create(path_intermediate_nfs, recursive = T)}

# prepare labels
regions <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 1)
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 2)
n_regions <- nrow(regions)
f_regions <- regions %>% slice(rep(1:n(), each = nrow(sectors)))
f_sectors <- sectors %>% slice(rep(1:n(), n_regions))
labels <- cbind(f_regions[, c(2,3)], f_sectors) 

# store setup
if(country_aggregates){
  lab <- unique(labels$Region_acronyms)
  reg <- length(lab) # number of regions
  yea <- length(years) # number years
  l <- reg*reg*yea
  
  store <- data.frame("index" = 1:l,
                      "from_region" = lab,
                      "to_region" = rep(lab, each = reg),
                      "year" = rep(years,each = (reg*reg)),
                      "value" = 0, 
                      stringsAsFactors=FALSE)
}

for(year in years){
  
  cat("\n", year, "\n")
  
  # if calculated at sector level, store and save yearly
  if(! country_aggregates){
    lab <- labels %>%
      dplyr::mutate(reg_sec = paste0(Region_acronyms, "_", sprintf("%03d", Lfd_Nr)))
    sec <- nrow(lab) # number of sectors
    l <- nrow(lab)*nrow(lab)
    
    store <- data.frame("index" = 1:l,
                        "from_sector" = lab$reg_sec,
                        "to_sector" = rep(lab$reg_sec, each = nrow(lab)),
                        "year" = rep(year,each = (sec*sec)),
                        "value" = 0, 
                        stringsAsFactors=FALSE)
  }
  
  if(! file.exists(file.path(path_intermediate_nfs, paste0("Q_forest_loss_", allocation, "_", year, ".RData")))){
    
    # load data ---------------------------------------------------------------
    
    # prepare Y, X, Z, e data
    
    Y <- readRDS(file.path(path_parsed_nfs, paste0("Y/Y_", year, ".rds")))
    Z <- readRDS(file.path(path_parsed_nfs, paste0("T/Z_", year, ".rds")))
    L <- readRDS(file.path(path_parsed_nfs, paste0("L/L_", year, ".rds")))
    load(file.path(path_parsed, paste0("e_forest_loss_", allocation, "_", year, ".RData")))
    
    Y <- Matrix::Matrix(data.matrix(Y), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(rep(regions$Region_acronyms, each=6),"_",rep(1:6, n_regions))))
    Z <- Matrix::Matrix(data.matrix(Z), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr))))
    x <- Matrix::rowSums(Z) + Matrix::rowSums(Y)

    
    # calculate footprint -----------------------------------------------------
    
    # # step 1:  final demand matrix (Y) pre-calculation: aggregate consumption types (household, government, etc.) over regions
    Y_tot <- rowSums(Y)
    
    # step 2:  calculate direct intensity matrix Q (columns denote direct intensity vectors by material category)
    # column selection needs to be automatized via concordance table
    Q <- e/x
    Q[is.na(Q)] <- 0
    Q[Q == Inf] <- 0
    
    dnamesx <- dimnames(Z)[[1]]
    dnamesy <- dimnames(Z)[[2]]
    rm(Z, Y, e); gc()
    
    save(Q, file = file.path(path_intermediate_nfs, paste0("Q_forest_loss_", allocation, "_", year, ".RData")))
    
  } else {
    
    load(file.path(path_intermediate_nfs, paste0("Q_forest_loss_", allocation, "_", year, ".RData")))
    Y <- readRDS(file.path(path_parsed_nfs, paste0("Y/Y_", year, ".rds")))
    Z <- readRDS(file.path(path_parsed_nfs, paste0("T/Z_", year, ".rds")))
    L <- readRDS(file.path(path_parsed_nfs, paste0("L/L_", year, ".rds")))
    
    Y <- Matrix::Matrix(data.matrix(Y), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(rep(regions$Region_acronyms, each=6),"_",rep(1:6, n_regions))))
    Z <- Matrix::Matrix(data.matrix(Z), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr))))
    
    dnamesx <- dimnames(Z)[[1]]
    dnamesy <- dimnames(Z)[[2]]
    rm(Z)
    Y_tot <- rowSums(Y); rm(Y)
  }
  
  if(country_aggregates){
    if(! file.exists(file.path(path_intermediate_nfs, paste0("FP_forest_loss_agg_", allocation, "_", year, ".RData")))){
      
      # step 3 & 4 (in one step): 
      # step 3: calculate MP (total intensity vector (Kitzes notation  F), i.e. Leontief * Intensity per category q
      # step 4: calculate FP (total upstream emissions (Kitzes notation  E), i.e. MP * Y per category q and country
      FP <- (L * as.numeric(Q)) %*% diag(Y_tot); gc()
      dimnames(FP) <- list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                           paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)))
      
      # # step 5: aggregate from industries to country level
      
      agg <- function(x) {
        x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
        return(x)
      }
      dimnames(FP)[[1]] <- substr(dnamesx, 1, 3)    
      FP <- agg(t(FP))
      FP <- t(FP)
      dimnames(FP)[[2]] <- substr(dnamesy, 1, 3)
      FP <- agg(FP)
      
      # # check
      # sum(FP); sum(e)
      # View(as.matrix(colSums(FP))) # footprints
      # View(as.matrix(rowSums(FP))) # domestic extraction
      # sum(colSums(FP))
      # sum(rowSums(FP))
      
      save(FP, file = file.path(path_intermediate_nfs, paste0("FP_forest_loss_agg_",  allocation, "_", year, ".RData")))
    }
    
    else {
      load(file.path(path_intermediate_nfs, paste0("FP_forest_loss_agg_",  allocation, "_", year, ".RData")))
    } 
  } else { # end country aggregates
    
    if(! file.exists(file.path(path_intermediate_nfs, paste0("FP_forest_loss_sec_",  allocation, "_", year, ".RData")))){
      
      # step 3 & 4 (in one step): 
      # step 3: calculate MP (total intensity vector (Kitzes notation  F), i.e. Leontief * Intensity per category q
      # step 4: calculate FP (total upstream emissions (Kitzes notation  E), i.e. MP * Y per category q and country
      FP <- (L * as.numeric(Q)) %*% diag(Y_tot); gc()
      dimnames(FP) <- list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                           paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)))
      
      # # check
      # sum(FP); sum(e)
      # View(as.matrix(colSums(FP))) # footprints
      # View(as.matrix(rowSums(FP))) # domestic extraction
      # sum(colSums(FP))
      # sum(rowSums(FP))
      
      save(FP, file = file.path(path_intermediate_nfs, paste0("FP_forest_loss_sec_",  allocation, "_", year, ".RData")))
    }
    
    else {
      
      load(file.path(path_intermediate_nfs, paste0("FP_forest_loss_sec_",  allocation, "_", year, ".RData")))
      
    }
    
    
  }
  
  # step 5: store from-to upstream flows 
  store$value[store$year == year] <- FP
  gc()
  
  if(! country_aggregates){
    store <- store %>% 
      dplyr::filter(value > 0) %>%
      dplyr::mutate(from_region = substr(from_sector, 1, 3), to_region = substr(to_sector, 1, 3)) %>%
      dplyr::mutate(from_sector = substr(from_sector, 5, 7), to_sector = substr(to_sector, 5, 7))  
    write.csv(store, file.path(path_results, paste0("sec_",  allocation, "_", year, "_", Sys.time(), ".csv")))
  }
  
  cat(paste(year, "done.")); gc()
  
}

if(country_aggregates){
  write.csv(store, file.path(path_results, paste0("agg_", allocation, "_", years[1], "-", years[length(years)], "_", Sys.time(), ".csv")))
}

rm(list = ls()); gc()




# # inspect store
# store <- read.csv("data/results/sec_2001-2001_2024-01-31 20:41:10.877254.csv")
# e <- read.csv("data/forest_loss/extension_forest_loss_price_allocation.csv") %>%
#   dplyr::filter(commodity != "unknown")
# sum(store$value)
# sum(e$forest_loss_km2)
# store %>% dplyr::filter(year == 2010) %>% dplyr::summarise(total = sum(value))
# e %>% dplyr::filter(year == 2010) %>% dplyr::summarise(total = sum(forest_loss_km2))
# store %>% dplyr::filter(from_region  == "IDN") %>% dplyr::summarise(total = sum(value))
# e %>% dplyr::filter(country_isoa3  == "IDN") %>% dplyr::summarise(total = sum(forest_loss_km2))
# store %>% dplyr::filter(from_region  == "AUT") %>% dplyr::summarise(total = sum(value))
# e %>% dplyr::filter(country_isoa3  == "AUT") %>% dplyr::summarise(total = sum(forest_loss_km2))