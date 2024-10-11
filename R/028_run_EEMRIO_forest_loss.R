library(data.table)
library(dplyr)
library(Matrix)

years <- c(2001:2019) # if country_aggregates == FALSE, run only for single years!
allocation <- "price" # run for "price", "mass", "equal" and "primary"

country_aggregates <- TRUE # FALSE only necessary for "price" allocation
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"
path_parsed <- "data/parsed/"
path_results <- "data/results/"
path_intermediate_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/intermediate/forest_loss"
if(!dir.exists(path_intermediate_nfs)){dir.create(path_intermediate_nfs, recursive = T)}
labels <- read.csv(file.path(path_parsed_nfs, "labels.csv")) 

# store setup
if(country_aggregates){
  lab <- unique(paste0(labels$Region_acronyms, labels$type))
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
      dplyr::mutate(reg_sec = paste0(Region_acronyms, "_", sprintf("%03d", Lfd_Nr), type))
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
    
    load(file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))
    load(file.path(path_parsed_nfs, paste0("Y_", year, ".RData")))
    load(file.path(path_parsed_nfs, paste0("x_", year, ".RData")))
    load(file.path(path_parsed_nfs, paste0("L_inv_", year, ".RData")))
    load(file.path(path_parsed, paste0("e_forest_loss_", allocation, "_", year, ".RData")))

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
    load(file.path(path_parsed_nfs, paste0("L_inv_", year, ".RData")))
    load(file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))
    dnamesx <- dimnames(Z)[[1]]
    dnamesy <- dimnames(Z)[[2]]
    rm(Z)
    load(file.path(path_parsed_nfs, paste0("Y_", year, ".RData")))
    Y_tot <- rowSums(Y); rm(Y)
  }
  
  if(country_aggregates){
  if(! file.exists(file.path(path_intermediate_nfs, paste0("FP_forest_loss_agg_", allocation, "_", year, ".RData")))){
      
      # step 3 & 4 (in one step): 
      # step 3: calculate MP (total intensity vector (Kitzes notation  F), i.e. Leontief * Intensity per category q
      # step 4: calculate FP (total upstream emissions (Kitzes notation  E), i.e. MP * Y per category q and country
      FP <- (L_inv * as.numeric(Q)) %*% diag(Y_tot); gc()
      dimnames(FP) <- list(paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)), 
                           paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)))
      
      # # step 5: aggregate from industries to country level
      
        agg <- function(x) {
          x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
          return(x)
        }
        dimnames(FP)[[1]] <- substr(dnamesx, 1, 5)    
        FP <- agg(t(FP))
        FP <- t(FP)
        dimnames(FP)[[2]] <- substr(dnamesy, 1, 5)
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
    FP <- (L_inv * as.numeric(Q)) %*% diag(Y_tot); gc()
    dimnames(FP) <- list(paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)), 
                         paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)))
    
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
        dplyr::mutate(type = paste0(substr(from_sector, 8, 8), "x", substr(to_sector, 8, 8))) %>%
        dplyr::filter(type == "ixp") %>% dplyr::select(-type) %>%
        dplyr::mutate(from_region = substr(from_sector, 1, 3), to_region = substr(to_sector, 1, 3)) %>%
        dplyr::mutate(from_sector = substr(from_sector, 5, 7), to_sector = substr(to_sector, 5, 7))  
      write.csv(store, file.path(path_results, paste0("sec_",  allocation, "_", year, "_", Sys.time(), ".csv")))
    }
    
    cat(paste(year, "done.")); gc()
  
}

if(country_aggregates){
  store <- store %>% dplyr::mutate(type = paste0(substr(from_region, 4, 4), "x", substr(to_region, 4, 4))) %>%
    dplyr::filter(type == "ixp") %>% dplyr::select(-type) %>%
    dplyr::mutate(from_region = substr(from_region, 1, 3), to_region = substr(to_region, 1, 3))
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