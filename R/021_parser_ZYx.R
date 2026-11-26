
library(data.table)
library(dplyr)
library(Matrix)

path_T_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/T"
path_Y_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/Y"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"

if(!dir.exists(path_parsed_nfs)){dir.create(path_parsed_nfs)}

years <- c(2001:2020)

# prepare labels ----------------------------------------------------------
regions <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 1)
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 2)
n_regions <- nrow(regions)
f_regions <- regions %>% slice(rep(1:n(), each = nrow(sectors)*2))
f_sectors <- sectors %>% slice(rep(1:n(), n_regions*2))
f_sectors$type <- rep(c(rep("i", 120),rep("p",120)), n_regions)
labels <- cbind(f_regions[, c(2,3)], f_sectors)
write.csv(labels, file.path(path_parsed_nfs, "labels.csv"), row.names = FALSE)

sectors_FD <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 3)
f_regions_FD <- regions %>% slice(rep(1:n(), each = 6))
labels_FD <- cbind(f_regions_FD, sectors_FD)
write.csv(labels_FD, file.path(path_parsed_nfs, "labels_fd.csv"), row.names = FALSE)

for(year in years){
  
  print(year)
  
  # loda data ---------------------------------------------------------------
  
  # intermediate production (Z)
  if(! file.exists(file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))){
    # NOTES
    # Files: YYYYMMDD_120secMother_AllCountries_002_VAR-Results_YYYY_057_Markup00M(full).csv,  
    # with  VAR  =  {T,Y,V}  being  supply-use  transactions  (T),  final demand (Y) and value added (v), 
    # and M = 1,...,5 being valuations (basic prices, trade  margins,  transport  margins,  taxes  on  products,  subsidies  on  products).  
    
    # 120 sectors * 2 * 164 countries
    file_Z <- list.files(path= path_T_nfs, pattern = paste0("_", year, "_"), full.names = T)
    Z <- data.table::fread(file_Z)
    Z <- Matrix::Matrix(data.matrix(Z), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr))))
    
    save(Z, file = file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))
  } else {
    load(file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))
  }
  # # Checks: inspect AT
  # Z_at <- Z[labels$Region_acronyms == "AUT", labels$Region_acronyms == "AUT"]
  # Z_at <- as.matrix(Z_at)
  # 
  # Z_at_df <- matrix(Z_at, dimnames=list(t(outer(colnames(Z_at), rownames(Z_at), FUN=paste)), NULL)) %>%
  #   as.data.frame() %>%
  #   cbind(expand.grid(X=colnames(Z_at), Y=rownames(Z_at) ))
  # 
  # Z_at_df$Y <- factor(Z_at_df$Y, levels = rev(rownames(Z_at)))
  # Z_at_df$X <- factor(Z_at_df$X, levels = colnames(Z_at))
  # p <- ggplot(data.frame(Z_at_df), aes(X, Y, fill= V1)) +
  #   geom_tile() +
  #   scale_fill_viridis() +
  #   labs(title = paste0("Z (AT, ", year, ")"), x = NULL, y = NULL, fill = NULL) +
  #   theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
  #         axis.text.y = element_text(size = 4))
  # ggplot2::ggsave(paste0("Z_at_", year, ".png"),
  #                 plot = p, device = "png",
  #                 path = paste0("./figures/checks"),
  #                 scale = 1, width = 400, height = 380, units = "mm")
  
  # final demand (Y)
  if(! file.exists(file.path(path_parsed_nfs, paste0("Y_", year, ".RData")))){
    # 164 countries * 6 final demand categories
    file_Y <- list.files(path= path_Y_nfs, pattern = paste0("_", year, "_"), full.names = T)
    Y <- data.table::fread(file_Y)
    Y <- Matrix::Matrix(data.matrix(Y), sparse = TRUE,
                        dimnames = list(paste0(labels$Region_acronyms,"_", labels$type, sprintf("%03d",labels$Lfd_Nr)), 
                                        paste0(rep(regions$Region_acronyms, each=6),"_",rep(1:6, n_regions))))
    
    # mirroring (see Lenzen et al 2022 Nat Sust SI S. 28)
    # i.e. 1) set negative values in Y to 0 and 2) add these differences in positive values to value added. 
    # Since we dont use value added in our IO model, we only need to do 1)
    Y[Y < 0] <- 0
    
    # # Checks: inspect AT
    # Y_at <- Y[labels$Region_acronyms == "AUT", labels_FD$Region_acronyms == "AUT"]
    # Y_at <- as.data.frame(as.matrix(Y_at)) %>% dplyr::add_rownames(var = "sector")
    # Y_at_df <- tidyr::gather(Y_at, X, Y, -sector)
    # Y_at_df$sector <- factor(Y_at_df$sector, levels = rev(Y_at$sector))
    # Y_at_df$X <- factor(Y_at_df$X, levels = colnames(Y_at)[-1])
    # p <- ggplot(data.frame(Y_at_df), aes(X, sector, fill= Y)) +
    #   geom_tile() +
    #   scale_fill_viridis() +
    #   labs(title = paste0("Y (AT, ", year, ")"), x = NULL, y = NULL, fill = NULL) +
    #   theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
    #         axis.text.y = element_text(size = 4))
    # ggplot2::ggsave(paste0("Y_at_", year, ".png"),
    #                 plot = p, device = "png",
    #                 path = paste0("./figures/checks"),
    #                 scale = 1, width = 200, height = 380, units = "mm")
    
    save(Y, file = file.path(path_parsed_nfs, paste0("Y_", year, ".RData")))
  } else {
    load(file.path(path_parsed_nfs, paste0("Y_", year, ".RData")))
  }
  
  # total output (x) 
  if(! file.exists(file.path(path_parsed_nfs, paste0("x_", year, ".RData")))){
    x <- Matrix::rowSums(Z) + Matrix::rowSums(Y)
    save(x, file = file.path(path_parsed_nfs, paste0("x_", year, ".RData")))
  } else {
    load(file.path(path_parsed_nfs, paste0("x_", year, ".RData")))
  }
  
}



