

library(data.table)
library(dplyr)
library(Matrix)
library(MASS)

library(ggplot2)
library(viridis)

path_T_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/T"
path_Y_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/Y"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"

years <- c(1995:2009)
labels <- read.csv(file.path(path_parsed_nfs, "labels.csv")) 

for(year in years){
  
  print(year)
  
  # read Z and x
  load(file.path(path_parsed_nfs, paste0("Z_", year, ".RData")))
  load(file.path(path_parsed_nfs, paste0("x_", year, ".RData")))
  
  # technology matrix (A)
  if(! file.exists(file.path(path_parsed_nfs, paste0("A_", year, ".RData")))){
    A <- t(t(as.matrix(Z)) / x)
    
    # adjust A
    A[!is.finite(A)] <- 0
    A[A < 0] <- 0
    
    # # Set diagonal values that are zero to small number -> would be needed to be done for block diags
    # diag(A)[diag(A) == 0] <- 10^-7
    
    A_sparse <- Matrix::Matrix(data.matrix(A), sparse = TRUE)
    save(A_sparse, file = file.path(path_parsed_nfs, paste0("A_", year, ".RData")))
  } else {
    load(file.path(path_parsed_nfs, paste0("A_", year, ".RData"))) 
  }
  
  # # inspect A (AT)
  # A_at <- A_sparse[labels$Region_acronyms == "AUT", labels$Region_acronyms == "AUT"]
  # A_at <- as.matrix(A_at)
  # 
  # A_at_df <- matrix(A_at, dimnames=list(t(outer(colnames(A_at), rownames(A_at), FUN=paste)), NULL)) %>%
  #   as.data.frame() %>%
  #   cbind(expand.grid(X=colnames(A_at), Y=rownames(A_at) ))
  # 
  # A_at_df$Y <- factor(A_at_df$Y, levels = rev(rownames(A_at)))
  # A_at_df$X <- factor(A_at_df$X, levels = colnames(A_at))
  # p <- ggplot(data.frame(A_at_df), aes(X, Y, fill= V1)) +
  #   geom_tile() +
  #   scale_fill_viridis() +
  #   labs(title = paste0("A (AT, ", year, ")"), x = NULL, y = NULL, fill = NULL) +
  #   theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
  #         axis.text.y = element_text(size = 4))
  # ggplot2::ggsave(paste0("A_at_", year, ".png"),
  #                 plot = p, device = "png",
  #                 path = paste0("./figures/checks"),
  #                 scale = 1, width = 400, height = 380, units = "mm")
  
  
  # Leontief inverse (L)
  if(! file.exists(file.path(path_parsed_nfs, paste0("L_inv_", year, ".RData")))){
    L <- .sparseDiagonal(nrow(A_sparse)) - A_sparse
    lu(L) # Computes LU decomposition and stores it in L
    L_inv <- solve(L, tol = .Machine[["double.eps"]])
    # L_inv <- MASS::ginv(as.matrix(L))
    save(L_inv, file = file.path(path_parsed_nfs, paste0("L_inv_", year, ".RData")))
    gc()
  } else {
    load(file.path(path_parsed_nfs, paste0("L_inv_", year, ".RData")))
  }
  
  # # inspect L_inv (AT)
  # L_inv_at <- L_inv[labels$Region_acronyms == "AUT", labels$Region_acronyms == "AUT"]
  # L_inv_at <- as.matrix(L_inv_at)
  # 
  # L_inv_at_df <- matrix(L_inv_at, dimnames=list(t(outer(colnames(L_inv_at), rownames(L_inv_at), FUN=paste)), NULL)) %>%
  #   as.data.frame() %>%
  #   cbind(expand.grid(X=colnames(A_at), Y=rownames(A_at) ))
  # 
  # L_inv_at_df$Y <- factor(L_inv_at_df$Y, levels = rev(rownames(A_at)))
  # L_inv_at_df$X <- factor(L_inv_at_df$X, levels = colnames(A_at))
  # 
  # p <- ggplot(L_inv_at_df, aes(X, Y, fill= V1)) +
  #   geom_tile() +
  #   viridis::scale_fill_viridis() +
  #   labs(title = paste0("L inv (AT, ", year, ")"), x = NULL, y = NULL, fill = NULL) +
  #   theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
  #         axis.text.y = element_text(size = 4))
  # ggplot2::ggsave(paste0("L_inv_at_", year, ".png"),
  #                 plot = p, device = "png",
  #                 path = paste0("./figures/checks"),
  #                 scale = 1, width = 400, height = 380, units = "mm")
  
}

rm(list = ls()); gc()

