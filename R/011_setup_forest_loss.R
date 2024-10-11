

library(dplyr)
library(ggplot2)

# forest loss within clusters ---------------------------------------------

data_cluster_forest_loss <- read.csv(file = "data/forest_loss/mining_forest_loss_timeseries_cluster.csv", 
                                     colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric" )) %>%
  dplyr::select(1:3) %>%
  dplyr::rename(total = area_000) 

write.csv(data_cluster_forest_loss, file = "data/parsed/cluster_forest_loss.csv", row.names = FALSE)


# SNL production within clusters -------------------------------------------

# The raw SNL data is proprietary, so we saved  the pre-processed dataset in the "SNL" folder directly for further analysis.
# This is the code for prcessing, if the user has access to the SNL data
if(dir.exists("data/SNL_production")) {
  yrs <- c(2000:2019)
  commodities <- dir("data/SNL_production/")[grepl(pattern = "production", dir("data/SNL_production/"))]
  
  # concordance between SNL mines and clusters
  conc_cluster <- read.csv("data/forest_loss/mine_snl_hcluster_concordance.csv", colClasses = c("character"))
  
  data_snl <- list()
  for (com in commodities){
    cat("\nload", com, "...")
    load(paste0("data/SNL_production/", com))
    data_snl[[com]] <- x %>% dplyr::filter(value > 0, year %in% yrs) 
  }
  rm(x)
  data_snl <- do.call("rbind", data_snl)
  rownames(data_snl) <- NULL
  
  # all units to tonne
  unique(data_snl$unit)
  data_snl <- data_snl %>%
    dplyr::mutate(value = ifelse(unit == "lb", value * 453592e-9, value)) %>%
    dplyr::mutate(value = ifelse(unit == "ct", value * 2e-7, value)) %>%
    dplyr::mutate(value = ifelse(unit == "oz", value * 2.835e-5, value)) %>%
    dplyr::mutate(unit = "tonne")
  
  # production by commodity and cluster
  data_cluster_production <- data_snl %>% dplyr::left_join(conc_cluster %>% dplyr::select(snl_id, hcluster_id)) %>% 
    dplyr::group_by(hcluster_id, commodity, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(unit = "tonnes")
  
  # there is no production reported for majority of clusters!
  length(unique(conc_cluster$hcluster_id)) # No. total clusters
  length(unique(data_cluster_production$hcluster_id)) # No. clusters with production
  
  write.csv(data_cluster_production, file = "data/parsed/cluster_production.csv", row.names = FALSE)
  
} else {
  
  data_cluster_production <- read.csv("data/SNL/cluster_production.csv")
  write.csv(data_cluster_production, file = "data/parsed/cluster_production.csv", row.names = FALSE)
  
}


# plots -------------------------------------------------------------------

# forest loss all mining clusters
data_cluster_forest_loss %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(total = sum(total)) %>%
  dplyr::filter(year %in% c(2001:2019)) %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = total)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2021)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1250), expand = c(0, 0)) +
  ggplot2::labs(title = "Total forest loss within mining polygons, 2001-2020", x = "Year", y = "Forest loss (square km)") +
  ggplot2::theme(axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1))

# annual production by cluster and commodity
data_cluster_production %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = value / 1000, group = hcluster_id)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(.~commodity, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2020)) +
  ggplot2::labs(title = "Production by cluster, 2000-2019", x = "Year", y = "Production (kilotonnes)") +
  ggplot2::theme(axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 legend.position = "bottom")


# annual production by commodity (aggregate)
data_cluster_production %>% 
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(value = sum(value)) %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = value / 1000)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(.~commodity, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2020)) +
  ggplot2::labs(title = "Production aggregate, 2000-2019", x = "Year", y = "Production (kilotonnes)") +
  ggplot2::theme(axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1))


# annual number of clusters reporting production
data_cluster_production %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(ncluster = n()) %>%
  ggplot2::ggplot(aes(x = as.numeric(year), y = ncluster)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(.~commodity, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2020)) +
  ggplot2::labs(title = "Number of clusters", x = "Year", y = "Cluster count") +
  ggplot2::theme(axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1))


