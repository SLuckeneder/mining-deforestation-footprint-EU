
library(dplyr)
library(ggplot2)

# prepare cluster data ---------------------------------------------------------

data_cluster_forest_loss <- read.csv("data/parsed/cluster_forest_loss.csv") # tree cover loss

# remove clusters where no forest loss occurred and subset to study time coverage
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  dplyr::rename(total_ha = area_forest_loss_ALL,
                plantation_ha = area_forest_loss_PLANTED_ALL) %>%
  dplyr::filter(!is.na(total_ha) & total_ha > 0, year %in% c(2001:2019))

orig_forest_loss <- sum(data_cluster_forest_loss$total_ha)

# concordance between clusters and commodities
conc_cluster_commodity_list <- read.csv2("data/parsed/cluster_commodity_list.csv", sep = ",") %>%
  dplyr::select(id_cluster, materials_list)

# add commodities to cluster data
data_cluster_forest_loss <- data_cluster_forest_loss %>% dplyr::left_join(conc_cluster_commodity_list)

# bring commodities list into long format (multiple commodities per cluster possible, this is handled below)
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  tidyr::separate_rows(materials_list, sep = ",") %>%
  dplyr::rename(commodity_list = materials_list)

# summarize into "Other non-ferrous ores n.e.c."
remove_commodities <- c("Lithium", "Lanthanides", "Tantalum", "Heavy Mineral Sands", "Ilmenite",           
                        "Rutile", "Titanium", "Zircon", "Antimony", "Tungsten", "Vanadium",           
                        "Niobium", "Graphite", "Rhodium", "Bismuth", "Chromite", "Magnesium",   
                        "Rare Earth Element", "Platinum Group Element",
                        "Ferrochrome", "Tellurium", "Iridium", "Ruthenium", "Sodium Bicarbonate",
                        "Diamonds", "Manganese", "Platinum", "Palladium",
                        "Phosphate", "Potash", "Molybdenum", "Cobalt",
                        "Ammonium Sulfate", "Coke", "Crude Oil", "Ferroniobium", "Pyrite", "Sodium Tripolyphosphate", "Sulphuric Acid", "Tar",
                        "Rare Earth Elements", "Neodymium", "Praseodymium", "Rhenium", "Thorium", "Cerium", "Lanthanum", "Potassium Oxide", 
                        "Caesium", "Magnetite",  "Europium", "Scandium", "Borates", "Potassium Chloride", "Spodumene",  "Arsenic", "Chromium", 
                        "Leucoxene", "Emerald", "Indium", "Yttrium", "Ytterbium", "Platinum Group Metals", "Gallium", "Germanium", "Dysprosium", 
                        "Terbium", "Beryllium", "Promethium", "Osmium", "Selenium", "Barite", "Hafnium", "Jade", "Cadmium", "Beryl", "Rubidium",
                        "Limestone", "Frac Sand", "Samarium", "Gadolinium", "Holmium", "Erbium", "Thulium", "Lutetium", "Asbestos", "Samarium", 
                        "Gadolinium", "Vermiculite", "Kaolin", "Garnet", "Marble", "Ferromanganese", "Ferrovanadium", "Iron Sand",
                        "Ferronickel", "Ferrotungsten", "Aggregates", "Gypsum", "Mercury", "Alumina", "Silica", "Strontium",
                        "Calcium Carbonate", "Boron", "Zinc-Lead", "Sapphire", "Potassium Sulfate", "Topaz", "Hematite", "Ruby",
                        "Dolomite", "Nepheline", "Potassium")
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  dplyr::mutate(commodity_list = ifelse(commodity_list %in% remove_commodities, "Other non-ferrous ores n.e.c.", commodity_list))

# count number of commodities reported and allocate equally by commodity
temp <- data_cluster_forest_loss %>% 
  dplyr::group_by(id_cluster, commodity_list) %>% 
  dplyr::slice(1) %>%
  dplyr::group_by(id_cluster) %>%
  mutate(count = n()) %>%
  dplyr::mutate(commodity_list = ifelse(is.na(commodity_list) & count == 1, "unknown", commodity_list)) %>%
  dplyr::filter(!is.na(commodity_list)) %>%
  mutate(count = n()) %>%
  dplyr::select(id_cluster, commodity_list, count) 
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  dplyr::select(-commodity_list) %>%
  dplyr::group_by(id_cluster, year) %>%
  dplyr::slice(1) %>%
  dplyr::left_join(temp) %>%
  dplyr::mutate(total_ha = total_ha/ count) %>%
  dplyr::select(-count)
sum(data_cluster_forest_loss$total_ha) == orig_forest_loss

# tidy and plot
data_cluster <- data_cluster_forest_loss %>%
  dplyr::rename(commodity = commodity_list) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Lead", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Zinc", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Silver", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::group_by(id_cluster, commodity, year) %>%
  dplyr::summarise(total_ha = sum(total_ha)) %>%
  dplyr::ungroup()
sum(data_cluster$total_ha) == orig_forest_loss


p <- data_cluster %>% 
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(total_ha = sum(total_ha)) %>%
  ggplot2::ggplot(aes(x = year, y = total_ha, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity", position = "stack")
ggplot2::ggsave("forest_loss_equal.png", 
                plot = p, device = "png", 
                path = paste0("./figures/checks"),
                scale = 1, width = 350, height = 200, units = "mm")

# write final data
cluster_forest_loss_equal_allocation <- data_cluster %>% 
  dplyr::select(id_cluster, year, commodity, total_ha)
write.csv(cluster_forest_loss_equal_allocation, file = "data/forest_loss/cluster_forest_loss_equal_allocation.csv", row.names = FALSE)


# create extension --------------------------------------------------------

# concordance cluster-countries
conc <- read.csv2("data/forest_loss/extensions_tbl.csv", sep = ",") %>%
  dplyr::select(id_cluster, country_isoa3) %>%
  dplyr::group_by(id_cluster) %>% dplyr::slice(1)

# aggregate GLORIA world regions XAM (Rest of Americas), XEU (Rest of Europe), XAF (Rest of Africa), XAS (Rest of Asia-Pacific)
XAM <- c("ABW", "GUY", "SUR")
XAF <- c("ESH", "GNB", "LSO", "SWZ")
XAS <- c("FJI", "NCL", "SLB")
XEU <- c("GRL", "MNE", "SJM")
conc <- conc %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 %in% XAM, "XAM", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 %in% XAF, "XAF", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 %in% XAS, "XAS", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 %in% XEU, "XEU", country_isoa3))

# merge
cluster_forest_loss_equal_allocation <- cluster_forest_loss_equal_allocation %>% dplyr::left_join(conc)

# data to extension format ------------------------------------------------

# i.e.: per commodity, country and year
E <- cluster_forest_loss_equal_allocation %>%
  dplyr::group_by(year, commodity, country_isoa3) %>%
  dplyr::summarise(forest_loss_ha = sum(total_ha))

# export
write.csv(E, file = "data/forest_loss/extension_forest_loss_equal_allocation.csv", row.names = FALSE)


# visualise extension -----------------------------------------------------

# total
E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_ha = sum(forest_loss_ha)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_ha / 1000, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity") +
  # ggplot2::scale_y_continuous(limits = c(0, 1300), expand = c(0, 0)) +
  ggplot2::scale_x_continuous(limits = c(2000.5, 2019.5), breaks = seq(2001, 2019, 1), expand = c(0, 0)) +
  ggplot2::labs(title = "Forest loss induced by mining, equal allocation, 2001-2019", x = NULL, y = "Forest loss (thousand ha)") +
  ggplot2::scale_fill_manual(name = NULL, values =  c(RColorBrewer::brewer.pal(n = 10, name = "Paired"), "grey")) + 
  ggplot2::theme_bw() +
  ggplot2::theme(title = element_text(size = 16),
                 axis.text.x = element_text(size = 14, angle = 45, hjust=1),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank())

# by commodity
E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_ha = sum(forest_loss_ha)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_ha / 1000)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(.~commodity, scales = "free_y") +
  ggplot2::labs(title = "Forest loss induced by mining, equal allocation, 2001-2019", x = "Year", y = "Forest loss (thousand ha)")

