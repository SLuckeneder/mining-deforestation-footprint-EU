
library(dplyr)
library(ggplot2)

# prepare cluster data ---------------------------------------------------------

data_cluster_forest_loss <- read.csv("data/parsed/cluster_forest_loss.csv") # deforestation

# remove clusters where no forest loss occurred and subset to study time coverage
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  dplyr::rename(def_total = total) %>%
  dplyr::filter(!is.na(def_total) & def_total > 0, year %in% c(2001:2019))


# merge commodities list --------------------------------------------------

# create cluster list of commodities, summarise all materials excluded in GLORIA into "Other non-ferrous ores n.e.c."
remove_commodities <- c("Lithium", "Lanthanides", "Tantalum", "Heavy Mineral Sands", "Ilmenite",           
                        "Rutile", "Titanium", "Zircon", "Antimony", "Tungsten", "Vanadium",           
                        "Niobium", "Graphite", "Rhodium", "Bismuth", "Chromite", "Magnesium",          
                        "Ferrochrome", "Tellurium", "Iridium", "Ruthenium", "Sodium Bicarbonate",
                        "Diamonds", "Manganese", "Platinum", "Palladium",
                        "Phosphate", "Potash", "Molybdenum", "Cobalt",
                        "Rare Earth Elements", "Neodymium", "Praseodymium", "Rhenium", "Thorium", "Cerium", "Lanthanum", "Potassium Oxide", 
                        "Caesium", "Magnetite",  "Europium", "Scandium", "Borates", "Potassium Chloride", "Spodumene",  "Arsenic", "Chromium", 
                        "Leucoxene", "Emerald", "Indium", "Yttrium", "Ytterbium", "Platinum Group Metals", "Gallium", "Germanium", "Dysprosium", 
                        "Terbium", "Beryllium", "Promethium", "Osmium", "Selenium", "Barite", "Hafnium", "Jade", "Cadmium", "Beryl", "Rubidium",
                        "Limestone", "Frac Sand", "Samarium", "Gadolinium", "Holmium", "Erbium", "Thulium", "Lutetium", "Asbestos", "Samarium", 
                        "Gadolinium", "Vermiculite", "Kaolin", "Garnet", "Marble", "Ferromanganese", "Ferrovanadium", "Iron Sand",
                        "Ferronickel", "Ferrotungsten", "Aggregates", "Gypsum", "Mercury", "Alumina", "Silica", "Strontium",
                        "Calcium Carbonate", "Boron", "Zinc-Lead", "Sapphire", "Potassium Sulfate", "Topaz", "Hematite", "Ruby")
commodities_list <- read.csv("data/forest_loss/mine_hcluster_commodities.csv", colClasses = c("character")) %>%
  dplyr::select(hcluster_id, commodities_list) %>%
  dplyr::mutate(commodities_list = gsub(pattern = paste(remove_commodities, collapse = "|"), "", commodities_list)) %>%
  dplyr::filter(! commodities_list %in% c(";", ";;", ";;;", ";;;;", ";;;;;", ";;;;;;", ";;;;;;;", ";;;;;;;;", ";;;;;;;;;", ";;;;;;;;;")) %>%
  dplyr::mutate(commodities_list = gsub(pattern = ";|;;|;;;|;;;;|;;;;;|;;;;;;|;;;;;;;|;;;;;;;;|;;;;;;;;;|;;;;;;;;;;", ", ", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = ",,", ",", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "IronOre", "Iron Ore", commodities_list)) %>%
  dplyr::mutate(commodities_list = ifelse(commodities_list == "", "Other non-ferrous ores n.e.c.", commodities_list))

# manually fill NAs in commodities_list for clusters with highest deforestation rates
manual_commodities <- read.csv(file = "data/forest_loss/manual_corrections.csv", colClasses = c("character"))  %>%
  dplyr::mutate(commodities_list = ifelse(commodities_list == "Bauxite;Aluminium", "Bauxite", commodities_list)) %>%
  dplyr::mutate(manual_n_commodity = stringr::str_count(commodities_list, ";") + 1) %>%
  dplyr::mutate(hcluster_id = gsub("\\n", "", hcluster_id)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = paste(remove_commodities, collapse = "|"), "", commodities_list)) %>%
  dplyr::filter(! commodities_list %in% c(";", ";;", ";;;", ";;;;", ";;;;;", ";;;;;;", ";;;;;;;", ";;;;;;;;", ";;;;;;;;;", ";;;;;;;;;")) %>%
  dplyr::mutate(commodities_list = gsub(pattern = ";|;;|;;;|;;;;|;;;;;|;;;;;;|;;;;;;;|;;;;;;;;|;;;;;;;;;|;;;;;;;;;;", ", ", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "^,*| |^  *|,*$", "", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = ",,", ",", commodities_list)) %>%
  dplyr::mutate(commodities_list = gsub(pattern = "IronOre", "Iron Ore", commodities_list)) %>%
  dplyr::mutate(commodities_list = ifelse(commodities_list == "", "Other non-ferrous ores n.e.c.", commodities_list))

commodities_list <- commodities_list %>% 
  dplyr::bind_rows(manual_commodities %>% dplyr::select(1, 2))


data_cluster <- data_cluster_forest_loss %>%
  dplyr::left_join(commodities_list %>% dplyr::mutate(hcluster_id = as.integer(hcluster_id)))
data_cluster <- data_cluster%>%
  tidyr::separate_rows(commodities_list, sep = ",") %>%
  dplyr::rename(commodity = commodities_list) %>%
  dplyr::mutate(commodity = ifelse(is.na(commodity), "unknown", commodity))

# now, due to double counting, deforestation is substantially larger than in the measured raw data
sum(data_cluster$def_total)
sum(data_cluster_forest_loss$def_total) 

# normalize to original deforestation area by equally weighting each commodity
data_cluster <- data_cluster %>%
  dplyr::group_by(hcluster_id, year) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::mutate(def_weight = def_total / n) %>% 
  dplyr::select(-def_total)

# tidy and plot -----------------------------------------------------------

data_cluster <- data_cluster %>%
  dplyr::mutate(commodity = ifelse(commodity == "Ferrochrome", "Iron Ore", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Tellurium", "Copper", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Lead", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Zinc", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Silver", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::group_by(hcluster_id, commodity, year) %>%
  dplyr::summarise(def_weight = sum(def_weight)) %>%
  dplyr::ungroup()

sum(data_cluster$def_weight) == sum(data_cluster_forest_loss$def_total) 

p <- data_cluster %>% 
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(def_weight = sum(def_weight)) %>%
  ggplot2::ggplot(aes(x = year, y = def_weight, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity", position = "stack")
ggplot2::ggsave("forest_loss_equal.png", 
                plot = p, device = "png", 
                path = paste0("./figures/checks"),
                scale = 1, width = 350, height = 200, units = "mm")

# write final data
cluster_forest_loss_equal_allocation <- data_cluster %>% 
  dplyr::select(hcluster_id, year, commodity, def_weight)
write.csv(cluster_forest_loss_equal_allocation, file = "data/forest_loss/cluster_forest_loss_equal_allocation.csv", row.names = FALSE)


# create extension --------------------------------------------------------

# concordance
conc <- read.csv(file = "data/forest_loss/mine_polygons_hcluster_concordance.csv",
                 colClasses = c("character")) %>%
  dplyr::select(hcluster_id, country_isoa3) %>%
  dplyr::group_by(hcluster_id) %>% dplyr::slice(1) %>%
  dplyr::mutate(hcluster_id = as.integer(hcluster_id))

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
  dplyr::summarise(forest_loss_km2 = sum(def_weight))

# export
write.csv(E, file = "data/forest_loss/extension_forest_loss_equal_allocation.csv", row.names = FALSE)


# visualise extension -----------------------------------------------------

# total
E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(limits = c(0, 1300), expand = c(0, 0)) +
  ggplot2::scale_x_continuous(limits = c(2000.5, 2019.5), breaks = seq(2001, 2019, 1), expand = c(0, 0)) +
  ggplot2::labs(title = "Forest loss induced by mining, equal allocation, 2001-2019", x = NULL, y = expression(Forest~loss~(km^2))) +
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
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_km2)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(.~commodity, scales = "free_y") +
  ggplot2::labs(title = "Forest loss induced by mining, equal allocation, 2001-2019", x = "Year", y = "Forest loss (square km)")



