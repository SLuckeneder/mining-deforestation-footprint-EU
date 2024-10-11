
library(dplyr)
library(ggplot2)

# prepare cluster data ---------------------------------------------------------

data_cluster_forest_loss <- read.csv("data/parsed/cluster_forest_loss.csv") # deforestation
data_cluster_production <- read.csv("data/parsed/cluster_production.csv") # known production

# remove clusters that were not matched with any SNL (production) ID (only 68 out of 44880 observations)
data_cluster_production <- data_cluster_production %>% dplyr::filter(!is.na(hcluster_id))

# remove diamonds (non-ores) 
data_cluster_production <- data_cluster_production %>%
  dplyr::filter(commodity != "Diamonds")

# set commodities considered in GLORIA tables, group the rest as "other"
# GLORIA covers 8 metal extraction sectors + 1 other non-ferrous ores n.e.c. + Coal: 
# Ferrous ores, uranium, aluminium, copper, gold, lead/zinc/silver, nickel, tin
GLORIA_commodities <- c("Iron Ore", "Ferrochrome", "U3O8", "Bauxite", "Copper", "Tellurium", "Gold", "Lead", "Zinc", "Silver", "Nickel", "Tin", "Coal")
data_cluster_production <- data_cluster_production %>%
  dplyr::mutate(commodity = ifelse(commodity %in% GLORIA_commodities, commodity, "Other non-ferrous ores n.e.c.")) %>% 
  dplyr::mutate(commodity = ifelse(commodity == "Ferrochrome", "Iron Ore", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Tellurium", "Copper", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Lead", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Zinc", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Silver", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::group_by(hcluster_id, commodity, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

# remove clusters where no forest loss occurred and subset to study time coverage
data_cluster_forest_loss <- data_cluster_forest_loss %>%
  dplyr::rename(def_total = total) %>%
  dplyr::filter(!is.na(def_total) & def_total > 0, year %in% c(2001:2019))

# merge production and forest loss in clusters ----------------------------

# create yearly IDs
data_cluster_forest_loss <- data_cluster_forest_loss %>% 
  dplyr::mutate(unid = paste(hcluster_id, year, sep = "_"))
data_cluster_production <- data_cluster_production %>% 
  dplyr::mutate(unid = paste(hcluster_id, year, sep = "_")) %>%
  dplyr::rename("production_tonnes" = "value") %>%
  dplyr::select(-hcluster_id, -year)

data_cluster <- data_cluster_forest_loss %>%
  dplyr::left_join(data_cluster_production, by = "unid") %>%
  dplyr::mutate(commodity = ifelse(is.na(commodity), "unknown", commodity))


# mass allocation ---------------------------------------------------------

data_cluster <- data_cluster %>%
  dplyr::group_by(unid) %>%
  dplyr::mutate(prod_total = sum(production_tonnes)) %>%
  dplyr::mutate(prod_weight = production_tonnes / prod_total)
  
temp <- data_cluster %>% dplyr::filter(commodity == "unknown") %>% dplyr::group_by(hcluster_id) %>% slice(1) %>% 
  dplyr::pull(hcluster_id)
temp <- data_cluster %>% dplyr::filter(hcluster_id %in% temp) %>% dplyr::group_by(hcluster_id, commodity) %>% slice(1) %>% 
  dplyr::pull(hcluster_id)
unkown_single <- temp[!duplicated(temp)]
# unkown_byprod <- temp[duplicated(temp)] # not needed, as the case of unknown weights for byproducts does not occur, see check below sum == sum
data_cluster <- data_cluster %>%
  dplyr::mutate(prod_weight = ifelse(hcluster_id %in% unkown_single & is.na(prod_weight), 1, prod_weight)) %>%
  dplyr::mutate(def_weight = def_total * prod_weight,
                estimate = FALSE)

sum(data_cluster_forest_loss$def_total) == sum(data_cluster$def_weight)

p <- data_cluster %>% 
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(def_weight = sum(def_weight)) %>%
  ggplot2::ggplot(aes(x = year, y = def_weight, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity", position = "stack")
ggplot2::ggsave("forest_loss_mass_raw.png", 
                plot = p, device = "png", 
                path = paste0("./figures/checks"),
                scale = 1, width = 350, height = 200, units = "mm")

# gap filling -------------------------------------------------------------

known <- data_cluster %>% dplyr::filter(commodity != "unknown")
sum(known$def_weight) / sum(data_cluster$def_weight)
# only half of total deforestation can be allocated, for many clusters no production is available.

### fill entries where commodities of other years are known - single commodities
check <- data_cluster %>% dplyr::filter(hcluster_id %in% temp)
check <- check %>% dplyr::group_by(hcluster_id, commodity) %>% dplyr::slice(1) %>% dplyr::ungroup()
check <- check %>% count(unid) %>% dplyr::filter(n == 1) %>% dplyr::pull(unid)
check <- data_cluster %>% dplyr::filter(unid %in% check) %>% dplyr::filter(commodity!= "unknown") %>%
  dplyr::ungroup() %>%
  dplyr::select(hcluster_id, commodity) %>%
  dplyr::rename(commodity_rep = commodity) %>%
  dplyr::group_by(hcluster_id, commodity_rep) %>% dplyr::slice(1) %>% dplyr::ungroup() 
check_single <- check %>% dplyr::count(hcluster_id) %>% dplyr::filter(n == 1) %>%dplyr::pull(hcluster_id)
check <- check %>% dplyr::filter(hcluster_id %in% check_single)
  
data_cluster <- data_cluster %>%
  dplyr::left_join(check, by = "hcluster_id") %>%
  dplyr::mutate(estimate = ifelse(!is.na(commodity_rep) & commodity == "unknown", TRUE, estimate),
                commodity = ifelse(!is.na(commodity_rep) & commodity == "unknown", commodity_rep, commodity)) %>%
  dplyr::select(-commodity_rep)

sum(data_cluster_forest_loss$def_total) == sum(data_cluster$def_weight) 
known <- data_cluster %>% dplyr::filter(commodity != "unknown")
sum(known$def_weight) / sum(data_cluster$def_weight)
rm(check, temp, known)


### fill entries where commodities of other years are known - multiple commodities: compute weights from accumulated known production
temp <- data_cluster %>% dplyr::filter(commodity == "unknown") %>% dplyr::group_by(hcluster_id) %>% slice(1) %>% 
  dplyr::pull(hcluster_id)
temp <- data_cluster %>% dplyr::filter(hcluster_id %in% temp) %>% dplyr::group_by(hcluster_id, commodity) %>% slice(1) %>% 
  dplyr::pull(hcluster_id)
unkown_byprod <- temp[duplicated(temp)]
temp <- data_cluster %>% dplyr::filter(hcluster_id %in% unkown_byprod)
fill_unids <- temp %>% dplyr::filter(commodity == "unknown") %>% dplyr::pull(unid)
fill_conc <- temp %>% dplyr::filter(commodity != "unknown") %>%
  dplyr::group_by(hcluster_id, commodity) %>%
  dplyr::summarise(prod_commodity = sum(production_tonnes, na.rm = TRUE)) %>%
  dplyr::group_by(hcluster_id) %>%
  dplyr::mutate(prod_total = sum(prod_commodity, na.rm = TRUE)) %>%
  dplyr::mutate(fill_weight = prod_commodity / prod_total) %>%
  dplyr::select(hcluster_id, commodity, fill_weight) %>%
  dplyr::rename(fill_commodity = commodity)
temp <- temp %>% dplyr::filter(unid %in% fill_unids) %>%
  dplyr::left_join(fill_conc) %>%
  dplyr::mutate(def_weight = def_weight * fill_weight,
                commodity = fill_commodity,
                estimate = TRUE) %>%
  dplyr::select(-fill_weight, -fill_commodity)
data_cluster <- data_cluster %>% dplyr::filter(! unid %in% fill_unids) %>%
  dplyr::bind_rows(temp)

sum(data_cluster_forest_loss$def_total) == sum(data_cluster$def_weight) 
known <- data_cluster %>% dplyr::filter(commodity != "unknown")
sum(known$def_weight) / sum(data_cluster$def_weight)
rm(temp, known)


### compute global shares based on polymetallic combinations and shares known from the step above and assign forest loss to clusters

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

# sort string with commodities alphabetically
conc_combinations <- data.frame(
  hcluster_id = commodities_list$hcluster_id,
  commodities_sorted = unname(sapply(commodities_list$commodities_list, function(x) {
    paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse=',')} ))
) %>% 
  dplyr::mutate(commodities_sorted = sub(pattern = "^,", "", commodities_sorted)) %>% 
  dplyr::mutate(commodities_sorted = ifelse(commodities_sorted == "", "Other non-ferrous ores n.e.c.", commodities_sorted)) %>%
  dplyr::mutate(hcluster_id = as.integer(hcluster_id))

# wherever revenue weight = 1 (only 1 commodity produced within cluster) -> forest weight also = 1
data_cluster <- data_cluster  %>% 
  dplyr::left_join(conc_combinations, by = "hcluster_id") %>%
  dplyr::mutate(n_commodity = stringr::str_count(commodities_sorted, ",") + 1)
data_cluster <- data_cluster %>%
  dplyr::mutate(estimate = ifelse(commodity == "unknown" & n_commodity == 1, TRUE, estimate)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "unknown" & n_commodity == 1, commodities_sorted, commodity)) %>%
  dplyr::mutate(commodity = ifelse(is.na(commodity), "unknown", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Lead", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Zinc", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Silver", "Lead/Zinc/Silver", commodity)) %>%
  dplyr::select(-n_commodity)

sum(data_cluster_forest_loss$def_total) == sum(data_cluster$def_weight) 
known <- data_cluster %>% dplyr::filter(commodity != "unknown")
sum(known$def_weight) / sum(data_cluster$def_weight); rm(known)
  
# shares within cluster
commodity_weights <- data_cluster %>% 
  dplyr::filter(!commodity %in% c("Other non-ferrous ores n.e.c.", "unknown")) %>%
  dplyr::group_by(hcluster_id, commodities_sorted, commodity) %>%
  dplyr::summarise(commodity_weight = mean(prod_weight)) %>%
  dplyr::filter(commodity_weight != 1) # for some combinations, only one commodity is reported in terms of production, which would give a weight of 1. Remove

# average shares for all observed commodity combinations
commodity_weights <- commodity_weights %>% 
  dplyr::group_by(commodities_sorted, commodity) %>%
  dplyr::summarise(commodity_weight = mean(commodity_weight)) %>%
  dplyr::ungroup()

# make sure weight sum up to 1
commodity_weights %>% dplyr::group_by(commodities_sorted) %>%
  dplyr::summarise(total = sum(commodity_weight))
commodity_weights <- commodity_weights %>% 
  dplyr::group_by(commodities_sorted) %>%
  dplyr::mutate(unbalanced_sum = sum(commodity_weight)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(commodity_weight = commodity_weight / unbalanced_sum) %>%
  dplyr::select(-unbalanced_sum)

# apply weights
data_cluster_gap_fill <- data_cluster %>% 
  dplyr::filter(commodity == "unknown" & !is.na(commodities_sorted)) %>%
  dplyr::select(-commodity) %>%
  dplyr::left_join(commodity_weights, by = "commodities_sorted") %>%
  dplyr::mutate(def_weight = def_weight * commodity_weight) %>%
  dplyr::filter(!is.na(def_weight))

# merge with already allocated deforestation data
data_cluster <- data_cluster %>% 
  dplyr::filter(! unid %in% data_cluster_gap_fill$unid) %>%
  dplyr::bind_rows(data_cluster_gap_fill)

sum(data_cluster_forest_loss$def_total) == sum(data_cluster$def_weight) 
known <- data_cluster %>% dplyr::filter(commodity != "unknown")
sum(known$def_weight) / sum(data_cluster$def_weight); rm(known)

# mass allocation after gap filling ---------------------------------------

p <- data_cluster %>% 
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(def_weight = sum(def_weight)) %>%
  ggplot2::ggplot(aes(x = year, y = def_weight, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity", position = "stack")
ggplot2::ggsave("forest_loss_mass_fill.png", 
                plot = p, device = "png", 
                path = paste0("./figures/checks"),
                scale = 1, width = 350, height = 200, units = "mm")

# write final data
cluster_forest_loss_mass_allocation <- data_cluster %>% 
  dplyr::select(hcluster_id, year, commodity, def_weight)
write.csv(cluster_forest_loss_mass_allocation, file = "data/forest_loss/cluster_forest_loss_mass_allocation.csv", row.names = FALSE)


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
cluster_forest_loss_mass_allocation <- cluster_forest_loss_mass_allocation %>% dplyr::left_join(conc)

# data to extension format ------------------------------------------------

# i.e.: per commodity, country and year
E <- cluster_forest_loss_mass_allocation %>%
  dplyr::group_by(year, commodity, country_isoa3) %>%
  dplyr::summarise(forest_loss_km2 = sum(def_weight))

# export
write.csv(E, file = "data/forest_loss/extension_forest_loss_mass_allocation.csv", row.names = FALSE)


# visualise extension -----------------------------------------------------

# total
E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(limits = c(0, 1300), expand = c(0, 0)) +
  ggplot2::scale_x_continuous(limits = c(2000.5, 2019.5), breaks = seq(2001, 2019, 1), expand = c(0, 0)) +
  ggplot2::labs(title = "Forest loss induced by mining, mass allocation, 2001-2019", x = NULL, y = expression(Forest~loss~(km^2))) +
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
  ggplot2::labs(title = "Forest loss induced by mining, mass allocation, 2001-2019", x = "Year", y = "Forest loss (square km)")

