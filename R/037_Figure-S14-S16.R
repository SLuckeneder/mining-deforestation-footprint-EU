
library(dplyr)
library(ggplot2)
library(data.table)
library(scales)
library(countrycode)
library(wesanderson)

# load extensions ---------------------------------------------------------


E_mass <- read.csv("data/forest_loss/extension_forest_loss_mass_allocation.csv") %>%
  dplyr::mutate(type = "mass")
E_price <- read.csv("data/forest_loss/extension_forest_loss_price_allocation.csv") %>%
  dplyr::mutate(type = "price")
E_primary <- read.csv("data/forest_loss/extension_forest_loss_primary_allocation.csv") %>%
  dplyr::mutate(type = "primary")
E_equal <- read.csv("data/forest_loss/extension_forest_loss_equal_allocation.csv") %>%
  dplyr::mutate(type = "equal")


# coal by main source of coal
lignite <- c("ALB", "ARM", "AUT", "BLR", "BIH", "BGR", "BDI", "CZE", "CSK", "DNK", "EST", "FIN", "GEO", "DEU", "GRC", "HUN", 
             "IRL", "ITA", "KSV", "KGZ", "LAO", "LVA", "LTU", "MNE", "MKD", "ROU", "RWA", "SRB", "YUG", "SVK", "SVN", "SWE", 
             "THA", "TUR", "UZB")
E_mass <- E_mass %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Coal (lignite and peat)", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Coal (hard coal)", commodity))
E_price <- E_price %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Coal (lignite and peat)", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Coal (hard coal)", commodity))
E_primary <- E_primary %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Coal (lignite and peat)", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Coal (hard coal)", commodity))
E_equal <- E_equal %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Coal (lignite and peat)", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Coal (hard coal)", commodity))

E <- dplyr::bind_rows(E_mass, E_price, E_primary, E_equal)


E <- E %>%
  dplyr::mutate(commodity = ifelse(commodity == "Copper", "Copper ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Gold", "Gold ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Iron Ore", "Iron ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Lead/Zinc/Silver", "Lead/zinc/silver ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Nickel", "Nickel ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Other non-ferrous ores n.e.c.", "Other non-ferrous ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Tin", "Tin ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "U3O8", "Uranium ores", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "unknown", "Unknown", commodity))

E$commodity <- factor(E$commodity, levels = c(unique(E$commodity)))


# S14 forest loss by allocation type --------------------------------------

p <- E %>% 
  dplyr::filter(year %in% c(2001, 2005, 2010, 2015, 2019)) %>%
  dplyr::group_by(year, commodity, type) %>%
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = type, y = forest_loss_km2, fill = commodity), position = "fill") +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid(.~year) +
  ggplot2::scale_y_continuous(limits = c(0, 800), expand = c(0, 0)) +
  ggplot2::labs(title = NULL, x = NULL, y = expression("Forest loss (km"^2~")")) +
  ggplot2::scale_fill_manual(name = NULL, values =  c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)], "grey")) + 
  ggplot2::theme_bw() +
  ggplot2::theme(title = element_text(size = 16),
                 axis.text.x = element_text(size = 14, angle = 45, hjust=1),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 16),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 strip.background = element_blank(),
                 strip.text.x = element_text(size = 14, face = "bold"))

ggplot2::ggsave("figure-S14_allocation_comparison_forest_loss.png", 
                plot = p, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 400, height = 150, units = "mm")


# prepare footprint results -----------------------------------------------

path_results <- "data/results"

# rename files accordingly!
files <- c("agg_price_2001-2019_2024-10-08 14:49:30.353889.csv", 
           "agg_mass_2001-2019_2024-10-08 18:06:25.887588.csv", 
           "agg_equal_2001-2019_2024-10-09 11:54:50.346649.csv",
           "agg_primary_2001-2019_2024-10-09 16:06:50.373638.csv")
allocations <- c("price", "mass", "equal", "primary")

store <- list()
for(i in seq_along(files)){
  results <- read.csv(file.path(path_results, files[i])) %>%
    dplyr::select(3:6) %>%
    dplyr::mutate(allocation = allocations[i])
  store[[i]] <- results
}
store <- do.call("rbind", store)

# aggregate EU27
EU27 <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN", "FRA", "GRC", "IRL", "ITA",
          "HRV", "LVA", "LTU", "LUX", "MLT", "NLD", "AUT", "POL", "PRT", 
          "ROU", "SWE", "SVK", "SVN", "ESP", "CZE", "HUN", "CYP")
p_dat <- store %>% 
  dplyr::mutate(to_region = ifelse(to_region %in% EU27, "EU27", to_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% EU27, "EU27", from_region))
p_dat <- p_dat %>% 
  dplyr::group_by(from_region, to_region, allocation) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup()

p_dat %>% dplyr::group_by(allocation) %>% dplyr::summarise(y2001_2019 = sum(y2001_2019))



# S15 production perspective by allocation type ---------------------------

p_dat_production <- p_dat %>%
  dplyr::group_by(from_region, allocation) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::ungroup()

top_producers <- p_dat_production %>% 
  dplyr::filter(allocation == "price", y2001_2019 > 0) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-share) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(top_producers = ifelse(cumsum < 0.92, from_region, "ROW"))
top_producers <- unique(top_producers$top_producers)[unique(top_producers$top_producers) != "ROW"]
p_dat_production <- p_dat_production %>%
  dplyr::mutate(from_region = ifelse(from_region %in% top_producers, from_region, "ROW"))
p_dat_production$from_region <- factor(p_dat_production$from_region, levels = c(top_producers, "ROW"))

p <- p_dat_production %>%
  dplyr::group_by(from_region, allocation) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  ggplot2::ggplot(aes(x = from_region, y = y2001_2019, fill = allocation)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::labs(x = "Producer region", y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 3500), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = "Allocation type", values = wes_palette("Moonrise2", 4, type = "discrete")) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x = element_text(size = 14, hjust = 1),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 14),
                 legend.position = c(0.5, 0.8),
                 legend.direction = "horizontal",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank()) +
  ggplot2::guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

ggplot2::ggsave("figure-S15_allocation_comparison_production.png", 
                plot = p, device = "png", 
                path = paste0("./figures//"),
                scale = 1, width = 200, height = 100, units = "mm")


# S16 consumption perspective by allocation type --------------------------

p_dat_consumption <- p_dat %>%
  dplyr::group_by(to_region, allocation) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::ungroup()

top_consumers <- p_dat_consumption %>% 
  dplyr::filter(allocation == "price", y2001_2019 > 0) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-share) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(top_consumers = ifelse(cumsum < 0.85, to_region, "ROW"))
top_consumers <- unique(top_consumers$top_consumers)[unique(top_consumers$top_consumers) != "ROW"]
p_dat_consumption <- p_dat_consumption %>%
  dplyr::mutate(to_region = ifelse(to_region %in% top_consumers, to_region, "ROW"))
p_dat_consumption$to_region <- factor(p_dat_consumption$to_region, levels = c(top_consumers, "ROW"))

p <- p_dat_consumption %>%
  dplyr::group_by(to_region, allocation) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  ggplot2::ggplot(aes(x = to_region, y = y2001_2019, fill = allocation)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::labs(x = "Consumer region", y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 2200), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = "Allocation type", values = wes_palette("Moonrise2", 4, type = "discrete")) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12),
                 axis.title.x = element_text(size = 14, hjust = 1),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 14),
                 legend.position = c(0.5, 0.8),
                 legend.direction = "horizontal",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank()) +
  ggplot2::guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

ggplot2::ggsave("figure-S16_allocation_comparison_consumption.png", 
                plot = p, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 200, height = 100, units = "mm")


