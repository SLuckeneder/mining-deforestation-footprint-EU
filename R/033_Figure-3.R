

library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(ggalluvial)
library(countrycode)
library(cowplot)

path_results <- "data/results"


# A sankey ----------------------------------------------------------------

# adjust here to access the correct model results file:
file <- basename(results_agg_price)
store <- read.csv(file.path(path_results, file))

# aggregate EU27
EU27 <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN", "FRA", "GRC", "IRL", "ITA",
          "HRV", "LVA", "LTU", "LUX", "MLT", "NLD", "AUT", "POL", "PRT", 
          "ROU", "SWE", "SVK", "SVN", "ESP", "CZE", "HUN", "CYP")
p_dat <- store %>% 
  dplyr::mutate(to_region = ifelse(to_region %in% EU27, "EU27", to_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% EU27, "EU27", from_region))
p_dat <- p_dat %>% 
  dplyr::group_by(from_region, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup()

# aggregate ROW
top_producers <- p_dat %>% 
  dplyr::select(-to_region) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(from_region) %>%
  dplyr::mutate(share = sum(share)) %>%
  dplyr::mutate(y2001_2019 = sum (y2001_2019)) %>%
  dplyr::slice(1) %>%
  dplyr::arrange(-share) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(top_producers = ifelse(cumsum < 0.84, from_region, "ROW"))
top_producers <- unique(top_producers$top_producers)[unique(top_producers$top_producers) != "ROW"]

top_consumers <- p_dat %>% 
  dplyr::select(-from_region) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(to_region) %>%
  dplyr::mutate(share = sum(share)) %>%
  dplyr::slice(1) %>%
  dplyr::arrange(-share) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(top_consumers = ifelse(cumsum < 0.7, to_region, "ROW"))
top_consumers <- unique(top_consumers$top_consumers)[unique(top_consumers$top_consumers) != "ROW"]

sankey_data <- p_dat %>%
  dplyr::mutate(from_continent = countrycode::countrycode(from_region, "iso3c", "continent")) %>%
  dplyr::mutate(to_continent = countrycode::countrycode(to_region, "iso3c", "continent")) %>%
  dplyr::mutate(to_region2 = ifelse(to_region %in% top_consumers, to_region, "ROW")) %>%
  dplyr::mutate(from_region2 = ifelse(from_region %in% top_producers, from_region, "ROW")) %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 == "ROW" & from_continent == "Europe", "XEU", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 == "ROW" & from_continent == "Asia", "XAS", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 == "ROW" & from_continent == "Oceania", "XAS", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 == "ROW" & from_continent == "Americas", "XAM", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 == "ROW" & from_continent == "Africa", "XAF", from_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(to_region2 == "ROW" & to_continent == "Europe", "XEU", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(to_region2 == "ROW" & to_continent == "Asia", "XAS", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(to_region2 == "ROW" & to_continent == "Oceania", "XAS", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(to_region2 == "ROW" & to_continent == "Americas", "XAM", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(to_region2 == "ROW" & to_continent == "Africa", "XAF", to_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "EU27", "EU27", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "XEU", "XEU", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "XAS", "XAS", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "XAM", "XAM", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "XAF", "XAF", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "DYE", "XAF", from_region2)) %>%
  dplyr::mutate(from_region2 = ifelse(is.na(from_region2) & from_region == "SDS", "XAF", from_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "EU27", "EU27", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "XEU", "XEU", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "XAS", "XAS", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "XAM", "XAM", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "XAF", "XAF", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "DYE", "XAF", to_region2)) %>%
  dplyr::mutate(to_region2 = ifelse(is.na(to_region2) & to_region == "SDS", "XAF", to_region2)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(from_region2, to_region2) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

sankey_data2 <- sankey_data %>%
  dplyr::mutate(from_region2 = ifelse(from_region2 %in% c("EU27", "XAF", "XAS", "XEU"), "ROW", from_region2)) %>%
  dplyr::group_by(from_region2, to_region2) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))
sankey_data2$from_region2 <- factor(sankey_data2$from_region2,
                                    levels = c(unique(sankey_data2$from_region2)[unique(sankey_data2$from_region2) != "ROW"], "ROW"))

sankey_data_EU <- sankey_data2 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(from_region_EU = ifelse(to_region2 == "EU27", paste0(from_region2, " "), as.character(from_region2)) )

sankey_data_EU$from_region_EU <- factor(sankey_data_EU$from_region_EU,
                                        levels = c("AUS", "BRA", "CAN", "GHA", "IDN", "PER", "RUS", "USA", "XAM", "ROW",
                                                   "AUS ", "BRA ", "CAN ", "GHA ", "IDN ", "PER ", "RUS ", "USA ", "XAM ", "ROW "))

p_sankey <- sankey_data_EU %>%
  ggplot2::ggplot(aes(y = y2001_2019, axis1 = from_region2, axis2 = to_region2)) +
  ggalluvial::geom_flow(aes(fill = from_region_EU), width = 1/24, aes.bind = TRUE) +
  ggalluvial::geom_stratum(width = 1/12, color = "black") +
  geom_text(aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")), stat = "stratum", size = 4) +
  geom_text( aes(label = ifelse(after_stat(x)  == 2, as.character(after_stat(stratum)), "")), stat = "stratum", size = 4) +
  ggplot2::scale_x_discrete(limits = c("Production", "Consumption"), expand = c(.05, .05)) +
  ggplot2::scale_fill_manual(values = c("#984ea3", "#1b4399", "#40a9d3", "#4daf4a", "#e41a1c", "#ff7f00", "#ffff33", "#a65628", "#3b3938", "black" ),
                             limits = c("AUS ", "BRA ", "CAN ", "GHA ", "IDN ", "PER ", "RUS ", "USA ", "XAM ", "ROW ")) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave("figure-3A_sankey.png",
                plot = p_sankey, device = "png",
                path = paste0("./figures/presentations/"),
                scale = 1, width = 200, height = 200, units = "mm")

p_sankey_world <- sankey_data_EU %>%
  ggplot2::ggplot(aes(y = y2001_2019, axis1 = from_region2, axis2 = to_region2)) +
  ggalluvial::geom_flow(aes(fill = from_region2), width = 1/24, aes.bind = TRUE) +
  ggalluvial::geom_stratum(width = 1/12, color = "black") +
  geom_text(aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")), stat = "stratum", size = 4) +
  geom_text( aes(label = ifelse(after_stat(x)  == 2, as.character(after_stat(stratum)), "")), stat = "stratum", size = 4) +
  ggplot2::scale_x_discrete(limits = c("Production", "Consumption"), expand = c(.05, .05)) +
  ggplot2::scale_fill_manual(values = c("#984ea3", "#1b4399", "#40a9d3", "#4daf4a", "#e41a1c", "#ff7f00", "#ffff33", "#a65628", "#3b3938", "black")) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none",
                 plot.background = element_blank())

ggplot2::ggsave("figure-S9_full_colour_sankey.png",
                plot = p_sankey_world, device = "png",
                path = paste0("./figures/"),
                scale = 1, width = 200, height = 200, units = "mm")



# B EU consumption --------------------------------------------------------

path_results <- "data/results"
years <- c(2001:2019)

files <- dir(path_results) %>% stringr::str_subset(pattern = "^sec.*\\.csv") %>%  stringr::str_subset(pattern = paste(years, collapse = "|"))
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 2)

store <- list()
for(i in seq_along(files)){
  print(files[i])
  results_raw <- read.csv(file.path(path_results, files[i])) %>%
    dplyr::select(5, 3, 4, 7, 8, 6)
  store[[i]] <- results_raw
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
  dplyr::group_by(from_sector, to_sector, from_region, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup()

# add sector labels
p_dat <- p_dat %>%
  dplyr::left_join(sectors, by = c("from_sector" = "Lfd_Nr")) %>%
  dplyr::rename("from_sector_name" = "Sector_names") %>%
  dplyr::left_join(sectors, by = c("to_sector" = "Lfd_Nr")) %>%
  dplyr::rename("to_sector_name" = "Sector_names")

EU_data <- p_dat %>%
  dplyr::filter(to_region == "EU27") %>%
  dplyr::group_by(from_region, from_sector_name) %>%
  dplyr::summarise( y2001_2019 = sum(y2001_2019))
 

# share of forest loss embodied in EU-27 metals and coal consumption occurring outside the EU-27 
in_EU <- EU_data %>% dplyr::filter(from_region == "EU27") %>% dplyr::pull(y2001_2019)
out_EU <- sum(EU_data$y2001_2019) - sum(in_EU)
out_EU / sum(EU_data$y2001_2019)

top_producers <- EU_data %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::group_by(from_region) %>%
  dplyr::mutate(region_total = sum(y2001_2019)) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(share = region_total / total) %>%
  dplyr::arrange(-share) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(top_producers = ifelse(cumsum < 0.97, from_region, "ROW"))
top_producers <- unique(top_producers$top_producers)[unique(top_producers$top_producers) != "ROW"]

EU_data <- EU_data %>% 
  dplyr::mutate(continent = countrycode::countrycode(from_region, "iso3c", "continent")) %>%
  dplyr::mutate(continent = ifelse(from_region == "EU27", "Europe", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XEU", "Europe", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAM", "Americas", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAS", "Asia", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAF", "Africa", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "DYE", "Africa", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "SDS", "Africa", continent)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% top_producers, from_region, "ROW")) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Europe", "XEU", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Asia", "XAS", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Oceania", "XAS", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Americas", "XAM", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Africa", "XAF", from_region)) %>%
  dplyr::mutate(continent = ifelse(continent == "Asia", "Asia-Pacific", continent)) %>%
  dplyr::mutate(continent = ifelse(continent == "Oceania", "Asia-Pacific", continent)) %>%
  dplyr::group_by(from_region, continent, from_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(y2001_2019)


from_sorted <- EU_data %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(total = sum(y2001_2019)) %>%
  dplyr::arrange(total) %>%
  dplyr::pull(from_region)
EU_data$from_region <- factor(EU_data$from_region, levels = c("EU27", from_sorted[from_sorted != "EU27"]))

EU_data <- EU_data %>% 
  dplyr::mutate(from_sector_name  = ifelse(from_sector_name == "Hard coal", "Coal (hard coal)", from_sector_name)) %>%
  dplyr::mutate(from_sector_name  = ifelse(from_sector_name == "Lignite and peat", "Coal (lignite and peat)", from_sector_name)) %>%
  dplyr::mutate(from_sector_name  = ifelse(from_sector_name == "Aluminium ore", "Bauxite", from_sector_name))

p_from_regions <- EU_data %>%
  ggplot2::ggplot(aes(x = from_region, y = y2001_2019, fill = from_sector_name)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = NULL, y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 250), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = NULL, values =  c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)])) + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.x = element_text(size = 14, hjust = 1),
                 legend.text = element_text(size = 14),
                 legend.position = c(0.7, 0.4),
                 legend.direction = "vertical",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.background = element_blank(),
                 plot.margin = unit(c(25, 20, 15, 20), "pt"))

ggplot2::ggsave("figure-3B_EU_consumption.png",
                plot = p_from_regions, device = "png",
                path = paste0("./figures/presentations/"),
                scale = 1, width = 200, height = 200, units = "mm")


# C sectors ---------------------------------------------------------------

top_producers <- p_dat %>%
  dplyr::filter(to_region == "EU27") %>%
  dplyr::group_by(from_region ) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(from_region2 = ifelse(cumsum < 0.8, from_region, "ROW"))
top_producers <- unique(top_producers$from_region2)

p_dat_EU <- p_dat %>%
  dplyr::filter(to_region == "EU27") %>%
  dplyr::mutate(continent = countrycode::countrycode(from_region, "iso3c", "continent")) %>%
  dplyr::mutate(continent = ifelse(from_region == "EU27", "Europe", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XEU", "Europe", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAM", "Americas", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAS", "Asia", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "XAF", "Africa", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "DYE", "Africa", continent)) %>%
  dplyr::mutate(continent = ifelse(from_region == "SDS", "Africa", continent)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% top_producers, from_region, "ROW")) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Europe", "XEU", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Asia", "XAS", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Oceania", "XAS", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Americas", "XAM", from_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region == "ROW" & continent == "Africa", "XAF", from_region)) %>%
  dplyr::mutate(continent = ifelse(continent == "Asia", "Asia-Pacific", continent)) %>%
  dplyr::mutate(continent = ifelse(continent == "Oceania", "Asia-Pacific", continent))%>%
  dplyr::group_by(from_region, to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

top_consumers_sectors <- p_dat_EU %>%
  dplyr::group_by(to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(to_sector_name2 = ifelse(cumsum < 0.875, to_sector_name, "Other sectors"))
top_consumers_sectors <- top_consumers_sectors %>% 
  dplyr::mutate(to_sector_name2 = 
                  ifelse(to_sector_name2 == "Other sectors", 
                         paste0("Other sectors (n = ", 121-length(unique(top_consumers_sectors$to_sector_name2)), ")"), 
                         to_sector_name2))
top_consumers_sectors <- top_consumers_sectors %>% dplyr::select(to_sector_name, to_sector_name2)
p_dat_EU <- p_dat_EU %>% dplyr::left_join(top_consumers_sectors)
p_dat_EU$to_sector_name2 <- factor(p_dat_EU$to_sector_name2, levels = rev(unique(top_consumers_sectors$to_sector_name2)))

top_producers_regions <- p_dat_EU %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)
p_dat_EU$from_region <- factor(p_dat_EU$from_region, levels = rev(top_producers_regions$from_region))

p_EU <- p_dat_EU %>%
  dplyr::group_by(to_sector_name2, from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  ggplot2::ggplot(aes(x = to_sector_name2, y = y2001_2019, fill = from_region)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = NULL, y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 180), breaks = scales::pretty_breaks(8), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = "Forest loss impact", 
                             values = c("IDN" = "#e41a1c", 
                                        "BRA" = "#1b4399", 
                                        "CAN" = "#40a9d3",
                                        "XAF" = "#30873e", 
                                        "GHA" = "#4daf4a", 
                                        "XAS" = "#f94de2",
                                        "RUS" = "#ffff33", 
                                        "USA" = "#a65628", 
                                        "XEU" = "#f77d7d", 
                                        "XAM" = "#999999", 
                                        "EU27" = "#3b3938"))+
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.x = element_text(size = 14, hjust = 1),
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 12),
                 legend.position = c(0.5, 0.1),
                 legend.direction = "horizontal",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.background = element_blank(),
                 plot.margin = unit(c(5.5, 20.5, 5.5, 5.5), "pt")) +
  ggplot2::guides(fill = guide_legend(reverse=T, title.position = "top", title.hjust = 0.5, nrow = 1))


# merge -------------------------------------------------------------------

p_AB <- cowplot::plot_grid(p_sankey, p_from_regions , nrow=1, labels = c("A", "B"), label_size = 16)
p_ABC <- cowplot::plot_grid(p_AB, p_EU , nrow=2, labels = c("", "C"), label_size = 16)

ggplot2::ggsave("figure-3_finding-2.png",
                plot = p_ABC, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 400, height = 350, units = "mm")


# numbers for manuscript text ---------------------------------------------

sum(sankey_data2$y2001_2019) # total forest loss (excluding undefined commodities)
1416 / (sum(sankey_data2$y2001_2019)) # EU share
sankey_data2 %>% dplyr::group_by(to_region2) %>% # by consumer region
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)

# share of forest loss embodied in EU-27 metals and coal consumption occurring outside the EU-27 
in_EU <- EU_data %>% dplyr::filter(from_region == "EU27") %>% dplyr::pull(y2001_2019)
out_EU <- sum(EU_data$y2001_2019) - sum(in_EU)
out_EU / sum(EU_data$y2001_2019)

# EU consumption by impact region
EU_data %>% dplyr::group_by(from_region) %>% 
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)
# Americas
americas <- EU_data %>% dplyr::group_by(from_region) %>% 
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::filter(from_region %in% c("BRA", "CAN", "XAM", "USA", "PER", "COL", "VEN"))
sum(americas$y2001_2019)

# cummulative footprint of Global North, China and India
sub <- sankey_data_EU %>%
  dplyr::group_by(to_region2) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::filter(to_region2 %in% c("CHN", "EU27", "USA", "XEU", "JPN", "IND", "CAN", "GBR")) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(y2001_2019= sum(y2001_2019)) %>% dplyr::pull(y2001_2019)
sub / sum(sankey_data_EU$y2001_2019)

# EU footprint by region and commodity
EU_data %>% dplyr::filter(from_region %in% c("IDN", "RUS")) %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_region %in% c("BRA")) %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_region %in% c("PER", "VEN", "CIV"), from_sector_name == "Gold ores") %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_sector_name == "Aluminium ore") %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_sector_name == "Copper ores") %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_sector_name == "Coal") %>% dplyr::arrange(-y2001_2019)
EU_data %>% dplyr::filter(from_sector_name == "Nickel ores") %>% dplyr::arrange(-y2001_2019)

# coal within EU
EU_data %>% dplyr::filter(from_region == "EU27") %>% dplyr::summarise(y2001_2019 = sum(y2001_2019))
p_dat <- store %>% 
  dplyr::mutate(to_region = ifelse(to_region %in% EU27, "EU27", to_region))
p_dat <- p_dat %>% 
  dplyr::group_by(from_sector, from_region, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup()
p_dat <- p_dat %>%
  dplyr::left_join(sectors, by = c("from_sector" = "Lfd_Nr")) %>%
  dplyr::rename("from_sector_name" = "Sector_names") 

p_dat %>% dplyr::filter(to_region == "EU27") %>% 
  dplyr::filter(from_sector %in% c(24, 25)) %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)

# EU by sector
p_dat_EU %>% dplyr::group_by(to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::mutate(cumsum = cumsum(share))

p_dat_EU %>%
  dplyr::filter(to_sector_name == "Motor vehicles, trailers and semi-trailers") %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::mutate(cumsum = cumsum(share))

