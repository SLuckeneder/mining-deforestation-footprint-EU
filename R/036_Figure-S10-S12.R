
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(countrycode)
library(cowplot)

path_results <- "data/results"
years <- c(2001:2019)
files <- dir(path_results) %>%  stringr::str_subset(pattern = "^sec.*\\.csv") %>%  stringr::str_subset(pattern = paste(years, collapse = "|"))
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 2)

# read and merge sector-level footprint results ---------------------------

store <- list()
for(i in seq_along(files)){
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


# S10 forest loss footprint China -----------------------------------------

top_producers <- p_dat %>%
  dplyr::filter(to_region == "CHN") %>%
  dplyr::group_by(from_region ) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(from_region2 = ifelse(cumsum < 0.8, from_region, "ROW"))
top_producers <- unique(top_producers$from_region2)

p_dat_CHN <- p_dat %>%
  dplyr::filter(to_region == "CHN") %>%
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
  dplyr::group_by(from_region, to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

top_consumers_sectors <- p_dat_CHN %>%
  dplyr::group_by(to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(to_sector_name2 = ifelse(cumsum < 0.9, to_sector_name, "Other sectors"))
top_consumers_sectors <- top_consumers_sectors %>% 
  dplyr::mutate(to_sector_name2 = 
                  ifelse(to_sector_name2 == "Other sectors", 
                         paste0("Other sectors (n = ", 121-length(unique(top_consumers_sectors$to_sector_name2)), ")"), 
                         to_sector_name2))
top_consumers_sectors <- top_consumers_sectors %>% dplyr::select(to_sector_name, to_sector_name2)
p_dat_CHN <- p_dat_CHN %>% dplyr::left_join(top_consumers_sectors)
p_dat_CHN$to_sector_name2 <- factor(p_dat_CHN$to_sector_name2, levels = rev(unique(top_consumers_sectors$to_sector_name2)))

top_producers_regions <- p_dat_CHN %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)
p_dat_CHN$from_region <- factor(p_dat_CHN$from_region, levels = rev(top_producers_regions$from_region))

p_CHN <- p_dat_CHN %>%
  dplyr::group_by(to_sector_name2, from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  ggplot2::ggplot(aes(x = to_sector_name2, y = y2001_2019, fill = from_region)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = NULL, y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 550), breaks = scales::pretty_breaks(4), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = "Forest loss location",
                             values = c(
                               "IDN" = "#e41a1c",
                               "BRA" = "#1b4399",
                               "CAN" = "#40a9d3",
                               "XAF" = "#30873e",
                               "GHA" = "#4daf4a",
                               "AUS" = "#984ea3",
                               "XAS" = "#f94de2",
                               "RUS" = "#ffff33",
                               "XEU" = "#f77d7d",
                               "XAM" = "#999999",
                               "CHN" = "#890f0f",
                               "MMR" = "#616d0a"))+
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.title.x = element_text(size = 14, hjust = 1),
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 legend.position = c(0.5, 0.1),
                 legend.direction = "horizontal",
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.margin = unit(c(5.5, 20.5, 5.5, 5.5), "pt")) +
  ggplot2::guides(fill = guide_legend(reverse=T, title.position = "top", title.hjust = 0.5, nrow = 1))

ggplot2::ggsave("figure-S10_forest_loss_sectors_CHN.png", 
                plot = p_CHN, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 400, height = 200, units = "mm")


# S11 forest loss footprint USA -------------------------------------------

top_producers <- p_dat %>%
  dplyr::filter(to_region == "USA") %>%
  dplyr::group_by(from_region ) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(from_region2 = ifelse(cumsum < 0.8, from_region, "ROW"))
top_producers <- unique(top_producers$from_region2)

p_dat_USA <- p_dat %>%
  dplyr::filter(to_region == "USA") %>%
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
  dplyr::group_by(from_region, to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

top_consumers_sectors <- p_dat_USA %>%
  dplyr::group_by(to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(to_sector_name2 = ifelse(cumsum < 0.9, to_sector_name, "Other sectors"))
top_consumers_sectors <- top_consumers_sectors %>% 
  dplyr::mutate(to_sector_name2 = 
                  ifelse(to_sector_name2 == "Other sectors", 
                         paste0("Other sectors (n = ", 121-length(unique(top_consumers_sectors$to_sector_name2)), ")"), 
                         to_sector_name2))
top_consumers_sectors <- top_consumers_sectors %>% dplyr::select(to_sector_name, to_sector_name2)
p_dat_USA <- p_dat_USA %>% dplyr::left_join(top_consumers_sectors)
p_dat_USA$to_sector_name2 <- factor(p_dat_USA$to_sector_name2, levels = rev(unique(top_consumers_sectors$to_sector_name2)))

top_producers_regions <- p_dat_USA %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019)
p_dat_USA$from_region <- factor(p_dat_USA$from_region, levels = rev(top_producers_regions$from_region))

p_USA <- p_dat_USA %>%
  dplyr::group_by(to_sector_name2, from_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  ggplot2::ggplot(aes(x = to_sector_name2, y = y2001_2019, fill = from_region)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = NULL, y = expression(Forest~loss~(km^2))) +
  ggplot2::scale_y_continuous(limits = c(0, 150), breaks = scales::pretty_breaks(4), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = "Forest loss location",
                             values = c("BRA" = "#1b4399",
                                        "CAN" = "#40a9d3",
                                        "XAF" = "#30873e",
                                        "XAS" = "#f94de2",
                                        "PER" = "#ff7f00",
                                        "USA" = "#a65628",
                                        "XEU" = "#f77d7d",
                                        "XAM" = "#999999"))+
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
                 plot.margin = unit(c(5.5, 20.5, 5.5, 5.5), "pt")) +
  ggplot2::guides(fill = guide_legend(reverse=T, title.position = "top", title.hjust = 0.5, nrow = 1))

ggplot2::ggsave("figure-S11_forest_loss_sectors_USA.png", 
                plot = p_USA, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 400, height = 200, units = "mm")



# S12 doughnut charts footprint by consumption region --------------------

# Consumption perspective sectors
p_dat_consumer <- p_dat %>%
  dplyr::group_by(to_sector_name, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

top_consumers_sectors <- p_dat %>%
  dplyr::group_by(to_sector_name) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(to_sector_name2 = ifelse(cumsum < 0.9, to_sector_name, "Other sectors"))
top_consumers_sectors <- top_consumers_sectors %>% 
  dplyr::mutate(to_sector_name2 = 
                  ifelse(to_sector_name2 == "Other sectors", 
                         paste0("Other sectors (n = ", 121-length(unique(top_consumers_sectors$to_sector_name2)), ")"), 
                         to_sector_name2))
top_consumers_sectors <- top_consumers_sectors %>% dplyr::select(to_sector_name, to_sector_name2)
p_dat_consumer <- p_dat_consumer %>% dplyr::left_join(top_consumers_sectors)
p_dat_consumer$to_sector_name2 <- factor(p_dat_consumer$to_sector_name2, levels = rev(unique(top_consumers_sectors$to_sector_name2)))

top_consumers_regions <- p_dat_consumer %>%
  dplyr::group_by(to_region) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::mutate(total = sum(y2001_2019)) %>%
  dplyr::mutate(share = y2001_2019 / total) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::mutate(to_region2 = ifelse(cumsum < 0.7, to_region, "ROW"))
top_consumers_regions <- top_consumers_regions %>% dplyr::select(to_region, to_region2)
p_dat_consumer <- p_dat_consumer %>% dplyr::left_join(top_consumers_regions)
p_dat_consumer$to_region2 <- factor(p_dat_consumer$to_region2, levels = rev(unique(top_consumers_regions$to_region2)))

p_dat_consumer <- p_dat_consumer %>%
  dplyr::group_by(to_sector_name2, to_region2) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019))

# sort pies by total forest loss
sectors_sorted <- p_dat_consumer %>%
  dplyr::group_by(to_sector_name2) %>%
  dplyr::summarise(y2001_2019 = sum(y2001_2019)) %>%
  dplyr::arrange(-y2001_2019) %>%
  dplyr::filter(to_sector_name2 != "Other sectors (n = 84)")

# add shorter labels
sectors_sorted$to_sector_name_short <- c("Civil engineering \n construction",
                                         "Automotives\n", 
                                         "Building construction\n",
                                         "Machinery and equipment\n",
                                         "Electrical equipment\n",
                                         "Furniture and other \nmanufacturing",
                                         "Electric power generation, \ntransmission and distribution",
                                         "Computers; electronic products;\n optical and precision instruments",
                                         "Government; social security;\n defence; public order",
                                         "Gold ores\n",
                                         "Fabricated metal products\n",
                                         "Wholesale and retail trade; \nrepair of automotives",
                                         "Other transport equipment\n",
                                         "Human health and social \nwork activities",
                                         "Hard coal\n",
                                         "Basic Gold\n",
                                         "Property and real estate\n",
                                         "Other non-ferrous ores\n",
                                         "Hospitality\n",
                                         "Refined petroleum products\n",
                                         "Education\n",
                                         "Textiles and clothing\n",
                                         "Distribution of gaseous \nfuels through mains",
                                         "Professional, scientific and \ntechnical services",
                                         "Basic non-ferrous metals\n",
                                         "Telecommunications\n",
                                         "Other services\n",
                                         "Coke oven products\n",
                                         "Lignite and peat\n",
                                         "Finance and insurance\n",
                                         "Pharmaceuticals and \nmedicinal products",
                                         "Alcoholic and \nother beverages",
                                         "Arts, entertainment \nand recreation",
                                         "Aluminium ore\n",
                                         "Basic iron and steel\n",
                                         "Information services\n")

# plot first pie
pie_dat <- p_dat_consumer %>% dplyr::filter(to_sector_name2 == sectors_sorted$to_sector_name2[1])
pie_dat$fraction = pie_dat$y2001_2019 / sum(pie_dat$y2001_2019)
pie_dat$ymax = cumsum(pie_dat$fraction)
pie_dat$ymin = c(0, head(pie_dat$ymax, n=-1))
pie <- pie_dat %>%
  ggplot2::ggplot(aes(ymax=ymax, ymin=ymin, xmax = 4, xmin = 3, fill = to_region2)) +
  ggplot2::geom_rect() +
  ggplot2::coord_polar(theta="y") +
  ggplot2::xlim(c(2, 4)) +
  viridis::scale_fill_viridis(discrete = T, name = "Consumer regions", option = "plasma") +
  ggplot2::annotate('text', x = 2, y = 0.5, label = sectors_sorted$to_sector_name2[1], size = 5) +
  ggplot2::theme_void()

# store legend
legend_a <- cowplot::get_legend(
  pie + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 22),
          legend.title =  element_text(size = 25)) +
    ggplot2::guides(fill = guide_legend(reverse=T, title.position = "top", title.hjust = 0.5, nrow = 1))
)

# plot all pies and store
store <- list()
for(i in seq_along(sectors_sorted$to_sector_name2)){
  pie_dat <- p_dat_consumer %>% dplyr::filter(to_sector_name2 == sectors_sorted$to_sector_name2[i])
  pie_dat$fraction = pie_dat$y2001_2019 / sum(pie_dat$y2001_2019)
  pie_dat$ymax = cumsum(pie_dat$fraction)
  pie_dat$ymin = c(0, head(pie_dat$ymax, n=-1))
  total_def = paste0(round(sum(pie_dat$y2001_2019), 0), " km2")
  pie <- pie_dat %>%
    ggplot2::ggplot(aes(ymax=ymax, ymin=ymin, xmax = 4, xmin = 3, fill = to_region2)) +
    ggplot2::geom_rect() +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlim(c(2, 4)) +
    viridis::scale_fill_viridis(discrete = T, name = "Consumer regions", option = "plasma") +
    ggplot2::labs(title = sectors_sorted$to_sector_name_short[i]) +
    ggplot2::annotate('text', x = 2, y = 0.5, label = total_def, size = 5) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.title = element_text(size = 16, hjust = 0.5))
  store[[i]] <- pie
}

pies_merge <- cowplot::plot_grid(store[[1]], store[[2]], store[[3]], store[[4]], store[[5]], store[[6]],
                                 store[[7]], store[[8]], store[[9]], store[[10]], store[[11]], store[[12]],
                                 store[[13]], store[[14]], store[[15]], store[[16]], store[[17]], store[[18]],
                                 store[[19]], store[[20]], store[[21]], store[[22]], store[[23]], store[[24]],
                                 store[[25]], store[[26]], store[[27]], store[[28]], store[[29]], store[[30]],
                                 store[[31]], store[[32]], store[[33]], store[[34]], store[[35]], store[[36]],
                                 nrow = 6)
pies_merge <- cowplot::plot_grid(pies_merge, legend_a, nrow = 2, rel_heights = c(1, 0.08))

ggplot2::ggsave("figure-S12_sector_doughnut.png", 
                plot = pies_merge, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 500, height = 500, units = "mm")






