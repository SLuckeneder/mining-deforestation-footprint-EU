
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)

# read data
E <- read.csv("data/forest_loss/extension_forest_loss_price_allocation.csv")

lignite <- c("ALB", "ARM", "AUT", "BLR", "BIH", "BGR", "BDI", "CZE", "CSK", "DNK", "EST", "FIN", "GEO", "DEU", "GRC", "HUN", 
             "IRL", "ITA", "KSV", "KGZ", "LAO", "LVA", "LTU", "MNE", "MKD", "ROU", "RWA", "SRB", "YUG", "SVK", "SVN", "SWE", 
             "THA", "TUR", "UZB")
E <- E %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Lignite and peat", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Hard coal", commodity))

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

# plot single commodity by country
sectors <- c("Bauxite", "Hard coal", "Lignite and peat", "Copper ores", "Gold ores", "Iron ores",
             "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores", "Unknown")
lvls <- unique(E$country_isoa3)
store <- list()

for(i in seq_along(sectors)) {
  p_dat <- E %>%
    dplyr::group_by(year, commodity, country_isoa3) %>%
    dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
    dplyr::filter(commodity == sectors[i]) %>%
    dplyr::mutate(country_isoa3_lab = ifelse(forest_loss_km2 > 1.5*mean(forest_loss_km2), as.character(country_isoa3), NA))
  temp <- data.frame(
    year = rep(c(2001:2019), length(lvls[! lvls %in% unique(p_dat$country_isoa3)])),
    commodity = sectors[i],
    country_isoa3  = rep(lvls[! lvls %in% unique(p_dat$country_isoa3)], each = 19),
    forest_loss_km2  = 0
  )
  p_dat <- p_dat %>%
    dplyr::bind_rows(temp)
  p <- p_dat %>%
    ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = country_isoa3, label = country_isoa3_lab)) +
    ggplot2::geom_bar(stat = "identity", color = "darkgrey") +
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    ggplot2::scale_x_continuous(limits = c(2000.5, 2019.5), breaks = c(2001, 2005, 2010, 2015, 2019), expand = c(0, 0)) +
    ggplot2::geom_text(position = position_stack(vjust = 0.5), size = 2) + 
    ggplot2::labs(title = sectors[i], x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   axis.text.x = element_text(size = 14),
                   axis.text.y = element_text(size = 14),
                   legend.position = "none",
                   plot.margin = unit(c(5.5, 20.5, 5.5, 5.5), "pt"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank())
  
  store[[i]] <- p
}

p_AL <- cowplot::plot_grid(store[[1]], store[[2]], store[[3]], store[[4]], 
                           store[[5]], store[[6]], store[[7]], store[[8]], 
                           store[[9]], store[[10]], store[[11]], store[[12]],
                           nrow=4,
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                           label_size = 12)

y_grob <- grid::textGrob(expression("Forest loss (km"^2~")"), 
                         gp=gpar(fontsize=14), rot=90)
p_AL <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p_AL, left = y_grob))

ggplot2::ggsave("figure-S17_forest_loss_commodity_details.png", 
                plot = p_AL, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 350, height = 400, units = "mm")

