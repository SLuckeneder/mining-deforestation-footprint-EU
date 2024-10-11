

library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)
library(Matrix)
library(grid)
library(gridExtra)

years <- c(2001:2019)
path_parsed <- "data/parsed/"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"

labels <- read.csv(file.path(path_parsed_nfs, "labels.csv")) 
regions <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 1)


# A domestic extraction ---------------------------------------------------

labels_sub <- labels %>% dplyr::filter(type == "i", Lfd_Nr %in% c(24, 25, 28:36)) 

store <- list()
for(yr in seq_along(years)){
  
  cat("*")
  
  # load total output
  load(file.path(path_parsed_nfs, "extensions", paste0("e_material_4cat_", years[yr], ".RData")))
  
  # reduce to industries
  x <- e_material_4cat[substr(rownames(e_material_4cat), 4, 5) == "_i",]
  
  # reduce to metal extraction sectors (28-36) and hard coal and lignite (24 and 25)
  x1 <- x[substr(rownames(x), 7, 8) %in% c(28:36), 2]
  x2 <- x[substr(rownames(x), 7, 8) %in% c(24,25), 4]
  x <- c(x1, x2)
  x <- data.frame(id = names(x), value = x)
  
  # add labels
  extraction_mat <- x %>%
    dplyr::left_join(labels_sub %>% dplyr::mutate(id = paste0(Region_acronyms, "_i0", Lfd_Nr))) %>%
    dplyr::mutate(year = years[yr])
  
  # store
  store[[yr]] <- extraction_mat
  
}

# merge into data frame
extraction_mat <- do.call(rbind, store)

# rename coal
extraction_mat <- extraction_mat %>%
  dplyr::mutate(Sector_names  = ifelse(Sector_names == "Hard coal", "Coal (hard coal)", Sector_names)) %>%
  dplyr::mutate(Sector_names  = ifelse(Sector_names == "Lignite and peat", "Coal (lignite and peat)", Sector_names))
  
  # plot total
p_extraction <- extraction_mat %>% 
  dplyr::group_by(year, Sector_names) %>%
  dplyr::summarise(value = sum(value)) %>%
  ggplot2::ggplot(aes(x = year, y = value / 1000000000, fill = Sector_names)) +
  ggplot2::geom_area() +
  ggplot2::scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  ggplot2::scale_x_continuous(limits = c(2001, 2019), breaks = c(2001, 2005, 2010, 2015, 2019), expand = c(0, 0)) +
  ggplot2::scale_fill_manual(name = NULL, values =  c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)])) +
  ggplot2::labs(title = "Global extraction", x = NULL, y = "Bn tonnes") +
  ggplot2::theme_bw() +
  ggplot2::theme(title = element_text(size = 16),
                 axis.text.x = element_text(size = 14, vjust = -0.3),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.margin = unit(c(5.5, 15.5, 5.5, 5.5), "pt"),
                 legend.text = element_text(size = 14),
                 plot.background = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank())


# B forest loss -----------------------------------------------------------

E <- read.csv(file = "data/forest_loss/extension_forest_loss_price_allocation.csv")

# coal by main source of coal
lignite <- c("ALB", "ARM", "AUT", "BLR", "BIH", "BGR", "BDI", "CZE", "CSK", "DNK", "EST", "FIN", "GEO", "DEU", "GRC", "HUN", 
             "IRL", "ITA", "KSV", "KGZ", "LAO", "LVA", "LTU", "MNE", "MKD", "ROU", "RWA", "SRB", "YUG", "SVK", "SVN", "SWE", 
             "THA", "TUR", "UZB")
E <- E %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & country_isoa3 %in% lignite, "Coal (lignite and peat)", commodity)) %>%
  dplyr::mutate(commodity = ifelse(commodity == "Coal" & ! country_isoa3 %in% lignite, "Coal (hard coal)", commodity))

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

p_forest_loss <- E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = commodity)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(limits = c(0, 1300), expand = c(0, 0), label = scales::comma) +
  ggplot2::scale_x_continuous(limits = c(2000.5, 2019.5), breaks = c(2001, 2005, 2010, 2015, 2019) , expand = c(0, 0)) +
  ggplot2::labs(title = "Forest loss within mining areas", x = NULL, y = expression(Km^2)) +
  ggplot2::scale_fill_manual(name = NULL, values =  c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)], "grey")) + 
  ggplot2::theme_bw() +
  ggplot2::theme(title = element_text(size = 16),
                 axis.text.x = element_text(size = 14, vjust = -0.3),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 plot.background = element_blank(),
                 legend.background = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank())

# export single chart for Figure S1
ggplot2::ggsave("figure-S1_forest_loss.png",
                plot = p_forest_loss + ggplot2::labs(title = NULL, y = NULL), device = "png",
                path = paste0("./figures/"),
                scale = 1, width = 250, height = 150, units = "mm")

### Figure S3 line charts
E %>%
  dplyr::group_by(year, commodity) %>%
  dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
  ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = commodity)) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::geom_point(shape = 15, size = 3) +
  ggplot2::facet_wrap(~commodity, scales = "free")
store <- list()
colpalette <- c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)], "grey")
sectors <- c("Bauxite", "Coal (hard coal)", "Coal (lignite and peat)", "Copper ores", "Gold ores", "Iron ores",
             "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores", "Unknown")

for(i in seq_along(sectors)) {

  p_seq <- E %>%
    dplyr::group_by(year, commodity) %>%
    dplyr::summarise(forest_loss_km2 = sum(forest_loss_km2)) %>%
    dplyr::filter(commodity == sectors[i]) %>%
    ggplot2::ggplot(aes(x = year, y = forest_loss_km2, fill = commodity)) +
    ggplot2::geom_line(size = 0.8, colour = colpalette[i]) +
    ggplot2::geom_point(shape = 15, size = 3, colour = colpalette[i]) +
    ggplot2::expand_limits(y = 0) + 
    scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
    ggplot2::scale_x_continuous(limits = c(2001, 2019), breaks = c(2001, 2005, 2010, 2015, 2019) , expand = c(0, 0)) +
    ggplot2::labs(title = sectors[i],
                  x = NULL,
                  y = NULL,
                  size = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   plot.margin = unit(c(5.5, 20.5, 5.5, 5.5), "pt"),
                   plot.title = element_text(hjust = 0.5),
                   legend.position = "none")
  
  store[[i]] <- p_seq
  
}

p_AK <- cowplot::plot_grid(store[[1]], store[[2]], store[[3]], store[[4]], 
                           store[[5]], store[[6]], store[[7]], store[[8]], 
                           store[[9]], store[[10]], store[[11]], store[[12]],
                           nrow=3,
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                           label_size = 12)
y_grob <- grid::textGrob(expression("Forest loss (km"^2~")"), 
                         gp=gpar(fontsize=12), rot=90)
p_AK <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p_AK, left = y_grob))

ggplot2::ggsave("figure-S3_forest_loss_lines.png",
                plot = p_AK, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 300, height = 220, units = "mm")

### Figure S4 doughnut
# aggregating amounts for all materials per country and year
aggregated_data <- E %>%
  dplyr::group_by(country_isoa3, commodity) %>%
  dplyr::summarise(total_value = sum(forest_loss_km2), .groups = 'drop')

# ranking all amounts
ranked_data <- aggregated_data %>%
  dplyr::group_by(commodity) %>%
  dplyr::arrange(desc(total_value)) %>%
  dplyr::mutate(rank = row_number()) %>%
  dplyr::ungroup()

# adjust the ranks
top_countries <- ranked_data %>%
  dplyr::group_by(commodity) %>%
  dplyr::filter(
    (commodity %in% c("Coal (hard coal)", "Gold ores") & rank <= 6) |
      (commodity %in% c("Coal (lignite and peat)", "Lead/zinc/silver ores", "Tin ores", "Uranium ores") & rank <= 1) |
      (! commodity %in% c("Coal (hard coal)", "Gold ores", "Coal (lignite and peat)", "Lead/zinc/silver ores", "Tin ores", "Uranium ores") & rank <= 2)
  ) %>%
  dplyr::ungroup()
rest_values <- ranked_data %>%
  group_by(commodity) %>%
  dplyr::filter(
    (commodity %in% c("Coal (hard coal)", "Gold ores") & rank > 6) |
      (commodity %in% c("Coal (lignite and peat)", "Lead/zinc/silver ores", "Tin ores", "Uranium ores") & rank > 1) |
      (! commodity %in% c("Coal (hard coal)", "Gold ores", "Coal (lignite and peat)", "Lead/zinc/silver ores", "Tin ores", "Uranium ores") & rank > 2)
  ) %>%
  summarise(total_value = sum(total_value, na.rm = TRUE), .groups = 'drop') %>%
  mutate(country_isoa3 = paste("Rest", commodity)) %>%
  dplyr::select(commodity, country_isoa3, total_value)

sunburst_data <- top_countries %>% bind_rows(rest_values) %>%
  dplyr::mutate(type = "country")

commodity_group_totals <- E %>%
  dplyr::group_by(commodity) %>%
  dplyr::summarise(total_value = sum(forest_loss_km2)) %>%
  dplyr::mutate(type = "commodity group",
                country_isoa3 = NA) 

commodity_group_totals <- commodity_group_totals %>%
  mutate(percentage = (total_value / sum(total_value)) * 100)

sunburst_data <- sunburst_data %>%
  mutate(percentage = (total_value / sum(total_value)) * 100)

sunburst_data <- bind_rows(commodity_group_totals, sunburst_data)

sunburst_data <- sunburst_data %>%
  dplyr::mutate(type = factor(type, levels = c("commodity group", "country")))

sunburst_data <- sunburst_data %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Bauxite" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Coal (hard coal)" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Coal (lignite and peat)" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Copper ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Gold ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Iron ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Lead/zinc/silver ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Nickel ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Other non-ferrous ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Tin ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Uranium ores" & type == "country", "ROW", country_isoa3)) %>%
  dplyr::mutate(country_isoa3 = ifelse(country_isoa3 == "Rest Unknown" & type == "country", "ROW", country_isoa3))

sunburst_data <- sunburst_data %>%
  dplyr::arrange(desc(commodity), type, desc(total_value), country_isoa3)

# ordering the factor levels for plotting
sunburst_data$commodity <- factor(sunburst_data$commodity, levels = unique(sunburst_data$commodity))

p <- sunburst_data %>% 
  dplyr::filter(type == "country") %>%
  ggplot2::ggplot(aes(x = type, y = percentage, fill = commodity, order = -total_value)) + 
  ggplot2::geom_col(color = "black", linewidth = 0.2) +
  ggplot2::coord_polar("y") +
  ggplot2::scale_fill_manual(name = NULL, values =  rev(c(RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(1:2)], "#0d218c", RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3:10)], "grey"))) + 
  ggrepel::geom_label_repel(aes(label = country_isoa3), position = position_stack(vjust = 0.5), size = 8, show.legend = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = element_text(size = 20),
                 legend.box.spacing = unit(-1, 'cm'),
                 plot.margin = unit(c(-1.5, -1.5, -1.5, -1.5),"cm")) +
  ggplot2::guides(fill = guide_legend(reverse=T)) +
  ggplot2::annotate("text", x = 0, y = 0, label = " ", size = 6, fontface = "bold", color = "black")

ggplot2::ggsave("figure-S4_forest_loss_doughnut.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 350, height = 250, units = "mm")
  
# C intensities -----------------------------------------------------------

nreg <- length(unique(labels$Region_acronyms))
nsec <-  length(unique(labels$Sector_names))
tmp_raw <- data.frame("index" = 1:(nreg * nsec * 2),
                      "entity_code" = rep( 1:2, each = nsec ))
labels_temp <- list("T" = tmp_raw)
indices <- list("ind" = labels_temp$T %>% filter(entity_code == 1) %>% pull(index),
                "pro" = labels_temp$T %>% filter(entity_code == 2) %>% pull(index) )

sel_tab <- data.frame(id = c(1:(length(indices$ind)*2)),
                      sel.i = rep(c(rep(1, 120), rep(0, 120)), length(unique(labels$Region_acronyms))),
                      sel.p = rep(c(rep(0, 120), rep(1, 120)), length(unique(labels$Region_acronyms)))) %>% 
  dplyr::mutate(sub.i = id * sel.i, sub.p = id * sel.p)
sel.i <- sel_tab  %>% dplyr::filter(sub.i > 0) %>% dplyr::pull(sub.i)

commodities <- c("Gold ores", "Hard coal", "Other non-ferrous ores", "Aluminium ore", "Iron ores", "Copper ores", 
                 "Nickel ores", "Lignite and peat", "Lead/zinc/silver ores", "Uranium ores", "Tin ores")


store <- list()
for(i in seq_along(years)){
  
  # total output
  load(file.path(path_parsed_nfs, paste0("x_", years[i], ".RData")))
  x <- x[sel.i]
  
  # forest loss
  load(file.path(path_parsed, paste0("e_forest_loss_price_", years[i], ".RData")))
  e <- e[sel.i ,]
  e <- e * 1000000 # m2
  
  # calculate intensities
  Q <- e/x
  Q[is.na(Q)] <- 0
  Q[Q == Inf] <- 0
  
  # add labels
  intensities_sector <- data.frame(
    country = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Region_acronyms) %>% dplyr::pull(),
    sector = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Sector_names) %>% dplyr::pull(),
    year = years[i],
    intensity = Q
  ) %>% 
    dplyr::filter(sector %in% commodities)
  
  store[[i]] <- intensities_sector
}
intensities_sector <- do.call("rbind", store)


intensities_sector <- intensities_sector %>%
  dplyr::filter(intensity > 0) %>%
  dplyr::filter(intensity < 1000000000) # remove Belgium hard coal (to be checked: very high intensities for hard coal)
summary(intensities_sector$intensity)


maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}

p_dat <- intensities_sector %>%
  dplyr::mutate(sector = ifelse(sector == "Aluminium ore", "Bauxite", sector)) %>%
  dplyr::group_by(country, sector) %>%
  dplyr::summarise(intensity = mean(intensity)) %>%
  dplyr::group_by(sector) %>%
  dplyr::mutate(clab = ifelse(intensity >= maxN(intensity, N = 3), country, NA)) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::mutate(xlab = paste0(sector, " (n = ", n, ")"))


p_dat$sector <- factor(p_dat$sector, levels = rev(c("Bauxite", "Hard coal", "Lignite and peat", "Copper ores", "Gold ores", "Iron ores",
                                                    "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores")))

p_dat <- p_dat %>% 
  dplyr::mutate(clab_manual = NA) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Uranium ores" & country %in% c("ZMB", "GAB", "AUS", "RUS", "CHN", "CAN"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Tin ores" & country %in% c("BRA", "MYS", "CHN", "IDN"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Nickel ores" & country %in% c("IDN", "AUS", "RUS", "GHA", "CUB"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Lead/zinc/silver ores" & country %in% c("VEN", "GIN", "CHN", "PER", "AUS"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Iron ores" & country %in% c("GIN", "BRA", "AUS"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Gold ores" & country %in% c("VEN", "XAM", "PRY", "CHN", "AUS", "RUS", "CAN", "USA", "UZB"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Copper ores" & country %in% c("VEN", "AGO", "CHL", "CHN", "COD", "PER", "USA"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Lignite and peat" & country %in% c("DEU", "TUR", "CZE"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Hard coal" & country %in% c("MMR", "IDN", "IND", "CHN", "USA", "ZAF"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Bauxite" & country %in% c("AUS", "CHN", "GIN", "IDN", "LAO", "IND"), country, clab_manual)) %>%
  dplyr::mutate(clab_manual = ifelse(sector == "Other non-ferrous ores" & country %in% c("GIN", "RUS", "ZAF", "CHL", "USA"), country, clab_manual))


# top 90% & 50% producers per commodity
top_producers <- extraction_mat %>% 
  dplyr::mutate(Sector_names = ifelse(Sector_names == "Aluminium ore", "Bauxite", Sector_names)) %>%
  dplyr::group_by(Region_acronyms, Sector_names) %>%
  dplyr::summarise(value = sum(value))  %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Sector_names) %>%
  dplyr::mutate(total = sum(value)) %>%
  dplyr::mutate(share = value / total) %>%
  dplyr::arrange(-share) %>%
  dplyr::mutate(cumsum = cumsum(share)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(top_group = "none") %>%
  dplyr::mutate(top_group = ifelse(cumsum < 0.8, "Top 80%", top_group)) %>%
  dplyr::mutate(unid = paste0(Region_acronyms, "_", Sector_names)) %>%
  dplyr::select(unid, top_group)

p_dat <- p_dat %>%
  dplyr::mutate(sector  = ifelse(sector == "Hard coal", "Coal (hard coal)", as.character(sector))) %>%
  dplyr::mutate(sector  = ifelse(sector == "Lignite and peat", "Coal (lignite and peat)", as.character(sector))) %>%
  dplyr::mutate(unid = paste0(country, "_", sector)) %>%
  dplyr::left_join(top_producers)
  
median_ranking <- p_dat %>%
  dplyr::group_by(sector) %>%
  dplyr::summarise(median = median(intensity)) %>%
  dplyr::arrange(-median)

p_dat$sector <- factor(p_dat$sector, levels = rev(median_ranking$sector))

p_intensities <- p_dat %>%
  ggplot2::ggplot(aes(x = sector, y = intensity, fill = sector)) +
  ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  ggplot2::geom_point(aes(colour =  top_group), size = 2, alpha = 0.5) +
  ggplot2::scale_y_log10(limits = c(0.00000001, 1000000),
                         breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000),
                         labels = trans_format("log10", math_format(10^.x)),
                         expand = c(0, 0)) +
  ggplot2::scale_x_discrete(breaks = p_dat$sector, labels = p_dat$xlab) +
  ggplot2::scale_fill_manual(name = NULL,
                             values = c("Bauxite" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[1],
                                        "Coal (hard coal)" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[2],
                                        "Coal (lignite and peat)" = "#0d218c",
                                        "Copper ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[3],
                                        "Gold ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[4],
                                        "Iron ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[5],
                                        "Lead/zinc/silver ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[6],
                                        "Nickel ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[7],
                                        "Other non-ferrous ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[8],
                                        "Tin ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[9],
                                        "Uranium ores" = RColorBrewer::brewer.pal(n = 10, name = "Paired")[10])) +
  ggplot2::scale_colour_manual(values = c("Top 80%" = "red", "none" = "black")) +
  ggplot2::labs(x = NULL, y = expression("Direct intensity (m"^2~"forest loss per 1,000 USD gross production)")) +
  ggplot2::geom_text(aes(label = clab_manual), size = 3, nudge_x = 0.25) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme_bw() +
  ggplot2::theme(title = element_text(size = 20),
                 axis.text.x = element_text(size = 20),
                 axis.title.x = element_text(size = 20),
                 axis.text.y = element_text(size = 20),
                 axis.title.y = element_text(size = 20),
                 legend.position = "none",
                 panel.grid = element_blank(),
                 plot.background = element_blank(),
                 plot.margin = unit(c(5.5, 15.5, 5.5, 5.5), "pt"))

# save single plot
ggplot2::ggsave("figure-2C_intensities.png",
                plot = p_intensities, device = "png",
                path = paste0("./figures/presentations/"),
                scale = 1, width = 400, height = 150, units = "mm")


# merge -------------------------------------------------------------------

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
legend_b <- g_legend(p_forest_loss)

p_AB <- cowplot::plot_grid(p_extraction + theme(legend.position="none"), 
                           p_forest_loss + theme(legend.position="none"), nrow=1, labels = c("A", "B"), label_size = 16)
p_AB <- cowplot::plot_grid(p_AB, legend_b, nrow=1, rel_widths = c(0.8, 0.2))

p <- cowplot::plot_grid(p_AB, p_intensities, nrow = 2, rel_heights = c(0.5, 0.5), labels = c("", "C"), label_size = 16)

ggplot2::ggsave("figure-2_finding-1.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 400, height = 300, units = "mm")


# numbers for manuscript text ---------------------------------------------

# mean intensities and sd by commodity
p_dat %>% dplyr::group_by(sector) %>%
  dplyr::summarise(median = median(intensity), mean = mean(intensity), sd = sd(intensity)) %>%
  dplyr::arrange(-median)
p_dat %>% dplyr::filter(country %in% c("AUS", "CHN", "GIN"), sector == "Bauxite")
p_dat %>% dplyr::filter(country %in% c("CHN", "PER", "CHL"), sector == "Copper ores")

# gold VEN
intensities_sector %>% 
  dplyr::filter(country == "VEN", sector == "Gold ores") %>%
  dplyr::arrange(-intensity)

# gold VEN
intensities_sector %>% 
  dplyr::filter(country == "VEN", sector == "Lead/zinc/silver ores") %>%
  dplyr::arrange(-intensity)

# iron ore GIN
intensities_sector %>% 
  dplyr::filter(country == "GIN", sector == "Iron ores") %>%
  dplyr::arrange(-intensity)





