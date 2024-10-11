

library(dplyr)
library(ggplot2)
library(scales)
library(Matrix)

years <- c(2001:2019)
path_parsed <- "data/parsed/"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v057new/parsed"

labels <- read.csv(file.path(path_parsed_nfs, "labels.csv")) 
regions <- readxl::read_xlsx("data/GLORIA_ReadMe_057.xlsx", sheet = 1)

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

conc <- data.frame(
  sector = c("Aluminium ore", "Hard coal", "Lignite and peat", "Copper ores", "Gold ores", "Iron ores",
             "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores"),
  short = c("Bauxite", "Hard coal", "Lignite/Peat", "Copper", "Gold", "Iron ore", "Pb/Zn/Ag", "Nickel", "Other non-Fe", "Tin", "Uranium")
)



# S5 heatmap absolute forest loss -----------------------------------------

# absolute forest loss
store <- list()
for(i in seq_along(years)){
  
  # forest loss
  load(file.path(path_parsed, paste0("e_forest_loss_price_", years[i], ".RData")))
  e <- e[sel.i ,] # km2
  
  # add labels
  forest_loss_sector <- data.frame(
    country = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Region_acronyms) %>% dplyr::pull(),
    sector = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Sector_names) %>% dplyr::pull(),
    year = years[i],
    forest_loss_ha = e * 100 # hectares
  ) %>% 
    dplyr::filter(sector %in% commodities)
  
  store[[i]] <- forest_loss_sector
}
forest_loss_sector <- do.call("rbind", store)

forest_loss_sector <- forest_loss_sector %>%
  dplyr::filter(forest_loss_ha > 0) 
forest_loss_sector$country <- factor(forest_loss_sector$country, levels = rev(sort(unique(forest_loss_sector$country))))

forest_loss_sector <- forest_loss_sector %>% dplyr::left_join(conc)
forest_loss_sector$short <- factor(forest_loss_sector$short, levels = c("Bauxite", "Hard coal", "Lignite/Peat", "Copper", "Gold", "Iron ore", "Pb/Zn/Ag", "Nickel", "Other non-Fe", "Tin", "Uranium"))

# to discrete scale (quantiles)
quantiles <- quantile(forest_loss_sector$forest_loss_ha,  probs = seq(0, 1, 0.1), na.rm = TRUE)
pretty_breaks <- round(quantiles[3:length(quantiles)-1], 3)
minVal <- min(forest_loss_sector$forest_loss_ha, na.rm = T)
maxVal <- max(forest_loss_sector$forest_loss_ha, na.rm = T)
labels_x <- c()
brks <- as.numeric(c(minVal, pretty_breaks, maxVal))
for(idx in 1:length(brks)){
  labels_x <- c(labels_x,round(brks[idx + 1], 3))
}
labels_x <- labels_x[1:length(labels_x)-1]
forest_loss_sector$brks <- cut(forest_loss_sector$forest_loss_ha, 
                               breaks = brks, 
                               include.lowest = TRUE, 
                               labels = labels_x)

brks_scale <- levels(forest_loss_sector$brks)
labels_scale <- rev(brks_scale)

p <- forest_loss_sector %>% 
  ggplot2::ggplot(aes(as.character(year), country)) + 
  ggplot2::geom_tile(aes(fill = brks)) + 
  ggplot2::scale_x_discrete(breaks = c(2001, 2005, 2010, 2015, 2019)) +
  ggplot2::scale_fill_manual(
    values = viridis::rocket(10),
    breaks = rev(brks_scale),
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(220 / length(labels_x), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )) +
  ggplot2::facet_wrap(.~short, nrow = 1) +
  ggplot2::labs(x = NULL, y = NULL, fill =  expression(Forest~loss~within~mine~sites~(ha))) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.position = "bottom",
                 legend.title = element_text(size = 16)) 

ggplot2::ggsave("figure-S5_heatmap_forest_loss.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 260, height = 350, units = "mm")


# S6 heatmap forest loss per ton extracted --------------------------------

store <- list()
for(i in seq_along(years)){
  
  # forest loss
  load(file.path(path_parsed, paste0("e_forest_loss_price_", years[i], ".RData")))
  e <- e[sel.i ,] # km2
  
  # add labels
  forest_loss_sector <- data.frame(
    country = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Region_acronyms) %>% dplyr::pull(),
    sector = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Sector_names) %>% dplyr::pull(),
    year = years[i],
    forest_loss_km2 = e
  ) %>% 
    dplyr::filter(sector %in% commodities) 
  
  # load total physical production
  load(file.path(path_parsed_nfs, "extensions", paste0("e_material_4cat_", years[i], ".RData")))
  
  # reduce to industries
  E <- e_material_4cat[substr(rownames(e_material_4cat), 4, 5) == "_i",]
  
  # reduce to metal extraction sectors (28-36) and hard coal and lignite (24 and 25)
  E1 <- E[substr(rownames(E), 7, 8) %in% c(28:36), 2]
  E2 <- E[substr(rownames(E), 7, 8) %in% c(24,25), 4]
  E <- c(E1, E2)
  E <- data.frame(id = names(E), value = E)
  
  # add to forest loss data
  forest_loss_extraction_sector <- forest_loss_sector %>%
    dplyr::mutate(id = rownames(forest_loss_sector)) %>%
    dplyr::left_join(E) %>%
    dplyr::mutate(forest_loss_extraction = forest_loss_km2 * 1000000 / value) # m2 / tonne
  
  # add year
  forest_loss_extraction_sector <- forest_loss_extraction_sector %>%
    dplyr::mutate(year = years[i])
  
  # store
  store[[i]] <- forest_loss_extraction_sector
  
}
# merge into data frame
forest_loss_extraction_sector <- do.call(rbind, store)

# remove Inf caused by positive forest loss and zero extraction mismatch (div by 0) and NaN (no forest loss and no extraction)
forest_loss_extraction_sector <- forest_loss_extraction_sector %>%
  dplyr::filter(forest_loss_extraction > 0) %>%
  dplyr::filter(is.finite(forest_loss_extraction))
forest_loss_extraction_sector$country <- factor(forest_loss_extraction_sector$country, levels = rev(sort(unique(forest_loss_extraction_sector$country))))

forest_loss_extraction_sector <- forest_loss_extraction_sector %>% dplyr::left_join(conc)
forest_loss_extraction_sector$short <- factor(forest_loss_extraction_sector$short, levels = c("Bauxite", "Hard coal", "Lignite/Peat", "Copper", "Gold", "Iron ore", "Pb/Zn/Ag", "Nickel", "Other non-Fe", "Tin", "Uranium"))

# to discrete scale (quantiles)
quantiles <- quantile(forest_loss_extraction_sector$forest_loss_extraction,  probs = seq(0, 1, 0.1), na.rm = TRUE)
pretty_breaks <- round(quantiles[3:length(quantiles)-1], 4)
minVal <- min(forest_loss_extraction_sector$forest_loss_extraction, na.rm = T)
maxVal <- max(forest_loss_extraction_sector$forest_loss_extraction, na.rm = T)
labels_x <- c()
brks <- as.numeric(c(minVal, pretty_breaks, maxVal))
for(idx in 1:length(brks)){
  labels_x <- c(labels_x,round(brks[idx + 1], 4))
}
labels_x <- labels_x[1:length(labels_x)-1]
labels_x[10] <- "large outliers"
forest_loss_extraction_sector$brks <- cut(forest_loss_extraction_sector$forest_loss_extraction, 
                                          breaks = brks, 
                                          include.lowest = TRUE, 
                                          labels = labels_x)

brks_scale <- levels(forest_loss_extraction_sector$brks)
labels_scale <- rev(brks_scale)

p <- forest_loss_extraction_sector %>% 
  ggplot2::ggplot(aes(as.character(year), country)) + 
  ggplot2::geom_tile(aes(fill = brks)) +
  ggplot2::scale_x_discrete(breaks = c(2001, 2005, 2010, 2015, 2019)) +
  ggplot2::scale_fill_manual(
    values = viridis::cividis(10),
    breaks = rev(brks_scale),
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(220 / length(labels_x), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )) +
  ggplot2::facet_wrap(.~short, nrow = 1) +
  ggplot2::labs(x = NULL, y = NULL, fill =  expression(Forest~loss~within~mine~sites~(m^2~per~tonne~of~extracted~material))) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.position = "bottom",
                 legend.title = element_text(size = 16)) 

ggplot2::ggsave("figure-S6_heatmap_forest_loss_per_tonne.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 260, height = 350, units = "mm")


# S7 heatmap total output -------------------------------------------------

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

# total output extractive industries in Mio USD
store <- list()
for(i in seq_along(years)){
  
  # total output
  load(file.path(path_parsed_nfs, paste0("x_", years[i], ".RData")))
  x <- x[sel.i] / 1000000 # Bn. USD (GLORIA comes in thousand)
  
  # add labels
  total_output_sector <- data.frame(
    country = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Region_acronyms) %>% dplyr::pull(),
    sector = labels %>% dplyr::filter(type == "i") %>% dplyr::select(Sector_names) %>% dplyr::pull(),
    year = years[i],
    output = x
  ) %>% 
    dplyr::filter(sector %in% commodities)
  
  store[[i]] <- total_output_sector
}
total_output_sector <- do.call("rbind", store)

total_output_sector <- total_output_sector %>%
  dplyr::filter(output > 0) %>%
  dplyr::filter(country %in% unique(intensities_sector$country))
total_output_sector$country <- factor(total_output_sector$country, levels = rev(sort(unique(total_output_sector$country))))

total_output_sector <- total_output_sector %>% dplyr::left_join(conc)
total_output_sector$short <- factor(total_output_sector$short, levels = c("Bauxite", "Hard coal", "Lignite/Peat", "Copper", "Gold", "Iron ore", "Pb/Zn/Ag", "Nickel", "Other non-Fe", "Tin", "Uranium"))


p <- total_output_sector %>% 
  ggplot2::ggplot(aes(year, country)) + 
  ggplot2::geom_tile(aes(fill = output)) + 
  viridis::scale_fill_viridis(option = "mako", direction = -1) +
  ggplot2::facet_wrap(.~short, nrow = 1) +
  ggplot2::labs(x = NULL, y = NULL, fill = "Total output (billion USD)") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.position = "bottom",
                 legend.title = element_text(size = 16)) +
  ggplot2::guides(fill = guide_colorbar(barheight = unit(5, 
                                                         units = "mm"), barwidth = unit(200, units = "mm"), 
                                        title.position = "top", title.hjust = 0.5, label.hjust = 0.5))

ggplot2::ggsave("figure-S7_heatmap_total_output.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 260, height = 350, units = "mm")

# S8 heatmap intensities sector and year ----------------------------------

intensities_sector <- intensities_sector %>%
  dplyr::filter(intensity > 0) 
intensities_sector$country <- factor(intensities_sector$country, levels = rev(sort(unique(intensities_sector$country))))

intensities_sector <- intensities_sector %>% dplyr::left_join(conc)
intensities_sector$short <- factor(intensities_sector$short, levels = c("Bauxite", "Hard coal", "Lignite/Peat", "Copper", "Gold", "Iron ore", "Pb/Zn/Ag", "Nickel", "Other non-Fe", "Tin", "Uranium"))

# to discrete scale (quantiles)
quantiles <- quantile(intensities_sector$intensity,  probs = seq(0, 1, 0.1), na.rm = TRUE)
pretty_breaks <- round(quantiles[3:length(quantiles)-1], 3)
minVal <- min(intensities_sector$intensity, na.rm = T)
maxVal <- max(intensities_sector$intensity, na.rm = T)
labels_x <- c()
brks <- as.numeric(c(minVal, pretty_breaks, maxVal))
for(idx in 1:length(brks)){
  labels_x <- c(labels_x,round(brks[idx + 1], 3))
}
labels_x <- labels_x[1:length(labels_x)-1]
labels_x[10] <- "large outliers"
intensities_sector$brks <- cut(intensities_sector$intensity, 
                               breaks = brks, 
                               include.lowest = TRUE, 
                               labels = labels_x)

brks_scale <- levels(intensities_sector$brks)
labels_scale <- rev(brks_scale)

p <- intensities_sector %>% 
  ggplot2::ggplot(aes(as.character(year), country)) + 
  ggplot2::geom_tile(aes(fill = brks)) + 
  ggplot2::scale_x_discrete(breaks = c(2001, 2005, 2010, 2015, 2019)) +
  ggplot2::scale_fill_manual(
    values = viridis::viridis(10),
    breaks = rev(brks_scale),
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(220 / length(labels_x), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )) +  ggplot2::facet_wrap(.~short, nrow = 1) +
  ggplot2::labs(x = NULL, y = NULL, fill = expression(Direct~intensity~(m^2~"/ 1,000"~USD))) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.position = "bottom",
                 legend.title = element_text(size = 16)) 

ggplot2::ggsave("figure-S8_heatmap_direct_intensities.png",
                plot = p, device = "png",
                path = paste0("./figures"),
                scale = 1, width = 260, height = 350, units = "mm")


