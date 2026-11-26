

library(dplyr)
library(ggplot2)
library(scales)
library(Matrix)
library(cowplot)
library(grid)
library(gridExtra)

mask <- "plantation" # "total" or "plantation", the letter will filter out tree cover loss within known plantation areas

years <- c(2001:2019)
path_parsed <- "data/parsed/"
path_results <- "data/results"
path_parsed_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-compiled/"
path_intermediate_nfs <- "/mnt/nfs_fineprint/tmp/gloria/v059-intermediate/"

# prepare labels
regions <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 1)
sectors <- readxl::read_xlsx("data/GLORIA_ReadMe_059a.xlsx", sheet = 2)
n_regions <- nrow(regions)
f_regions <- regions %>% slice(rep(1:n(), each = nrow(sectors)))
f_sectors <- sectors %>% slice(rep(1:n(), n_regions))
labels <- cbind(f_regions[, c(2,3)], f_sectors) 
n_sectors <-  length(unique(labels$Sector_names))

# based on Okabe-Ito color palette https://easystats.github.io/see/reference/scale_color_okabeito.html
commodities_palette <- c(
  "#882255",  # dark red-purple
  "#56B4E9",  # sky blue
  "#0072B2",  # blue
  "#D55E00",  # vermillion
  "#F0E442",  # yellow
  "#009E73",  # bluish green
  "#CC79A7",  # pink/magenta
  "#E69F00",  # orange
  "#661100",  # dark brown-red
  "#6699CC",  # light blue
  "#994EA3",  # purple (replacing teal)
  "grey"      # last stays grey
) 

# x-axis: forest loss intensities (Fig. 2C) -------------------------------

commodities <- c("Gold ores", "Hard coal", "Other non-ferrous ores", "Aluminium ore", "Iron ores", "Copper ores", 
                 "Nickel ores", "Lignite and peat", "Lead/zinc/silver ores", "Uranium ores", "Tin ores")

store <- list()
for(i in seq_along(years)){
  
  print(years[i])
  
  # total output
  Y <- readRDS(file.path(path_parsed_nfs, paste0("Y/Y_", years[i], ".rds")))
  Z <- readRDS(file.path(path_parsed_nfs, paste0("T/Z_", years[i], ".rds")))
  Y <- Matrix::Matrix(data.matrix(Y), sparse = TRUE,
                      dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                      paste0(rep(regions$Region_acronyms, each=6),"_",rep(1:6, n_regions))))
  Z <- Matrix::Matrix(data.matrix(Z), sparse = TRUE,
                      dimnames = list(paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr)), 
                                      paste0(labels$Region_acronyms,"_", sprintf("%03d",labels$Lfd_Nr))))
  x <- Matrix::rowSums(Z) + Matrix::rowSums(Y)
  
  # forest loss
  if(mask == "plantation"){
    load(file.path(path_parsed, paste0("e_forest_loss_price_plantation_", years[i], ".RData")))
  } else {
    load(file.path(path_parsed, paste0("e_forest_loss_price_", years[i], ".RData")))
  }
  e <- e * 10000 # m2
  
  # calculate intensities
  Q <- e/x
  Q[is.na(Q)] <- 0
  Q[Q == Inf] <- 0
  
  # add labels
  intensities_sector <- data.frame(
    country = labels %>% dplyr::select(Region_acronyms) %>% dplyr::pull(),
    sector = labels %>% dplyr::select(Sector_names) %>% dplyr::pull(),
    year = years[i],
    intensity = as.numeric(Q)
  ) %>% 
    dplyr::filter(sector %in% commodities)
  
  store[[i]] <- intensities_sector
}
intensities_sector <- do.call("rbind", store)

intensities_sector <- intensities_sector %>%
  dplyr::filter(intensity > 0)#  %>%
# dplyr::filter(intensity < 1000000000) # remove Belgium hard coal (to be checked: very high intensities for hard coal)
summary(intensities_sector$intensity)

x_dat <- intensities_sector %>%
  dplyr::group_by(country, sector) %>%
  dplyr::summarise(intensity = mean(intensity))

# aggregate EU27
EU27 <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN", "FRA", "GRC", "IRL", "ITA",
          "HRV", "LVA", "LTU", "LUX", "MLT", "NLD", "AUT", "POL", "PRT", 
          "ROU", "SWE", "SVK", "SVN", "ESP", "CZE", "HUN", "CYP")
x_dat <- x_dat %>% 
  dplyr::mutate(country = ifelse(country %in% EU27, "EU27", country))
x_dat <- x_dat %>% 
  dplyr::group_by(country, sector) %>%
  dplyr::summarise(intensity = mean(intensity)) %>%
  dplyr::ungroup()



# y-axis: global extraction (Fig. 2A) -------------------------------------

labels_sub <- labels %>% dplyr::filter(Lfd_Nr %in% c(24, 25, 28:36)) 

store <- list()
for(yr in seq_along(years)){
  
  cat("*")
  
  # load total output
  load(file.path(path_intermediate_nfs, "extensions", paste0("e_material_4cat_", years[yr], ".RData")))
  
  # reduce to metal extraction sectors (28-36) and hard coal and lignite (24 and 25)
  x1 <- e_material_4cat[substr(rownames(e_material_4cat), 6, 7) %in% c(28:36), 2]
  x2 <- e_material_4cat[substr(rownames(e_material_4cat), 6, 7) %in% c(24,25), 4]
  x <- c(x1, x2)
  x <- data.frame(id = names(x), value = x)
  
  # add labels
  extraction_mat <- x %>%
    dplyr::left_join(labels_sub %>% dplyr::mutate(id = paste0(Region_acronyms, "_0", Lfd_Nr))) %>%
    dplyr::mutate(year = years[yr])
  
  # store
  store[[yr]] <- extraction_mat
  
}

# merge into data frame
y_dat <- do.call(rbind, store)


# aggregate
y_dat <- y_dat %>%
  dplyr::group_by(Region_acronyms, Sector_names) %>%
  dplyr::summarise(value = sum(value))

# aggregate EU27
y_dat <- y_dat %>% 
  dplyr::mutate(Region_acronyms = ifelse(Region_acronyms %in% EU27, "EU27", Region_acronyms))
y_dat <- y_dat %>% 
  dplyr::group_by(Region_acronyms, Sector_names) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()

# z-axis: EU forest loss footprints (Fig. 3B) -----------------------------

# fill by commodity instead of world region
files <- dir(path_results) %>% stringr::str_subset(pattern = "^sec.*\\.csv") %>%  stringr::str_subset(pattern = paste(years, collapse = "|"))
if(mask == "plantation"){
  files <- files[stringr::str_detect(files, "plantation")]
} else {
  files <- files[!stringr::str_detect(files, "plantation")]
}

store <- list()
for(i in seq_along(files)){
  print(files[i])
  results_raw <- read.csv(file.path(path_results, files[i])) %>%
    dplyr::select(5, 3, 4, 7, 8, 6)
  store[[i]] <- results_raw
}
store <- do.call("rbind", store)

# aggregate EU27
z_dat <- store %>% 
  dplyr::mutate(to_region = ifelse(to_region %in% EU27, "EU27", to_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% EU27, "EU27", from_region))
z_dat <- z_dat %>% 
  dplyr::group_by(from_sector, from_region, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup()

# add sector labels
z_dat <- z_dat %>%
  dplyr::left_join(sectors, by = c("from_sector" = "Lfd_Nr")) %>%
  dplyr::rename("from_sector_name" = "Sector_names")

# filter for EU consumption
z_dat <- z_dat %>%
  dplyr::filter(to_region == "EU27")


# merge data --------------------------------------------------------------

x_dat <- x_dat %>% dplyr::mutate(unid = paste(country, sector, sep = "_"))
y_dat <- y_dat %>% dplyr::mutate(unid = paste(Region_acronyms, Sector_names, sep = "_")) %>% dplyr::select(unid, value)
z_dat <- z_dat %>% dplyr::mutate(unid = paste(from_region, from_sector_name, sep = "_")) %>% dplyr::select(unid, y2001_2019)

p_dat <- x_dat %>% dplyr::left_join(y_dat) %>% dplyr::left_join(z_dat)



# plot --------------------------------------------------------------------

p_dat$sector <- factor(p_dat$sector, levels = c("Aluminium ore", "Hard coal", "Lignite and peat", "Copper ores", "Gold ores", "Iron ores",
                                                "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores"))


alu <- c("AUS", "CHN", "BRA", "GIN", "IND", "IDN", "RUS", "JAM")
hco <- c("CHN", "USA", "IDN", "IND", "RUS", "AUS")
lig <- c("EU27", "GEO", "TUR", "SRB")
cop <- c("CHL", "USA", "CHN", "PER", "ZMB", "COD")
gol <- c("USA", "CHN", "AUS", "ZAF", "BRA", "RUS", "PER", "MEX", "GHA", "UZB", "IDN", "VEN", "CAN", "XAM")
iro <- c("CHN", "AUS", "BRA", "RUS", "CAN", "EU27", "IND")
lzs <- c("CHN", "PER", "MEX", "EU27", "COD", "USA", "RUS", "CAN")
nic <- c("AUS", "PHL", "IDN", "CAN", "RUS", "CHN", "EU27", "CUB", "MDG")
oth <- c("CHN", "ZAF", "USA", "CHL", "BRA", "RUS", "EU27")
tin <- c("CHN", "IDN", "BRA", "MMR", "MYS")
ura <- c("KAZ", "ZMB", "USA", "RUS", "CHN")

p_dat <- p_dat %>%
  dplyr::mutate(label2 = ifelse(sector == "Aluminium ore" & country %in% alu, country, NA)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Hard coal" & country %in% hco, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Lignite and peat" & country %in% lig, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Copper ores" & country %in% cop, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Gold ores" & country %in% gol, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Iron ores" & country %in% iro, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Lead/zinc/silver ores" & country %in% lzs, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Nickel ores" & country %in% nic, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Other non-ferrous ores" & country %in% oth, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Tin ores" & country %in% tin, country, label2)) %>%
  dplyr::mutate(label2 = ifelse(sector == "Uranium ores" & country %in% ura, country, label2))


store <- list()
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
sectors <- c("Aluminium ore", "Hard coal", "Lignite and peat", "Copper ores", "Gold ores", "Iron ores",
             "Lead/zinc/silver ores", "Nickel ores", "Other non-ferrous ores", "Tin ores", "Uranium ores")


for(i in seq_along(sectors)) {
  
  # create fake data to fill legend
  temp <- data.frame(
    country = NA,
    sector = sectors[i],
    intensity  = NA,
    unid = NA, 
    value  = NA,
    y2001_2019  = c(1000, 5000, 10000, 15000),
    label2 = NA
  )
  
  if(sectors[i] %in% c("Aluminium ore", "Lignite and peat", "Tin ores")){
    
    p_seq <- p_dat %>%
      dplyr::filter(value > 0, sector == sectors[i]) %>%
      dplyr::bind_rows(temp) %>%
      ggplot2::ggplot(aes(x = intensity, y = value / 1000000000, label = label2)) +
      ggplot2::geom_vline(xintercept = mean(p_dat$intensity[p_dat$sector == sectors[i] & p_dat$value > 0], na.rm = TRUE),
                          linetype = "dashed", colour = "grey40") +
      ggplot2::geom_hline(yintercept = mean(p_dat$value[p_dat$sector == sectors[i] & p_dat$value > 0] / 1000000000, na.rm = TRUE),
                          linetype = "dashed", colour = "grey40") +
      ggplot2::geom_point(aes(size = y2001_2019 / 1000), shape = 21, colour = "black", fill = commodities_palette[i]) + # thousand ha
      ggplot2::scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                             breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 10000, 10000)) +
      ggplot2::scale_size(breaks=c(1, 5, 10, 15)) +
      ggrepel::geom_text_repel() +
      ggplot2::labs(title = ifelse(sectors[i] == "Aluminium ore", "Bauxite", sectors[i]),
                    x = NULL,
                    y = NULL,
                    size = NULL) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     legend.position = "none")
    
  } else{
    
    p_seq <- p_dat %>%
      dplyr::filter(value > 0, sector == sectors[i]) %>%
      dplyr::bind_rows(temp) %>%
      ggplot2::ggplot(aes(x = intensity, y = value / 1000000000, label = label2)) +
      ggplot2::geom_vline(xintercept = mean(p_dat$intensity[p_dat$sector == sectors[i] & p_dat$value > 0], na.rm = TRUE),
                          linetype = "dashed", colour = "grey40") +
      ggplot2::geom_hline(yintercept = mean(p_dat$value[p_dat$sector == sectors[i] & p_dat$value > 0] / 1000000000, na.rm = TRUE),
                          linetype = "dashed", colour = "grey40") +
      ggplot2::geom_point(aes(size = y2001_2019 / 1000), shape = 21, colour = "black", fill = commodities_palette[i]) + # thousand ha
      ggplot2::scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                             breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
      ggplot2::scale_size(breaks=c(1, 5, 10, 15)) +
      ggrepel::geom_text_repel() +
      ggplot2::labs(title = ifelse(sectors[i] == "Aluminium ore", "Bauxite", sectors[i]),
                    x = NULL,
                    y = NULL,
                    size = NULL) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     legend.position = "none")
    
  }
  

  
  store[[i]] <- p_seq
  
}

p_legend <- p_dat %>%
  dplyr::filter(value > 0, sector == sectors[5]) %>%
  ggplot2::ggplot(aes(x = intensity, y = value / 1000000000, label = label2)) +
  ggplot2::geom_point(aes(size = y2001_2019 / 1000), shape = 21, colour = "black", fill = "white") + # thousand ha
  ggplot2::scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                         breaks = c(0.00000001, 0.000001, 0.0001, 0.01, 1, 100, 10000, 1000000)) +
  ggplot2::scale_size(breaks=c(1, 5, 10, 15)) +
  ggrepel::geom_text_repel() +
  ggplot2::labs(title = NULL,
                x = expression("Direct intensity (m"^2~"tree cover loss per 1,000 USD gross production)"),
                y = "2001-2019 extraction (bn tonnes)",
                size = "Tree cover loss \n(thousand ha)") +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank())
legend_dots <- g_legend(p_legend)

p_AK <- cowplot::plot_grid(store[[1]], store[[2]], store[[3]], store[[4]], 
                           store[[5]], store[[6]], store[[7]], store[[8]], 
                           store[[9]], store[[10]], store[[11]], legend_dots,
                           nrow=3,
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
                           label_size = 12)

#create common x and y labels
x_grob <- grid::textGrob(expression("Direct intensity (m"^2~"tree cover loss per 1,000 USD gross production)"), 
                         gp=gpar(fontsize=12))
y_grob <- grid::textGrob("2001-2019 extraction (bn tonnes)", 
                         gp=gpar(fontsize=12), rot=90)
p_AK <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p_AK, left = y_grob, bottom = x_grob))

if(mask == "plantation"){
  ggplot2::ggsave("figure-4_discussion.png",
                  plot = p_AK, device = "png",
                  path = paste0("./figures/"),
                  scale = 1, width = 300, height = 220, units = "mm")
} else {
  ggplot2::ggsave("figure-4_discussion-including-plantation.png",
                  plot = p_AK, device = "png",
                  path = paste0("./figures/including-plantation/"),
                  scale = 1, width = 300, height = 220, units = "mm")
  
}


