
library(dplyr)
library(sf)
library(raster)
library(stars)
library(ggplot2)
library(grid)
library(basemaps)

set_defaults(map_service = "esri", map_type = "world_imagery")

# read extension
E <- read.csv(file = "data/forest_loss/extension_forest_loss_price_allocation.csv")

# raw cluster data
data_cluster_forest_loss <- read.csv("data/parsed/cluster_forest_loss.csv") %>% 
  dplyr::rename(def_total = total) %>%
  dplyr::filter(!is.na(def_total) & def_total > 0, year %in% c(2001:2019))

# processed cluster data
data_cluster <- read.csv("data/forest_loss/cluster_forest_loss_price_allocation_full.csv")

# read mining  polygons
cluster_sf <- sf::st_read("data/forest_loss/mine_polygons_hcluster.gpkg")

# map ---------------------------------------------------------------------

data_cluster_agg <- data_cluster %>%
  dplyr::group_by(hcluster_id) %>%
  dplyr::summarise(def_total = sum(def_price_weight, na.rm = TRUE)) 

cluster_sf <- cluster_sf %>%
  dplyr::mutate(hcluster_id = as.integer(hcluster_id)) %>% 
  dplyr::full_join(data_cluster_agg)

# multipolygon to point coordinates
cluster_sf_point <- sf::st_cast(cluster_sf, "POINT") %>%
  dplyr::group_by(hcluster_id) %>%
  dplyr::slice(1)
sum(cluster_sf$def_total, na.rm = TRUE) == sum(cluster_sf_point$def_total, na.rm = TRUE)

# Aggregate forest loss to grid
degrees <- 2
r <- raster::raster(ncol = 360/degrees, nrow = 180/degrees) %>% 
  raster::projectRaster(crs = as.character(sf::st_crs(cluster_sf_point))[2])
create_grid_cell <- function(sf_tbl, r){
  
  # Get grid parameters from raster 
  dx <- res(r)[1]
  dy <- res(r)[2]
  
  # Get points coordinates 
  pto_coord <- sf_tbl %>% 
    sf::st_coordinates() %>% 
    tibble::as_tibble() 
  
  create_grid_pol <- function(x, y, dx, dy, .pb = NULL){
    
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    
    sf::st_polygon(list(rbind(c(x     , y     ), 
                              c(x     , y + dy),
                              c(x + dx, y + dy),
                              c(x + dx, y     ),
                              c(x     , y     ))))
    
  }
  
  # Compute grid col and row (Integer division) and grid coordinates 
  cat("\nCreating grid from polygons")
  grid_cells <- pto_coord %>% 
    dplyr::transmute(nx = X%/%dx, ny = Y%/%dy) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(gx = nx*dx, gy = ny*dy) %>% 
    dplyr::mutate(pol = purrr::map2(.x = .$gx, .y = .$gy, .f = create_grid_pol, dx = dx, dy = dy, .pb = dplyr::progress_estimated(length(.$gx)))) 
  
  res <- sf::st_sfc(grid_cells$pol, crs = sf::st_crs(sf_tbl)) %>% 
    sf::st_sf()
  
  return(res)
  
}
grid_cells <- create_grid_cell(cluster_sf_point, r)
# plot(grid_cells)

res <- cluster_sf_point %>% 
  dplyr::select(def_total) %>% 
  stats::aggregate(by = grid_cells, FUN = sum, na.rm = TRUE, join = sf::st_intersects)
sum(res$def_total, na.rm = T) == sum(data_cluster_forest_loss$def_total) # almost, rounding issue
# plot(res)

# draw a nice map
world_robin <- spData::world %>% dplyr::select(area_km2) %>% 
  sf::st_transform(crs = sf::st_crs("+proj=robin"))
res <- res %>% 
  sf::st_transform(crs = sf::st_crs(world_robin))
zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Robinson World", c(-11631775, 14842336), c(-5839280, 8459648),
  "box_BRA", c(-59,-56), c(-8, -4.5),
  "box_GHA", c(-3,-1), c(4.5, 7), 
  "box_IDN", c(112, 118), c(-5, 1)) %>% 
  dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                group = 1,
                geometry = lapply(geometry, sf::st_bbox),
                geometry = lapply(geometry, sf::st_as_sfc),
                geometry = lapply(geometry, sf::st_geometrycollection),
                geometry = sf::st_sfc(geometry)) %>% 
  sf::st_sf() %>% 
  sf::st_collection_extract()
lim <- zoom_bbox %>% 
  dplyr::filter(region == "Robinson World")


### zoom BRA
box_BRA <- zoom_bbox %>% dplyr::filter(region == "box_BRA")
sf::st_crs(box_BRA) <- sf::st_crs(cluster_sf_point)
BRA_sf_point <-sf::st_intersection(cluster_sf_point, box_BRA)
BRA_sf_poly <- cluster_sf %>% dplyr::filter(hcluster_id %in% BRA_sf_point$hcluster_id)
sum(BRA_sf_poly$def_total)
plot(BRA_sf_poly %>% dplyr::select(country_name))
# plot(BRA_sf_poly %>% dplyr::summarise(def_total = sum(def_total)))
satellite_basemap <- basemaps::basemap_raster(BRA_sf_poly)
crop_transform <- BRA_sf_poly %>% sf::st_transform(st_crs(satellite_basemap))
p_BRA <- basemaps::basemap_ggplot(ext = box_BRA) +
  ggplot2::geom_sf(data = crop_transform, fill = "white", colour = NA) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_sf(expand = FALSE, label_axes = "E--N", crs = st_crs(satellite_basemap)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = element_text(size = 5)) +
  ggspatial::annotation_scale(bar_cols = "white", text_col = "white")

### zoom GHA
box_GHA <- zoom_bbox %>% dplyr::filter(region == "box_GHA")
sf::st_crs(box_GHA) <- sf::st_crs(cluster_sf_point)
GHA_sf_point <-sf::st_intersection(cluster_sf_point, box_GHA)
GHA_sf_poly <- cluster_sf %>% dplyr::filter(hcluster_id %in% GHA_sf_point$hcluster_id)
sum(GHA_sf_poly$def_total)
# plot(GHA_sf_poly %>% dplyr::summarise(def_total = sum(def_total)))
satellite_basemap <- basemaps::basemap_raster(GHA_sf_poly)
crop_transform <- GHA_sf_poly %>% sf::st_transform(st_crs(satellite_basemap))
p_GHA <- basemaps::basemap_ggplot(ext = box_GHA) +
  ggplot2::geom_sf(data = crop_transform, fill = "white", colour = NA) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_sf(expand = FALSE, label_axes = "E--N") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = element_text(size = 5)) +
  ggspatial::annotation_scale(bar_cols = "white", text_col = "white")

### zoom IDN
box_IDN <- zoom_bbox %>% dplyr::filter(region == "box_IDN")
sf::st_crs(box_IDN) <- sf::st_crs(cluster_sf_point)
IDN_sf_point <-sf::st_intersection(cluster_sf_point, box_IDN)
IDN_sf_poly <- cluster_sf %>% dplyr::filter(hcluster_id %in% IDN_sf_point$hcluster_id)
sum(IDN_sf_poly$def_total)
# plot(IDN_sf_poly %>% dplyr::summarise(def_total = sum(def_total)))
satellite_basemap <- basemaps::basemap_raster(IDN_sf_poly)
crop_transform <- IDN_sf_poly %>% sf::st_transform(st_crs(satellite_basemap))
p_IDN <- basemaps::basemap_ggplot(ext = box_IDN) +
  ggplot2::geom_sf(data = crop_transform, fill = "white", colour = NA) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_sf(expand = FALSE, label_axes = "E--N") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = element_text(size = 5)) +
  ggspatial::annotation_scale(bar_cols = "white", text_col = "white")



# combine plots
box_BRA <- box_BRA %>% sf::st_transform(st_crs("+proj=robin +lon_0=-180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
box_GHA <- box_GHA %>% sf::st_transform(st_crs("+proj=robin +lon_0=-180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
box_IDN <- box_IDN %>% sf::st_transform(st_crs("+proj=robin +lon_0=-180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

p <- res %>% ggplot2::ggplot(aes(fill = def_total * 100 / 1000)) + # in thousand hectartes instead of km2
  ggplot2::geom_sf(data = world_robin, fill = "#FAFAFA") + 
  ggplot2::geom_sf(alpha = 0.65) + 
  ggplot2::geom_sf(data = box_BRA , fill = NA, colour = "black", lwd = 0.3) +
  ggplot2::geom_sf(data = box_GHA , fill = NA, colour = "black", lwd = 0.3) +
  ggplot2::geom_sf(data = box_IDN , fill = NA, colour = "black", lwd = 0.3) +
  viridis::scale_fill_viridis(direction = -1, na.value = NA, option = "viridis") +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  ggplot2::labs(fill = "Tree cover loss within mine sites, 2001-2019 accumulated (thousand ha)") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.justification = "center",
                 legend.box.margin=unit(c(-0.5,0,-0.2,0), "cm")) + 
  ggplot2::guides(fill = guide_colorbar(barheight = unit(2, 
                                                         units = "mm"), barwidth = unit(140, units = "mm"), 
                                        title.position = "top", title.hjust = 0.5, label.hjust = 1))


grob_A <- grid::grobTree(textGrob("A", x=0.23,  y=0.39, hjust=0, gp=gpar(col="black", fontsize=12)))
grob_B <- grid::grobTree(textGrob("B", x=0.43,  y=0.42, hjust=0, gp=gpar(col="black", fontsize=12)))
grob_C <- grid::grobTree(textGrob("C", x=0.79,  y=0.46, hjust=0, gp=gpar(col="black", fontsize=12)))
p <- p + annotation_custom(grob_A) + annotation_custom(grob_B) + annotation_custom(grob_C)

p_merge_zooms <- cowplot::plot_grid(p_BRA, p_GHA, p_IDN, nrow = 1, rel_widths = c(1/3, 1/3, 1/3), labels = c("A", "B", "C"), label_size = 10) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
p_merge <- cowplot::plot_grid(p_merge_zooms, p, nrow = 2, rel_heights = c(1/3, 2/3)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggplot2::ggsave("figure-1_maps.png", 
                plot = p_merge, device = "png", 
                path = paste0("./figures/"),
                scale = 1, width = 200, height = 200, units = "mm")


# SI map deforestation > 100 km2 ------------------------------------------

p <- res %>% dplyr::filter(def_total > 100) %>% 
  ggplot2::ggplot(aes(fill =  def_total * 100 / 1000)) + # in thousand hectartes instead of km2
  ggplot2::geom_sf(data = world_robin, fill = "#FAFAFA") +
  ggplot2::geom_sf(alpha = 0.65) +
  viridis::scale_fill_viridis(direction = -1, na.value = NA, option = "inferno", end = 0.9, 
                              limits = c(10, 80), breaks = seq(10, 80, 10)) +
  ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
  ggplot2::labs(fill = "Tree cover loss within mine sites, 2001-2019 accumulated (thousand ha)") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom",
                 legend.justification = "center",
                 legend.box.margin=unit(c(-0.7,0,0,0), "cm")) +
  ggplot2::guides(fill = guide_colorbar(barheight = unit(2,
                                                         units = "mm"), barwidth = unit(140, units = "mm"),
                                        title.position = "top", title.hjust = 0.5, label.hjust = 1))
ggplot2::ggsave("figure-S2_map_100-800.png",
                plot = p, device = "png",
                path = paste0("./figures/"),
                scale = 1, width = 200, height = 120, units = "mm")

# check data for manuscript text ------------------------------------------

# summary of tiles
summary(res$def_total)
nrow(res %>% dplyr::filter(def_total < 10)) / nrow(res)

# commodities in zoom boxes
check_BRA <- data_cluster %>% dplyr::filter(hcluster_id %in% unique(BRA_sf_poly$hcluster_id))
check_BRA %>% dplyr::group_by(commodity) %>% dplyr::summarise(loss = sum(def_price_weight))

check_GHA <- data_cluster %>% dplyr::filter(hcluster_id %in% unique(GHA_sf_poly$hcluster_id))
check_GHA %>% dplyr::group_by(commodity) %>% dplyr::summarise(loss = sum(def_price_weight))

check_IDN <- data_cluster %>% dplyr::filter(hcluster_id %in% unique(IDN_sf_poly$hcluster_id))
check_IDN %>% dplyr::group_by(commodity) %>% dplyr::summarise(loss = sum(def_price_weight))
