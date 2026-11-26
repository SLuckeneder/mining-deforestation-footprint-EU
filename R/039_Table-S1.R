
library(dplyr)
library(countrycode)
library(gt)
library(xtable)

path_results <- "data/results"

file <- "agg_price_2001-2019_2024-10-08 14:49:30.353889.csv" # rename file!
store <- read.csv(file.path(path_results, file))

# aggregate EU27
EU27 <- c("BEL", "BGR", "DNK", "DEU", "EST", "FIN", "FRA", "GRC", "IRL", "ITA",
          "HRV", "LVA", "LTU", "LUX", "MLT", "NLD", "AUT", "POL", "PRT", 
          "ROU", "SWE", "SVK", "SVN", "ESP", "CZE", "HUN", "CYP")
dat_EU <- store %>% 
  dplyr::mutate(to_region = ifelse(to_region %in% EU27, "EU27", to_region)) %>%
  dplyr::mutate(from_region = ifelse(from_region %in% EU27, "EU27", from_region))
dat_EU <- dat_EU %>% 
  dplyr::group_by(from_region, to_region) %>%
  dplyr::summarise(y2001_2019 = sum(value)) %>%
  dplyr::ungroup() 

from_EU <- dat_EU %>%
  dplyr::filter(from_region == "EU27") %>%
  dplyr::summarise(forest_loss_production = sum(y2001_2019)) %>%
  dplyr::mutate(from_region = "EU27") 

to_EU <- dat_EU %>%
  dplyr::filter(to_region == "EU27") %>%
  dplyr::summarise(forest_loss_consumption = sum(y2001_2019)) %>%
  dplyr::mutate(to_region = "EU27") 

# table production
dat_production <- store %>%
  dplyr::group_by(from_region) %>%
  dplyr::summarise(forest_loss_production = sum(value))
dat_production <- dat_production %>%
  dplyr::bind_rows(from_EU)

# table consumption
dat_consumption <- store %>%
  dplyr::group_by(to_region) %>%
  dplyr::summarise(forest_loss_consumption = sum(value))
dat_consumption <- dat_consumption %>%
  dplyr::bind_rows(to_EU)

# add population in 2019
dat_pop <- gt::countrypops %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(country_code_3, population)
dat_pop_EU <- dat_pop %>% dplyr::filter(country_code_3 %in% EU27) %>%
  dplyr::summarise("population" = sum(population)) %>%
  dplyr::mutate(country_code_3 = "EU27")
dat_pop <- dat_pop %>% 
  dplyr::bind_rows(dat_pop_EU)

# merge tables
dat <- dplyr::left_join(dat_production, dat_consumption, by = c("from_region" = "to_region")) %>%
  dplyr::left_join(dat_pop, by = c("from_region" = "country_code_3"))

# add m2 per capita values
dat <- dat %>%
  dplyr::mutate(forest_loss_production_capita = forest_loss_production * 1000000 / population) %>%
  dplyr::mutate(forest_loss_consumption_capita = forest_loss_consumption * 1000000 / population)

# export to latex
dat <- dat %>%
  dplyr::select(from_region, forest_loss_production, forest_loss_production_capita, forest_loss_consumption, forest_loss_consumption_capita) %>%
  dplyr::arrange(-forest_loss_consumption)
colnames(dat) <- c("Country", 
                   "Production (km2)", "Production (m2 / capita)",
                   "Consumption (km2)", "Consumption (m2 / capita)")
print(xtable::xtable(dat, 
                     digits = c(0, 0, 2, 2, 2, 2), align = "llrrrr",
                     caption = "Caption here."), 
      include.rownames=FALSE, size = "tiny")


