library(tidyverse)
library(sf)
library(lubridate)
library(stars)
#library(raster)
source("import.r")
source("analyze.r")

# Vector data

# read ram location data:
ram_data_filename = "./shapefiles/AllRams2004_2016.shp"
ram_data = read_snbs_shapefile(ram_data_filename)
project_crs = st_crs(ram_data)

# read core homerange polygons:
core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
core_range = core_range_filename %>%
  st_read %>%
  st_transform(crs = project_crs)

# Raster Data
raster_path = "./rasters"
cost_rasters = read_rasters(raster_path, proxy = TRUE, along = "cost-dist")
assertthat::are_equal(project_crs, st_crs(cost_rasters))

# find excursions outside homerange:
ram_data = ram_data %>% 
  label_excursions(polygon = core_range)

excursion_data = ram_data %>%
  filter(!in_core) %>%
  st_sample_sf(cost_rasters) %>%

# need to gather this into long df
excursion_data %>%
  gather("cd_layer", "cost", -datetime)

excursion_data = excursion_data %>% 
  mutate_at(c("FINALCD_Constant1.tif", "FINALCD_NegExp2.tif", "FINALCD_NegExp4.tif"),
         ~ . * -1 + 100)

excursion_data %>% 
  group_by(AnimalID, exit_event) %>%
  summarise(cost_constant = max(FINALCD_Constant1.tif),
            cost_exp2 = max(FINALCD_NegExp2.tif),
            cost_exp4 = max(FINALCD_NegExp4.tif)) %>%
  ungroup() %>%
  summarise(mean(cost_constant), mean(cost_exp2), mean(cost_exp4))



  
max_costs = excursion_data %>% 
  group_by(AnimalID, exit_event) %>%
  summarise(cost_constant = max(FINALCD_Constant1.tif),
            cost_exp2 = max(FINALCD_NegExp2.tif),
            cost_exp4 = max(FINALCD_NegExp4.tif)) %>%
  ungroup()

ggplot(data = max_costs) +
  geom_point(aes(x = cost_constant, y = cost_exp2), color = "blue") +
  geom_point(aes(x = cost_constant, y = cost_exp4), color = "red")
####################################################################

# excursion data from a single animal
# s20_excursions = ram_data %>% 
#   filter(AnimalID == "S20") %>%
#   filter(!in_core)
# 
# costs = cost_rasters %>% 
#   st_sample_by_sf(s20_excursions)

s20_excursions %>%
  group_by(exit_event) %>% 
  summarise(duration = difftime(max(datetime), min(datetime), 
                                units = "secs"),
            n_points = n()) %>%
  filter(duration > as.difftime("24:0:0")) ->
  x

s20_excursions %>% deparse()

ggplot() + 
  geom_sf(data = core_range, alpha = 0.2) +
  geom_sf(data = x, aes(color = as.factor(exit_event))) + 
  guides(color=FALSE) + 
  coord_sf(xlim = c(293000, 309000), ylim = c(4200000, 4230000))

# only data > certain lat
north_data = ram_data %>% arrange(desc(UTM_N)) %>% slice(1)

## Plots
theme_set(theme_minimal())

ggplot() + 
  geom_sf(data = core_range, alpha = 0.2) +
  geom_sf(data = s20_excursions, aes(color = as.factor(exit_event))) + 
  guides(color=FALSE) + 
  coord_sf(xlim = c(293000, 309000), ylim = c(4200000, 4230000))

ggplot(data = herd_units) + 
  geom_sf(aes(fill = GCU))

ggplot() + 
  geom_stars(data = risk)
