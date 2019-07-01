library(tidyverse)
library(sf)
# library(amt)
library(lubridate)
library(stars)
library(raster)
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
cost_rasters = read_rasters(path, proxy = TRUE, along = "cost-dist")

# find excursions outside homerange:
ram_data = ram_data %>% 
  label_excursions(polygon = core_range)


####################################################################

s20_data = d %>% filter(AnimalID == "S20")
# only data > certain lat
north_data = ram_data %>% arrange(desc(UTM_N)) %>% slice(1)

## Plots
theme_set(theme_minimal())

ggplot() + 
  geom_sf(data = core_range, alpha = 0.2) +
  geom_sf(data = s20_data %>% filter(!in_core), aes(color = as.factor(exit_event))) + 
  guides(color=FALSE) + 
  coord_sf(xlim = c(293000, 309000), ylim = c(4200000, 4230000))

ggplot(data = herd_units) + 
  geom_sf(aes(fill = GCU))

ggplot() + 
  geom_stars(data = risk)
