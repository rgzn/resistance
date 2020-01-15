library(tidyverse)
library(sf)
library(lubridate)
library(stars)
library(nngeo)
# library(raster)


# internal source files:
source("analyze.r")
source("import.r")

# read ram location data:
ram_data_filename = "./shapefiles/AllRams2004_2016.shp"
ram_data = read_snbs_shapefile(ram_data_filename)

# Use ram data CRS as project CRS:
project_crs = st_crs(ram_data)

# read core homerange polygons:
core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
core_range = core_range_filename %>%
  st_read %>%
  st_transform(crs = project_crs)

# read cost-distance layers:
cd_dir = "./rasters/costdistance"
cd_files = list.files(path = cd_dir, pattern = "*tif$", full.names = TRUE)
# produced 1 attribute with 4 dimensions:
# cd_stars = read_stars(cd_files, along = "cost_function", proxy = TRUE) 
cd_stars = read_rasters(path = cd_dir)
if ( st_crs(cd_stars) != project_crs) st_transform(cd_stars, project_crs)

# build excursions collection from ram data:
# ram_excursions <- get_excursions_with_buffers(ram_data, core_range)

MIN_DISTANCE = units::as_units(14000, 'm')
ram_data %>%
  tibble::rowid_to_column("id") %>% 
  mutate(dist_from_core =  st_distance(x = geometry, y = core_range)) %>% 
  filter(dist_from_core > MIN_DISTANCE) %>% 
  mutate(geometry.core = core_range$geometry) %>% 
  mutate(geometry.buffer = st_buffer(geometry.core, dist = dist_from_core)) %>% 
  mutate(geometry.buffer = st_cast(geometry.buffer, "MULTILINESTRING")) %>% 
  mutate(buffer.circumference = st_length(geometry.buffer)) %>% 
  mutate(n_samples = 10) %>% 
  group_by(id) %>% 
  mutate(geometry.pseudopoints = st_union(st_sample(geometry.buffer, n_samples, type = "regular"))) %>% 
  ungroup() ->
  x


x %>% 
  select(-geometry.core, -geometry.buffer, -buffer.circumference) %>% 
  st_set_geometry("geometry.pseudopoints") %>% 
  st_cast("POINT") ->
  y

y <-
  y %>% mutate(
    cdMean = raster_extract(cd_stars, primates_meso, fun = mean, na.rm = TRUE),
    cdMax = raster_extract(cd_stars, primates_meso, fun = max, na.rm = TRUE),
    cdMin = raster_extract(cd_stars, primates_meso, fun = min, na.rm = TRUE)
  )

y <- st_sample_by_sf(cd_stars, y)


ggplot() + geom_sf(data = x) + geom_sf(data = x$geometry.buffer) + geom_sf(data = core_range)




