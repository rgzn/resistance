library(tidyverse)
library(sf)
library(lubridate)
library(stars)
# library(raster)

# internal source files:
source("import.r")
source("analyze.r")

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
cd_stars = read_stars(cd_files, along = "cost_function", proxy = TRUE)
if ( st_crs(cd_stars) != project_crs) st_transform(cd_stars, project_crs)

# build excursions collection from ram data:
ram_excursions <- get_excursions_with_buffers(ram_data, core_range)

# ggplot() +  geom_point(data = ram_excursions, aes(x = duration, y = max_dist))
# ggplot() + geom_histogram(data = ram_excursions, aes(x=duration), binwidth = hours(24))

# subset of excursions for testing:
test_excursions <-
  ram_excursions %>% 
  filter(max_dist > 5000)

# sample density:
POINTS_PER_M = 1/10000

# sample points equidistant to furthest excursion point:

test_excursions <-
  test_excursions %>%
  group_by(AnimalID, exit_event) %>%
  mutate(geometry.buffer = st_cast(geometry.buffer, "MULTILINESTRING")) %>%
  mutate(circumference = st_length(geometry.buffer)) %>%
  mutate(n_samples = as.integer(circumference * POINTS_PER_M)) %>%
  mutate(geometry.sample = st_union(st_sample(geometry.buffer, n_samples, type = "regular"))) %>%
  ungroup()

samples <- 
  test_excursions %>% 
  st_set_geometry("geometry.sample") %>% 
  select(-geometry.core, -geometry.buffer, -geometry.points, -geometry.endpoint) %>% 
  st_cast("POINT")

ed_samplepoints <- st_sample_by_sf(cd_stars, samples) %>% 
  mutate(real = FALSE)
endpoints <- st_sample_by_sf(cd_stars, st_set_geometry(test_excursions, "geometry.endpoint")) %>% 
  mutate(real = TRUE)

# combine real and generated sample points with their cost values
sampled_points <- endpoints %>% 
  select(colnames(ed_samplepoints)) %>% 
  rbind(ed_samplepoints)

# spread to long format and generate percentile rank for cost:
sampled_points <- sampled_points %>%
  gather(key = "cost_layer",
         value = "cost",
         attr.V1,
         attr.V2,
         attr.V3,
         attr.V4) %>% 
  group_by(AnimalID, exit_event, cost_layer) %>% 
  mutate(percrank=rank(cost)/length(cost))

ggplot(data = sampled_points %>% filter(cost_layer == "attr.V1")) +
  geom_point(aes(x = max_dist, y = cost, color = real, size = real), alpha = 0.4) +
  ggtitle("Cost-Distance Values for Excursion Endpoints and Random Equidistant Points (-log ) ")

ggplot(data = sampled_points %>% filter(cost_layer == "attr.V4")) +
  geom_point(aes(x = max_dist, y = 100 - cost, color = real, size = real), alpha = 0.4) +
  ggtitle("Cost-Distance Values for Excursion Endpoints and Random Equidistant Points (-exp4) ")

ggplot(data = sampled_points %>% filter(real)) +
  geom_point(aes(x = max_dist, y = percrank, color = cost_layer))

bbox_sf <-
  test_excursions %>% 
  st_union() %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(dist = 1000)

ggplot() + 
  geom_stars(data = cd_stars[bbox_sf] %>% slice(cost_function, 1), downsample = 20) + 
  geom_sf(data = test_excursions$geometry.buffer , alpha = 0, color = "khaki") + 
  geom_sf(data = test_excursions$geometry.points, color = "orange") + 
  geom_sf(data = test_excursions$geometry.endpoint, color = "red") +
  geom_sf(data = test_excursions$geometry.sample, color = "yellow")



ggplot() + 
  geom_sf(data = test_excursions$geometry.buffer, alpha = 0, color  = "darkblue") + 
  geom_sf(data = test_excursions$geometry.endpoint, color = "red")

ggplot() + 
  geom_sf(data = test_excursions %>% st_set_geometry("geometry.buffer"), aes(color = as.factor(exit_event)))


