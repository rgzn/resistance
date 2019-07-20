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


ggplot() +  geom_point(data = ram_excursions, aes(x = duration, y = max_dist))
ggplot() + geom_histogram(data = ram_excursions, aes(x=duration), binwidth = hours(24))


test_excursions <-
  ram_excursions %>% 
  filter(AnimalID == "S20")

POINTS_PER_M = 1/10000
test_excursions <- 
  test_excursions %>% 
  mutate(geometry.buffer = st_cast(geometry.buffer, "LINESTRING")) %>% 
  mutate(circumference = st_length(geometry.buffer)) %>% 
  mutate(n_samples = as.integer(circumference*POINTS_PER_M)) %>% 
  mutate(geometry.sample = st_sample(geometry.buffer, size = n_samples)) 

bbox_sf <-
  test_excursions %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(dist = 1000)


ggplot() + 
  geom_stars(data = cd_stars[bbox_sf], downsample = 20) + 
  geom_sf(data = st_cast(test_excursions$geometry.buffer, "LINESTRING")) + 
  geom_sf(data = test_excursions$geometry.points, color = "blue") + 
  geom_sf(data = test_excursions$geometry.endpoint, color = "red") +
  geom_sf(data = test_excursions$geometry.sample, color = "yellow")


##############################
excursion_points <-
  ram_data %>%  
  label_excursions(core_range) %>% 
  filter(!in_core)

excursion_meta <-
  excursion_points %>% 
  group_by(AnimalID, exit_event) %>% 
  mutate(distance = st_distance(geometry)[1,]) %>% 
  summarise(duration = difftime(max(datetime), 
                                min(datetime), 
                                units = "secs"),
            n_points = n(),
            start_time = min(datetime),
            end_time = max(datetime),
            max_dist = max(distance))



###### ENCAPSULATE INTO FUNCTION TO APPLY ACROSS ANIMALS:
ram_data %>% 
  #group_by(AnimalID) %>% 
  filter(AnimalID == "S20") ->
  animal_data

# find excursions from core:
animal_data %>% 
  label_excursions(core_range) ->
  animal_data

# make table of excursion data:
animal_excursions <- animal_data %>%
  filter(!in_core) %>% 
  group_by(exit_event) %>% 
  summarise(duration = difftime(max(datetime), 
                                min(datetime), 
                                units = "secs"),
            n_points = n(),
            start = min(datetime),
            end = max(datetime)) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)



excursion_data <- animal_data %>% 
  filter(!in_core) %>% 
  left_join(animal_excursions)


