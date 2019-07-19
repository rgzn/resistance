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


summarize_excursion_costs = function(points, polygon) {
  
  points <-
    points %>%  
    label_excursions(polygon)
  
  excursions <-
    points %>% 
    filter(!in_core) %>% 
    group_by(AnimalID, exit_event) %>% 
    mutate(distance = st_distance(geometry)[1,]) %>% 
    summarise(duration = difftime(max(datetime), 
                                  min(datetime), 
                                  units = "secs"),
              n_points = n(),
              start_time = min(datetime),
              end_time = max(datetime),
              max_dist = max(distance))
  
  # core multipolygon as separate polygons
  polygons <- polygon %>% 
    st_cast("POLYGON") %>% 
    mutate(Id = rownames(.)) 
  
  # event points grouped together:
  event_multipoints <-
    points %>%
    group_by(AnimalID, exit_event) %>%
    summarize()
  
  # core polygons associated with events:
  event_polygons <-
    event_multipoints %>% 
    st_join(polygons, left = FALSE) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    left_join(polygons) %>% 
    st_as_sf() %>% 
    group_by(AnimalID, exit_event) %>% 
    summarize()
  
  # core polygons associated with animals:
  animal_polygons <-
    event_multipoints %>% 
    st_join(polygons, left = FALSE) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    left_join(polygons) %>% 
    st_as_sf() %>% 
    group_by(AnimalID) %>% 
    summarize()
  
  # get maximum distance for each event:
  excursion_points <- 
    points %>% filter(!in_core)
  
  # find maximum excursion distance for each event:
  # event_max_dist <- 
  #   event_polygons %>% 
  #   as_tibble() %>% 
  #   right_join(excursion_points, by = c("AnimalID", "exit_event")) %>% 
  #   mutate(dist = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>% 
  #   group_by(AnimalID, exit_event) %>% 
  #   summarize(max_dist = max(dist)) %>% 
  #   mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist))
  
 event_max_dist <- 
   animal_polygons %>% 
   as_tibble() %>% 
   right_join(excursion_points, by = c("AnimalID")) %>% 
   mutate(dist = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>% 
   group_by(AnimalID, exit_event) %>% 
   summarize(max_dist = max(dist)) %>% 
   mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist))
  
  # create buffers for each event from the core to max dist:
  event_buffers <- 
    animal_polygons %>%
    right_join(event_max_dist) %>% 
    mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist)) %>% 
    mutate(geometry = st_buffer(geometry, dist = max_dist))
  
  
  ggplot() + 
    geom_sf(data = event_buffers,  aes(color = as.factor(exit_event))) + 
    geom_sf(data = excursion_points,  aes(color = as.factor(exit_event)))
  

}

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


