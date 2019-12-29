library(tidyverse)
library(sf)
library(lubridate)
library(stars)
#library(raster)
source("import.r")
source("analyze.r")

# read ram location data:
ram_data_filename = "./shapefiles/AllRams2004_2016.shp"
ram_data = read_snbs_shapefile(ram_data_filename)
project_crs = st_crs(ram_data)

# read core homerange polygons:
core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
core_range = core_range_filename %>%
  st_read %>%
  st_transform(crs = project_crs)

# read rsf
rsf_file = "./rasters/rsf/pRasout_20190613.tif"
rsf_stars = read_stars(rsf_file, proxy = TRUE)
resistance = -log(rsf_stars) # replace this with stack of different transformations

# # label excursions from core for each animal:
# ram_data = ram_data %>% 
#   label_excursions(core_range)

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

# finde centroid:
animal_data %>% 
  st_union() %>% 
  st_centroid() ->
  animal_center

# find radius:
animal_data %>% 
  st_union() %>% 
  st_convex_hull() %>% 
  st_cast("POINT") %>% 
  st_distance(animal_center) %>% 
  max() ->
  animal_radius

animal_circle = animal_center %>% st_buffer(1.4*animal_radius)
 
animal_rsf = rsf_stars[animal_circle]
animal_multipoint = st_union(animal_data)  

animal_core <- core_range %>% 
  st_cast("POLYGON") %>%
  filter(st_intersects(geometry, animal_multipoint, sparse = FALSE))
  

ggplot() +
  geom_stars(data = -log(animal_rsf), downsample = 4) +
  geom_sf(data = animal_core, color = "red", alpha = 0.3) +
  geom_sf(data = excursion_data %>% filter(n_points > 1),
          aes(color = factor(exit_event))) +
  theme_minimal()


# Sampling boundaries:
test = st_cast(animal_core, "MULTILINESTRING")
test_2 = st_cast(animal_core, "POINT")

my_points = st_sample(test, size = 100)
ggplot() + 
  geom_sf(data = test) +
  geom_sf(data = my_points)

# testing geometric creation:
animal_data %>% filter(!in_core) %>%
  group_by(exit_event) %>% 
  summarise(duration = difftime(max(datetime), min(datetime), 
                                units = "secs"),
            n_points = n()) %>% 
  dplyr::arrange(desc(duration)) %>% 
  slice(1) %>% 
  dplyr::select(exit_event) ->
  big_event

animal_test = animal_data %>% filter(exit_event == big_event$exit_event)

a_point = animal_test[1,]

excursion_data %>% 
  group_by(exit_event) %>% 
  mutate(dist = st_distance(geometry, animal_center, by_element = FALSE)) %>% 
  summarize(dist = max(dist)) %>%
  arrange(desc(dist)) %>% 
  slice(1) ->
  test_excursion



test_2 %>% 
  mutate(dist = st_distance(geometry, a_point, by_element = FALSE)) %>% 
  arrange(as.numeric(dist)) %>% 
  slice(1:2) %>% 
  st_union() %>% 
  st_cast("LINESTRING") -> test_line
  
ggplot() + 
  geom_sf(data = test_line, color = "red", size = 10) + 
  geom_sf(data = a_point, size = 4, color = "green") + 
  geom_sf(data = test_2) + 
  geom_sf(data = test_excursion %>% st_rotate(1), color = "blue")
  
  
test %>%
  st_rotate(45) %>% 
  ggplot() + geom_sf()
  


resistance$neglog = -log(animal_rsf) 
resistance$negexp4 = negexp_resist(animal_rsf, 4)
resistance$negexp8 = negexp_resist(animal_rsf, 8)
resistance$negexp16 = negexp_resist(animal_rsf, 16)


resistance_sp = as_Spatial(resistance)
animal_transition = gdistance::transition(resistance, 
                                       transitionFunction = function(x) {1/mean(x)},
                                       directions = 8)
CorrMatrix <- geoCorrection(animal_transition, type="c")
animal_transitionC = geoCorrection(animal_transition, type="c")



animal_test %>% 
  mutate(dist = st_distance(geometry, animal_center)) %>% 
  filter(dist == max(dist) | dist == min(dist)) ->
  start_end_sf

start_end_sp = as_Spatial(start_end_sf)

# animal_excursions_sp = animal_data %>% filter(!in_core) %>% as_Spatial()

costDistance(animal_transitionC, start_end_sp)

test_path = shortestPath(animal_transitionC, 
                         start_end_sp[1,], 
                         start_end_sp[2,], 
                         output="SpatialLines")
animal_resistance %>%
  as.data.frame(xy = TRUE) %>% 
  ggplot() + geom_raster(aes(x=x,y=y))


plot(animal_resistance, 
     xlab="x coordinate (m)", 
     ylab="y coordinate (m)",
     legend.lab="resistance")
lines(test_path, col="red", lwd=2)
points(animal_test %>% as_Spatial())

ggplot() +
  geom_sf(data = start_end_sf)
