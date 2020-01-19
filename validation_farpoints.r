library(tidyverse)
library(sf)
library(lubridate)
library(stars)
library(geobgu)
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
cd_stars = read_rasters(path = cd_dir)
if ( st_crs(cd_stars) != project_crs) st_transform(cd_stars, project_crs)


# Invert rasters to make sure they are in terms of cost:
cd_stars  = 100 - cd_stars

# Correct for inconsistent cost distance rasters:
# this section may be removed when cost distance are all in terms of cost 
# s.t. cost increases with distance
# cd_stars <- cd_stars %>% 
#   mutate(FINALCD_Constant1.tif = 100 - FINALCD_Constant1.tif,
#          FINALCD_NegExp2.tif = 100 - FINALCD_NegExp2.tif,
#          FINALCD_NegExp4.tif = 100 - FINALCD_NegExp4.tif)

# create pseudopoints for each point:
MIN_DISTANCE = units::as_units(2000, 'm') #no points below this distance are included
ram_data %>%
  tibble::rowid_to_column("id") %>%                                             # give unique id to each point
  mutate(dist_from_core =  st_distance(x = geometry, y = core_range)) %>%       # calculate distance from core
  filter(dist_from_core > MIN_DISTANCE) %>%                                     # remove close points
  mutate(geometry.core = core_range$geometry) %>%                               # incorporate core range as a column
  mutate(geometry.buffer = st_buffer(geometry.core, dist = dist_from_core)) %>% # create buffer polygons
  mutate(geometry.buffer = st_cast(geometry.buffer, "MULTILINESTRING")) %>%     # convert buffers to linestrings
  mutate(buffer.circumference = st_length(geometry.buffer)) %>%                 # find circumference of buffers
  mutate(n_samples = 100) %>%                                                    # choose number of fake points
  group_by(id) %>%                                                              # generate fakepoint geometries
  mutate(geometry.pseudopoints = st_union(st_sample(geometry.buffer, n_samples, type = "regular"))) %>% 
  ungroup() ->
  real_data

# Generate sf collection of just the fake points:
fake_data <- real_data %>% 
  mutate(geometry = geometry.pseudopoints) %>% 
  # st_set_geometry("geometry.pseudopoints") %>% 
  st_cast("POINT")

# Label real and fake data:
real_data <- real_data %>% mutate(real = TRUE)
fake_data <- fake_data %>% mutate(real = FALSE)

# combine real and fake data:
all_data <- rbind(real_data, fake_data)

# remove unnecessary data:
all_data <- all_data %>% select(-DOP,
                                -SV, 
                                -n_samples,
                                -geometry.pseudopoints,
                                -geometry.core,
                                -buffer.circumference,
                                -geometry.buffer)
rm(real_data, fake_data)

# Extract raster values of CD layers for all points
all_data <- raster_extract_layers(cd_stars, all_data)

# Transform to long data frame:
all_data <- all_data %>%
  gather(key = "cost_layer",
         value = "cost",
         contains("tif"))

# Calculate percentile rank for each cohort of points with a shared
#   real point and cost layer
all_data <- all_data %>% 
  group_by(id, cost_layer) %>% 
  mutate(percrank = ntile(cost, 100))

#################### PLOTS AND METRICS ##########################################

ggplot(data = all_data %>% filter(real == TRUE, cost_layer %in% c("ln.tif", 
                                                                  "Exp8.tif", 
                                                                  "Exp16.tif")))+
  geom_point(aes(x = dist_from_core, y = percrank, color = cost_layer), size = 3.0, alpha = 0.1) +
  ggtitle("Rank of ram locations costs vs. equidistant pseudopoints") +
  xlab("Distance from core homerange [m]") + 
  ylab("Actual location percentile cost (vs equidistant pseudopoints)")

all_data %>% filter(real == TRUE, cost_layer %in% c("ln.tif", 
                                                    "Exp4.tif", 
                                                    "Exp16.tif")) 

all_data %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  summarize(mean_rank = mean(percrank)) %>% 
  arrange(mean_rank)

all_data %>% 
  as_tibble %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  summarize(mean_rank = mean(percrank), dist_weighted_rank = mean(percrank*dist_from_core)) %>% 
  arrange(dist_weighted_rank)

all_data %>% 
  group_by(cost_layer, real) %>% 
  summarize(cost_sum = sum(cost, na.rm = T)) %>% 
  group_by(cost_layer) %>% 
  summarize(cost_ratio = sum(cost_sum*real)/sum(cost_sum*(!real))) %>% 
  arrange(cost_ratio)

fitted_data <- all_data %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  do(fit = nls(percrank ~ a * dist_from_core + b, data = ., start = list(a = 0.5, b = 10))) %>% 
  broom::augment(fit)

qplot(dist_from_core, percrank, data = fitted_data, geom = 'point', colour = cost_layer) +
  geom_line(aes(y=.fitted))



ggplot(data = all_data %>% filter(cost_layer == "Exp8.tif")) +
  geom_point(aes(x = dist_from_core, y = cost, color = real, alpha = (0.1 + 0.9*real)))

ggplot(data = all_data %>% filter(cost_layer == "Exp16.tif")) +
  geom_point(aes(x = dist_from_core, y = cost, color = real, alpha = (0.1 + 0.9*real)))

ggplot(data = all_data %>% arrange(real)) +
  facet_wrap( ~ cost_layer, ncol = 3) + 
  geom_point(aes(x = dist_from_core, 
                 y = cost, 
                 color = real, 
                 alpha = (0.05 + 0.9*real),
                 size = 0.1 + .06*real))


# ggplot(data = all_data %>% filter(cost_layer == "FINALCD_NegExp4.tif")) +
#   geom_point(aes(x = dist_from_core, y = cost, color = real), size = 3.0, alpha = 0.4)
# 
# ggplot(data = all_data %>% filter(cost_layer == "CD_NegLog.tif")) +
#   geom_point(aes(x = dist_from_core, y = cost, color = real), size = 3.0, alpha = 0.4)
