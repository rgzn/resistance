# Validation by individual points, based on distance from core
# Jon Weissman 2020
#
# This compares different cost rasters. 
# Uses individual gps locations outside of the homerange, and compares the cost value
# at each point compared to the cost value of pseudopoints which are equidistant from
# the core range. 
#
# Inputs: Cost rasters, polygons from which those rasters were generated, points to test
# 

library(tidyverse)
library(sf)   # simple features for spatial vector data
library(lubridate)
library(stars) # for spatial raster data
library(geobgu)  # spatial helper functions (used to sample stars)
library(nngeo)   # spatial helper functions
# library(raster)


# internal source files:
source("analyze.r")    # my helper functions for data processing
source("import.r")     # helper functions to import data

# read ram location data:
ram_data_filename = "./shapefiles/AllRams2004_2016.shp"
ram_data = read_snbs_shapefile(ram_data_filename) %>% 
  select(-DOP, -SV, -Sex)

# Use ram data CRS as project CRS:
project_crs = st_crs(ram_data)

# read core homerange polygons:
core_range_dir = "./shapefiles/HomeRange2016_2019EwesRams_1yrPost_95%KDE_Plugin"  # directory containing core shapefiles
core_range_files = list.files(path = core_range_dir, 
                              pattern = "*shp$", 
                              full.names = TRUE)

# read shapefiles into sf collection
core_range <- core_range_files %>% 
  map(st_read) %>%
  map(function(x) st_transform(x, crs = project_crs)) %>% 
  reduce(st_union) %>% 
  mutate(Id = 0) %>% 
  select(Id, geometry) 

## Simplify core range geometry 
# this makes it take less memory and speed up distance computations
core_range = st_simplify(core_range, preserveTopology = FALSE, dTolerance = 200)

# DEPRECATED:
# if core range is already unified:
# core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
# core_range_single_shp = core_range_filename %>%
#   st_read %>%
#   st_transform(crs = project_crs)

# read cost-distance layers:
cd_dir = "./rasters/costdistance"
cd_files = list.files(path = cd_dir, pattern = "*tif$", full.names = TRUE)
cd_stars = read_rasters(path = cd_dir)
if ( st_crs(cd_stars) != project_crs) st_transform(cd_stars, project_crs)


# Invert rasters to make sure they are in terms of cost:
# cd_stars  = 100 - cd_stars

# Correct for inconsistent cost distance rasters:
# this section may be removed when cost distance are all in terms of cost 
# s.t. cost increases with distance
# cd_stars <- cd_stars %>% 
#   mutate(FINALCD_Constant1.tif = 100 - FINALCD_Constant1.tif,
#          FINALCD_NegExp2.tif = 100 - FINALCD_NegExp2.tif,
#          FINALCD_NegExp4.tif = 100 - FINALCD_NegExp4.tif)



# Filter out close points using this constant:
MIN_DISTANCE = units::as_units(15000, 'm') # no points below this distance are included
# make core range buffer:
core_range_buffer = core_range %>%
  st_buffer(dist = MIN_DISTANCE) %>%  # buffer distance
  st_simplify(dTolerance = 20)        # simplfication for reduced size


# main processing sequence:

valid_data <- ram_data  %>%
  # head(200) %>%                   # use to test for a few points
  tibble::rowid_to_column("id") %>% # give unique id to each point
  st_difference(core_range_buffer) %>%    # speeds things up by only taking into account points outside the polygons
  mutate(dist_from_core =  st_distance(x = geometry, y = core_range)) %>%       # calculate distance from core
  # filter(as.integer(dist_from_core) > 0) %>% 
  mutate(geometry.buffer = 
           get_buffers(distances = dist_from_core, sf_object = core_range)$geometry) %>%  # add buffer polygons for each point
  mutate(geometry.buffer = st_cast(geometry.buffer, "MULTILINESTRING")) %>%                    # convert buffers to linestrings
  # mutate(buffer.circumference = st_length(geometry.buffer)) %>%                 # find circumference of buffers
  # mutate(n_samples = 0.1 * buffer.circumference) %>%                            # choose number of fake points
  mutate(n_samples = 100) %>% 
  group_by(id) %>%                                                              # generate fakepoint geometries
  mutate(geometry.pseudopoints = st_union(st_sample(geometry.buffer, n_samples, type = "regular"))) %>% 
  ungroup() ->
  real_data

# OLD VERSIOJN:
# ram_data %>% 
#   filter(as.integer(dist_from_core) > 0) %>% 
#   mutate(geometry.buffer = get_buffers(distances = dist_from_core, sf_object = core_range)) -> p
#   # filter(dist_from_core > MIN_DISTANCE) %>%                                     # remove close points
#   # mutate(geometry.core = core_range$geometry) %>%                               # incorporate core range as a column
#   # mutate(geometry.buffer = st_buffer(geometry.core, dist = dist_from_core)) %>% # create buffer polygons
#   mutate(geometry.buffer = st_buffer(core_range, dist_from_core)) %>% 
#   mutate(geometry.buffer = st_cast(geometry.buffer, "MULTILINESTRING")) %>%     # convert buffers to linestrings
#   mutate(buffer.circumference = st_length(geometry.buffer)) %>%                 # find circumference of buffers
#   mutate(n_samples = 10) %>%                                                    # choose number of fake points
#   group_by(id) %>%                                                              # generate fakepoint geometries
#   mutate(geometry.pseudopoints = st_union(st_sample(geometry.buffer, n_samples, type = "regular"))) %>% 
#   ungroup() ->
#   real_data

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
all_data <- all_data %>% select(-n_samples,
                                -geometry.pseudopoints,
                                #-geometry.core,
                                # -buffer.circumference,
                                -geometry.buffer)
rm(real_data, fake_data, valid_data)

# Extract raster values of CD layers for all points
all_data <- raster_extract_layers(cd_stars, all_data)

# Transform to long data frame: (switch to dplyr::pivot_longer ??)
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

# plot all the real points
# distance vs percentile rank
# colored by cost layer
# (only certain cost layers plotted for clarity)
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

# Show cost layers by mean percentile rank of real points:
all_data %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  summarize(mean_rank = mean(percrank)) %>% 
  arrange(mean_rank)

# show cost layers by mean distance-weighted rank of real points:
all_data %>% 
  as_tibble %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  summarize(mean_rank = mean(percrank), dist_weighted_rank = mean(percrank*dist_from_core)) %>% 
  arrange(dist_weighted_rank)

# show cost layers by the ratio of real costs to fake costs:
all_data %>% 
  group_by(cost_layer, real) %>% 
  summarize(cost_sum = sum(cost, na.rm = T)) %>% 
  group_by(cost_layer) %>% 
  summarize(cost_ratio = sum(cost_sum*real)/sum(cost_sum*(!real))) %>% 
  arrange(cost_ratio)

# Experimental 
# try fitting data with a line?
fitted_data <- all_data %>% 
  filter(real == TRUE) %>% 
  group_by(cost_layer) %>% 
  do(fit = nls(percrank ~ a * dist_from_core + b, data = ., start = list(a = 0.5, b = 10))) %>% 
  broom::augment(fit)
qplot(dist_from_core, percrank, data = fitted_data, geom = 'point', colour = cost_layer) +
  geom_line(aes(y=.fitted))


# basic distance vs. cost plot for real and pseudopoints for a single cost layer
ggplot(data = all_data %>% 
         filter(cost_layer == "Constant1.tif") %>% 
         arrange(real)) +
  geom_point(aes(x = dist_from_core, y = cost, color = real, alpha = (0.1 + 0.5*real)))

ggplot(data = all_data %>% 
         filter(cost_layer == "ln.tif") %>% 
         arrange(real)) +
  geom_point(aes(x = dist_from_core, y = cost, color = real, alpha = (0.1 + 0.5*real)))

# Experimental:
ggplot() +
  geom_point(data = all_data %>% 
               filter(cost_layer == "ln.tif") %>% 
               filter(real == TRUE),
             aes(x = dist_from_core, y = cost, alpha = (0.1 + 0.5*real)),
             color = "blue") +
  geom_pointdensity(data = all_data %>% 
                      filter(cost_layer == "ln.tif") %>% 
                      filter(real == FALSE),
                    aes(x = dist_from_core, y = cost),
                    na.rm = TRUE)
                    

# All cost layers, comparing all real and fake points, as a facet graph:
# NOTE: with non-rescaled costs, this will scale the y-axis of each facet plot
# to the largest range. This is not ideal?
all_costs_plot <- ggplot(data = all_data %>% arrange(real)) +
  facet_wrap( ~ cost_layer, ncol = 3) + 
  geom_point(aes(x = dist_from_core, 
                 y = cost, 
                 color = real, 
                 alpha = (0.05 + 0.5*real)),
             size = 0.1) 

all_costs_plot

# All cost layers, comparing percentil rank of real points
# faceted plot
all_percranks_plot <- ggplot(data = all_data %>% 
                               filter(real == TRUE)) +
  facet_wrap( ~ cost_layer, ncol = 3) + 
  geom_point(aes(x = dist_from_core, 
                 y = percrank, 
                 color = real, 
                 alpha = (0.05 + 0.5*real)),
             size = 0.1)
all_percranks_plot



# ggplot(data = all_data %>% filter(cost_layer == "FINALCD_NegExp4.tif")) +
#   geom_point(aes(x = dist_from_core, y = cost, color = real), size = 3.0, alpha = 0.4)
# 
# ggplot(data = all_data %>% filter(cost_layer == "CD_NegLog.tif")) +
#   geom_point(aes(x = dist_from_core, y = cost, color = real), size = 3.0, alpha = 0.4)
