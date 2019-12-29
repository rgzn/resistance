library(tidyverse)
library(gdistance)
library(raster)
library(stars)

# random number generator seed
set.seed(Sys.time())
project_crs = crs("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")

# read RSF

rsf_file = "./rasters/rsf/pRasout_20190613.tif"
# costdistance_path = "./rasters/costdistance/"

# rsf = raster(rsf_file)

rsf_stars = read_stars(rsf_file, proxy = TRUE)
ggplot() +
  geom_stars(data = rsf_stars, downsample = 10)

# core range polygons:
core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
core_range = core_range_filename %>%
  st_read %>% 
  st_transform(project_crs)

# herd units:
herd_units_filename = "./shapefiles/All_SNBS_HerdUnits.shp"
herd_units = herd_units_filename %>% 
  st_read %>% 
  st_transform(project_crs)

# recovery units:
herd_units %>% 
  group_by(GCU) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() ->
  recovery_units

# sanity check plot:
ggplot() + 
  geom_stars(data = rsf_stars, downsample = 20) + 
  geom_sf(data = core_range, fill = "red", alpha = 0.6) +
  geom_sf(data = recovery_units, aes(color = GCU), size = 1, alpha = 0.2)




# max distance in meters
MAX_DISTANCE = 30000

# construct mask to reduce memory use:
range_mask_sf = core_range %>% 
  st_buffer(dist = MAX_DISTANCE) %>% 
  st_buffer(dist = 4000)

RU_masks_sf = recovery_units %>% 
  group_by(GCU) %>% 
  st_buffer(dist = MAX_DISTANCE) %>% 
  ungroup

x = rsf_stars[RU_masks_sf %>% filter(GCU == "Northern")] 
ggplot() + geom_stars(data = x %>% neglog_resist(), downsample = 20)

rsf_stars[RU_masks_sf %>% filter(GCU == "Northern")] %>% 
  neglog_resist() %>% 
  as("Raster")

as(x, "Raster")

range_mask = range_mask_sf %>% 
  as_Spatial

# mask rsf:
rsf_masked  = rsf %>% 
  crop(extent(range_mask)) %>% 
  mask(range_mask)

rsf_stars_masked = rsf_stars[range_mask_sf]

ggplot() + 
  geom_stars(data = rsf_stars_masked, downsample = 16) + 
  geom_sf(data = core_range, fill = "red", alpha = 0.6) +
  geom_sf(data = recovery_units, aes(color = GCU), size = 1, alpha = 0.2)


# make resistances from RSF

neglog_resist = function(x) {
  -log(x)
}

negexp_resist = function(x, b) {
  1 - (1 - exp(-b * x) / (1 - exp(-b)))
}

const_resist = function(x) {
  x/x
}

resistance_functions = list(neglog_resist, negexp_resist, const_resist)

# generate cost-distance layer using LCP
# rsf_resistance = neglog_resist(rsf_masked)
rsf_resistance = neglog_resist(rsf)

# rsf_transistions = gdistance::transition(rsf_resistance, transitionFunction = mean, directions = 8)
rsf_transistions = gdistance::transition(rsf_resistance, 
                                         transitionFunction = function(x) {1/mean(x)},
                                         directions = 8)
  