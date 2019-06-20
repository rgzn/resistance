library(tidyverse)
library(sf)
library(amt)
library(lubridate)


## Data Import
ram_data_filename = "./shapefiles/AllRams2004_2016.shp"
ram_data = st_read(ram_data_filename)

ram_data = ram_data %>% mutate(datetime = 
           as.POSIXct(paste(as.character(Date), Time), 
                      format ="%Y%m%d %H:%M:%S")) %>%
  select(datetime, 
         AnimalID,
         UTM_E,
         UTM_N,
         Method,
         DOP,
         SV,
         Fix,
         CSNoDate,
         HU,
         RU,
         Sex, 
         geometry
         )


herd_units_filename = "./shapefiles/All_SNBS_HerdUnits.shp"
herd_units = st_read(herd_units_filename)
herd_units = st_transform(herd_units, 32611)

core_range_filename = "./shapefiles/Kernels_Merged_Dissolved.shp"
core_range = st_read(core_range_filename)

test_polygon_file = "./shapefiles/wheeler_polygon.json"
st_layers(test_polygon_file)
test_polygon = st_read(test_polygon_file)
test_polygon = st_transform(test_polygon, 32611)

## Filters

ram_data %>% 
  filter(!st_contains(y = ., x = core_range, sparse = FALSE))

ram_data %>% 
  mutate(in_core = st_contains(y = geometry, x = core_range, sparse = FALSE))

d = ram_data %>%
  mutate(in_core = t(st_contains(y = ., x = core_range, sparse = FALSE))) %>%
  group_by(AnimalID) %>%
  mutate(core_exit = in_core - lag(in_core)) %>%
  mutate(core_exit = replace_na(core_exit, 0)) %>%
  filter(core_exit >= 0 ) %>%
  mutate(exit_event = as.integer(cumsum(core_exit))) %>%
  ungroup()

s20_data = d %>% filter(AnimalID == "S20")

.# only data > certain lat
north_data = ram_data %>% arrange(desc(UTM_N)) %>% slice(1)


  ## Plots
theme_set(theme_minimal())

ggplot() + 
  geom_sf(data = test_polygon, alpha = 0.5) +
  geom_sf(data = wh_data, 
          aes(color = AnimalID))

ggplot() + 
  geom_sf(data = test_polygon, alpha = 0.5, fill = "green") +
  geom_sf(data = my_point, size = 10)

st_contains(x = my_point, y = test_polygon, sparse = FALSE)
st_contains(x = test_polygon, y = my_point, sparse = FALSE)


ggplot() + 
  geom_sf(data = core_range, alpha = 0.2) +
  geom_sf(data = s20_data %>% filter(!in_core), aes(color = as.factor(exit_event))) + 
  coord_sf(xlim = c(292000, 308000), ylim = c(4200000, 4230000))

ggplot(data = herd_units) + 
  geom_sf(aes(fill = GCU))

