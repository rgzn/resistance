library(tidyverse)
library(sf)
# library(amt)
library(lubridate)
library(stars)


## Data Import

# Vector Data:
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

# Raster Data:

# risk_tif = "./rasters/FINAL_CD_Negate_pRasout_20190613_ln_Clip60km_Rescale1_100_Invert.tif"
# risk = read_stars(risk_tif, proxy = TRUE)
# plot(risk)
# rm(risk)

rsf_tif = "./rasters/rsfMap_20150316.tif"
rsf = read_stars(rsf_tif, proxy = TRUE)
plot(rsf)
str(rsf)



## ggplot() + geom_stars(data = risk)
  

## Filters

# label excursions outside of homerange
# how to make this into function using map???
d = ram_data %>%
  mutate(in_core = t(st_contains(y = ., x = core_range, sparse = FALSE))) %>%
  group_by(AnimalID) %>%
  mutate(core_exit = lag(in_core) - in_core) %>%
  mutate(core_exit = replace_na(core_exit, 0)) %>%
  mutate(exit_event = cumsum( pmax(core_exit,0))) %>%
  ungroup()


s20_data = d %>% filter(AnimalID == "S20")
         
  
.# only data > certain lat
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
