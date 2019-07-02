library(tidyverse)
library(sf)
library(stars)

# label excursions outside of homerange
label_excursions = function(points, polygon) {
  points %>%
    mutate(in_core = t(st_contains(y = ., x = polygon, sparse = FALSE))) %>%
    group_by(AnimalID) %>%
    mutate(core_exit = lag(in_core) - in_core) %>%
    mutate(core_exit = replace_na(core_exit, 0)) %>%
    mutate(exit_event = cumsum( pmax(core_exit,0))) %>%
    ungroup() %>%
    select(-core_exit)
}


st_sample_by_sf = function(stars_object, sf_object) {
  stars_object %>% 
    aggregate(sf_object,
              function(x) x[1], 
              as_points = FALSE) %>% 
    st_as_sf %>% 
    st_intersection(sf_object)
}
