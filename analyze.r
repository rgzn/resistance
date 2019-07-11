library(tidyverse)
library(sf)
library(stars)


# find excursions outside of polygon
# function for single animal
excursion = function(points, polygon) {
  points %>% 
    mutate(in_poly = t(st_contains(y = ., x = polygon, sparse = FALSE))) %>% 
    mutate(exit = lag(in_poly) - in_poly) %>% 
    mutate(exit = replace_na(exit, 0)) %>% 
    mutate(exit_event = cumsum( pmax(exit, 0))) %>% 
    select(-exit)
}

# label excursions outside of homerange
# function for multiple animals
label_excursions = function(points, polygon) {
  points %>%
    group_by(AnimalID) %>%
    excursion(polygon)
    ungroup()
}



st_sample_by_sf = function(stars_object, sf_object) {
  stars_object %>% 
    aggregate(sf_object,
              function(x) x[1], 
              as_points = FALSE) %>% 
    st_as_sf %>% 
    st_intersection(sf_object)
}

st_sample_sf = function(sf_object, stars_object) {
  stars_object %>% 
    aggregate(sf_object,
              function(x) x[1], 
              as_points = FALSE) %>% 
    st_as_sf %>% 
    st_intersection(sf_object)
}

filter_by_event_duration = function(.data, group_col, min = 0, max = Inf) {
  group_col = enquo(group_col)
  
  min_duration = dseconds(min)
  max_duration = dseconds(max)
  .data %>% 
    group_by(!!group_col) %>%
    summarise(duration =  as.duration(min(datetime) %--% max(datetime))) %>%
    filter(duration >= min_duration, 
           duration <= max_duration)
}

