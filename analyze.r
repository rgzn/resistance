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
    dplyr::select(-core_exit)
}

# get values of stars object at points from sf objects
st_sample_by_sf = function(stars_object, sf_object) {
  stars_object %>% 
    aggregate(sf_object,
              function(x) x[1], 
              as_points = FALSE) %>% 
    st_as_sf %>% 
    st_intersection(sf_object)
}

# rotate sf object
st_rotate = function(sf_object, rot_angle, units = "degrees", center = NULL) {
  
  ifelse(units == "degrees", pi/180, 1) ->
    k
  make_rotation_matrix = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  rot_M = make_rotation_matrix(rot_angle * k)
  
  if(is.null(center)) { 
    center = sf_object %>% 
      st_union() %>% 
      st_centroid
  }
  
  # (sf_object - center) * rot_M + center
  sf_object %>% 
    mutate(geometry = (geometry - center) * rot_M + center )
}


negexp_resist = function(x, b) {
  # 1 - (1 - exp(-b * x) / (1 - exp(-b)))
  x * (-b) %>% 
    exp()*-1 %>%
    `+`(1) %>% 
    `/`(1 - exp(-b)) %>% 
    `*`(-1) %>% 
    `+`(1)
}

get_excursions_with_buffers = function(points, polygon) {
  
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
              net_dist = max(distance)) %>% 
    ungroup()
  
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
  
  # find endpoints of each excursions:
  excursion_endpoints <-
    animal_polygons %>%
    right_join(as_tibble(excursion_points), by = c("AnimalID")) %>%
    rename(geometry.core = geometry.x, 
           geometry.point = geometry.y) %>% 
    mutate(dist = st_distance(geometry.core, geometry.point, by_element = TRUE)) %>%
    group_by(AnimalID, exit_event) %>%
    arrange(desc(dist)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(max_dist = dist,
           geometry.endpoint = geometry.point)
  
  # find buffering polygons for each excursions:
  excursion_buffers <-
    excursion_endpoints %>%
    mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist)) %>%
    mutate(geometry.buffer = st_buffer(geometry.core, dist = max_dist))
  
  # merge excursion data with buffer data:
  excursions_with_buffers <-
    excursion_buffers %>%
    inner_join(as_tibble(excursions), by = c("AnimalID", "exit_event")) %>%
    rename(geometry.points = geometry) %>% 
    select(AnimalID, 
           datetime, 
           exit_event,
           start_time,
           end_time,
           duration,
           n_points,
           max_dist, 
           net_dist,
           geometry.core,
           geometry.buffer,
           geometry.endpoint,
           geometry.points) %>%
    filter(n_points > 1)
  
  
  
  # find maximum excursion distance for each event:
  # event_max_dist <- 
  #   event_polygons %>% 
  #   as_tibble() %>% 
  #   right_join(excursion_points, by = c("AnimalID", "exit_event")) %>% 
  #   mutate(dist = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>% 
  #   group_by(AnimalID, exit_event) %>% 
  #   summarize(max_dist = max(dist)) %>% 
  #   mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist))
  
  # event_max_dist <- 
  #   animal_polygons %>% 
  #   as_tibble() %>% 
  #   right_join(excursion_points, by = c("AnimalID")) %>% 
  #   mutate(dist = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>% 
  #   group_by(AnimalID, exit_event) %>% 
  #   summarize(max_dist = max(dist)) %>% 
  #   mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist))
  
  # animal_polygons %>% 
  #   as_tibble() %>% 
  #   right_join(excursion_points, by = c("AnimalID")) %>% 
  #   mutate(dist = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>% 
  #   group_by(AnimalID, exit_event) %>% 
  #   filter(rank(dist) == 1) %>% 
  #   ungroup()
  
  # create buffers for each event from the core to max dist:
  # event_buffers <- 
  #   animal_polygons %>%
  #   right_join(event_max_dist) %>% 
  #   mutate(max_dist = ifelse(is.na(max_dist), 0, max_dist)) %>% 
  #   mutate(geometry = st_buffer(geometry, dist = max_dist)) 
  
  # excursions_with_buffers <- 
  #   event_buffers %>% 
  #   inner_join(as_tibble(excursions), by = c("AnimalID", "exit_event")) %>% 
  #   rename(geometry.buffer = geometry.x,
  #          geometry.points = geometry.y)
  # 
  # excursions_with_buffers <-
  #   excursions_with_buffers %>% 
  #   filter(n_points > 1)
  # 
  # ggplot() +
  #   geom_sf(data = excursions_with_buffers, aes(fill = as.factor(exit_event)), alpha = 0.2)
  # 
  # ggplot() + 
  #   geom_sf(data = event_buffers,  aes(color = as.factor(exit_event))) + 
  #   geom_sf(data = excursion_points,  aes(color = as.factor(exit_event)))
}
