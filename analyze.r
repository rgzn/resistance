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
    ungroup()
}