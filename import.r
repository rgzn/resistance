library(tidyverse)
library(sf)
library(lubridate)
library(stars)

# Vdt_from_date_time_str
# make datetime object from date string and time string
dt_from_date_time_str = function (datestr, 
                                  timestr, 
                                  date_format = "%Y%m%d", 
                                  time_format = "%H:%M:%S"
) {
  dt_str = paste(datestr, timestr)
  dt_format = paste(date_format, time_format)
  dt = as.POSIXct(dt_str, format = dt_format)
  return(dt)
}

# read_snbs_shapefile
# read a snbs formatted position shapefile
read_snbs_shapefile = function(path) {
  snbs_data = sf::st_read(path)
  snbs_data %>%
    mutate(datetime = dt_from_date_time_str(Date, Time)) %>%
    select(datetime,
           AnimalID,
#           UTM_E,
#           UTM_N,
#           Method,
           DOP,
           SV,
#           Fix,
#           CSNoDate,
           HU,
#           RU,
           Sex,
           geometry
    )
}

read_rasters = function(path, raster_type = "tif", proxy = FALSE, along = NA) {
  file_pattern = paste0("*.", raster_type, "$")
  raster_files = list.files(path, pattern = file_pattern, full.names = TRUE)
  rasters = read_stars(raster_files, proxy = proxy, along)
  return(rasters)
}

