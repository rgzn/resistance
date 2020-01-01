library(tidyverse)
library(sf)
library(lubridate)
library(stars)
# library(raster)


# internal source files:
source("analyze.r")
source("rsf2cost_functions.r")

# Input RSF:
rsf_path = "rasters/rsf/"
rsf_filename = "pRasout_20190613.tif"
rsf = read_stars(paste0(rsf_path, rsf_filename), proxy = TRUE)
attributes(rsf)$names <- "rspf"

# rspf to cost transformations:
# transformations is a LIST of FUNCTIONS
transformations = c(constant_transform,
                    neglog_tranform,
                    make_negexp(b = 2),
                    make_negexp(b = 16),
                    make_negexp(b = 32))

# Cost layers:
cost = rsf
attributes(cost)$names <- "cost"
cost1 = -log(cost)
cost2 = (exp(cost*(-16)) - 1)/(1-exp(-16)) + 1

ggplot() + geom_stars(data = cost2, downsample = 20)
