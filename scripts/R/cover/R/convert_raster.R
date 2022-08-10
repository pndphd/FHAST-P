options(stringsAsFactors = FALSE)
library(raster)
library(tidyverse)
library(inborutils)
library(here)
library(sf)
library(stars)


path <- here("data/esri_cover.tif")

tif <- read_stars(path)
sf <- st_as_sf(tif, as_points = FALSE, merge = FALSE)
