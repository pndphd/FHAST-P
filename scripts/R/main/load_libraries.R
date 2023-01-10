########################################
# This script checks for all necessary libraries (packages) to 
# run the R parts of FHAST. If not installed it installs them.
# It then loads the libraries and some color pallets
########################################

# Uncomment this next source command if you want to install
# inbornutils (this is not necessary to run FHAST).
# source("./scripts/R/main/load_inborutils.R")

# Make a function to insatll packages/libraries
install_all = function(package){
  if(!require(package, character.only = T)){
    install.packages(package)
  }
}

# This is a list of packages to check for and install any missing ones
packages = c(
  # Control Libraries
  "tictoc",
  "here",
  "fs",
  "knitr",
  "R.utils",
  "tools",
  
  # Data manipulate libraries
  "tidyverse",
  "dplyr",
  "stringr",
  "lubridate",
  "data.table",
  
  # Programming and processing
  "purrr",
  "furrr",
  "parallel",
  "igraph", # for adult pathfinding algorithm
  
  # GIS libraries
  "sf",
  "lutz", # used to get time zones
  "shadow",
  "maptools",
  "smoothr",
  "exactextractr",
  "raster",
  "terra",
  
  # Data viz libraries 
  "ggplot2",
  "viridis",
  "patchwork",
  "leaflet"
)

# install any missing packages and load all packages
lapply(X = packages, FUN = function(x) install_all(x))

# make sure select is the dplyr call not the raster package call
select <- dplyr::select
