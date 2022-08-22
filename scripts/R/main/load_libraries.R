# This script checks for all necessary libraries (packages) to 
# run the R parts of FHAST. If not installed it installs them.
# It then loads the libraries and some color pallets

# Uncomment this next source command if you want to install
# inbornutils (this is not necessary to run FHAST).
# source("./scripts/R/main/load_inborutils.R")

# Need to load this first
library(here)

# set the main input folders
input_folder = here("..","input_data")
input_file = "input_file.txt"
temp_folder = here("temporary")

# Load a color blind friendly pallet
cbPalette <- c("#999999", "#0072B2", "#D55E00",
               "#F0E442", "#56B4E9", "#E69F00",
               "#0072B2", "#009E73", "#CC79A7")

# Set the random seed
set.seed(6806665)

# Make a function to insatll packages/libraries
install_all = function(package){
  if(!require(package, character.only = T)){
    install.packages(package)
  }
}

# This will check for the necessary packages and install any missing ones
packages = c(
  # Control Libraries
  "tictoc",
  "here",
  
  # Data manipulate libraries
  "tidyverse",
  "dplyr",
  "stringr",
  "lubridate",
  
  # Programming and processing
  "purrr",
  "furrr",
  "parallel",
  
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

# install any missing packages an load all
lapply(X = packages, FUN = function(x) install_all(x))

select = dplyr::select

# Read in the main input file file to get cores used
input_data <- read.csv(file = here(input_folder, input_file),
                       sep = "=",
                       row.names = 1,
                       header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# Setup furrr

source(file = here("scripts", "R", "main", "num_cores_func.R"))
num_cores <- pick_num_cores()

future::plan(strategy = multisession,
             workers = num_cores)

# plan("future::multisession")
future.seed = FALSE
