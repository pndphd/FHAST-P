##### Description #####
# This runs the scripts and functions to sampel shape filees witht he grid

##### Inputs #####
# the location of the main input file
input_folder = "../input_data/"
input_file = "input_file.txt"
# Location of grid
grid_folder = "./temporary/R/"
# Where to save the output
output_folder = "./temporary/NetLogo/"

##### Libraries #####
# the simple features library for most of the shape file stuff
library(sf)
# leaflet for plotting shapefiles
library(leaflet)
# deal with most of the rater calculations
library(raster)
# you know why
library(tidyverse)
# to smooth the center line of the river
library(smoothr)
# the viridis color map
library(viridis)
# a faster way to do raster sampling with shape files
library(exactextractr)
# Library to parallelize purrr
library(furrr)
# Library to join graphs
library(patchwork)

# Make sure dplyr select is the default
select = dplyr::select

# Load the function to sample shapes on the grid
source("./scripts/R/spatial_inputs/scripts/Sample_Shapes_Functions.R")

##### Load Files #####
# Read in the main input file file
input_data <- read.csv(file = paste0(input_folder, input_file),
                       sep = "=",
                       row.names = 1,
                       header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# get the values
resolution = as.numeric(input_data["resolution",])
max_buffer = as.numeric(input_data["buffer",])

# load the river grid
river_grid = readRDS(paste0(grid_folder, "river_grid_", input_data["resolution",],
                            "_", input_data["buffer",], ".rds"))

# Load the cover file
cover_shape = st_read(paste0(input_folder, 
                             input_data["folder",],
                             "/cover/",
                             input_data["cover file",]), quiet = TRUE) 
# Load the canopy cover zone file
canopy_shape = st_read(paste0(input_folder, 
                              input_data["folder",],
                              "/cover/",
                              input_data["canopy cover",]), quiet = TRUE) 
# Load the vegetation bank file
# Load the shade file
shade_file = readRDS("./temporary/R/shade_file.rds") 

# Setup furrr
future::plan(multisession, workers = as.numeric(input_data["cores used",]))
future.seed = FALSE

##### Main Work #####
# make a list of files and variabel names
cover_names = list("veg", "wood", "fine", "gravel", "cobble", "rock")

# Get a list of data frames with just one for each df that 
shape_dfs = map(cover_names, ~select(cover_shape, matches(.x)))

the_variables = c(cover_names, as.list(paste0("shade_", seq(1,12,1))))
shape_files = c(shape_dfs, shade_file)
                  
# Sample all the shapes over the grid
sampeled_shapes = sample_all_shapes(river_grid,
                                    shape_files,
                                    the_variables,
                                    the_variables)

shapes_csv = sampeled_to_csv(sampeled_shapes)

##### Save Outputs #####
write.csv(shapes_csv,
          paste0(output_folder,
                 "Shape_Data_Input_", resolution,"_", max_buffer, ".csv"),
          na = "-9999",
          row.names = FALSE)


  
