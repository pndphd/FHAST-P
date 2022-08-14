##### Description #####
# This file runs the functions to sample rasters onto a grid 

##### Inputs #####
# Load Libraries and some base parameters
source("./scripts/R/main/load_libraries.R")

# Make sure the area function is raster::area 
area = raster::area

# load the functions for this script
source("./scripts/R/spatial_inputs/scripts/Sample_Raster_Functions.R")

##### Load Files #####
# Read in the main input file file
input_data <- read.csv(file = paste0(input_folder, input_file),
                       sep = "=",
                       row.names = 1,
                       header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# load the river grid
river_grid = readRDS(paste0(temp_folder, "R/river_grid_", input_data["resolution",],
                            "_", input_data["buffer",], ".rds"))

# Location of rasters
raster_folder = paste0(input_folder, 
                       input_data["folder",],
                       "/flow/")

# read the flow values
flows = as.numeric(strsplit(substr(input_data["flows",],
                           1,
                           nchar(input_data["flows",])), ',')[[1]])


##### Main Part #####
# Put all the rasters in a stack

raster_stack_d = load_rasters(type = "depth",
                              folder = raster_folder,
                              flows = flows)

# Put all the rasters in a stack
raster_stack_v = load_rasters(type = "velocity",
                              folder = raster_folder,
                              flows = flows)

# Check that all CRSs are the same
if (!(compareCRS(raster_stack_v, raster_stack_d) &
      compareCRS(river_grid, raster_stack_d))) {
  stop('The CRSs of some of your files are not the same.')
}

# Check if there ar more than 1 flow
if (length(flows) < 2) {
  stop('You must enter at least 2 flow values.')
}

# Sample the grid over the raster stack
sampeled_grid_d = sample_grid(stack = raster_stack_d,
                              grid = river_grid,
                              type = "depth")

# Sample the grid over the raster stack
sampeled_grid = sample_grid(stack = raster_stack_v,
                            grid = river_grid,
                            type = "velocity") %>% 
  left_join(sampeled_grid_d, by = c("lat_dist", "distance", "area"))

##### Save Outputs #####
# write the data
write.csv(sampeled_grid,
          paste0(temp_folder, "NetLogo/Depth_Velocity_Data_Input_",
                 input_data["resolution",],
                 "_", input_data["buffer",], ".csv"),
          na = "0", 
          row.names = FALSE)

##### Make Plots #####
if(0){
  # Plot and example
  stats_plot = sampeled_grid %>% 
    mutate(depth = ifelse(is.nan(mean.D300),NA, mean.D300)) 
  g = ggplot(stats_plot, aes(x = x , y = y , fill = depth )) +
    theme_classic() +
    geom_raster(na.rm = TRUE)
  g
  
  # Plot the bottom difference
  stats_plot = sampeled_grid %>% 
    mutate(difference = ifelse((bottom_area-area) == 0, NA, (bottom_area-area)))
  g = ggplot(stats_plot, aes(x = x , y = y , fill = (difference) )) +
    theme_classic() +
    geom_raster(na.rm = T)
  g
}