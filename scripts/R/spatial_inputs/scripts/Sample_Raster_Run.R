##### Description #####
# This file runs the functions to sample rasters onto a grid 

##### Inputs #####
# Load Libraries and some base parameters
library(here)
source(here("scripts","R","main","load_libraries.R"))

# Make sure the area function is raster::area 
area = raster::area

# load the functions for this script
source(here("scripts","R","spatial_inputs","scripts","Sample_Raster_Functions.R"))


# load the res files
res_file <- read.csv(file = grid_res_path,
                     sep = "=",
                     row.names = 1,
                     header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# load the river grid
river_grid = readRDS(here(temp_folder, "R",paste0("river_grid_", res_file["resolution",],
                            "_", res_file["buffer",], ".rds")))

# load the flow list

hab_file <- read.csv(file = hab_path,
                     sep = "=",
                     row.names = 1,
                     header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# read the flow values
flows = as.numeric(strsplit(substr(hab_file["flows",],
                           1,
                           nchar(hab_file["flows",])), ',')[[1]])

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
v_stack = stack(map(flows,~raster(here(raster_folder, paste0("V", .x, ".tif")))))
d_stack = stack(map(flows,~raster(here(raster_folder, paste0("D", .x, ".tif")))))
if (!(compareCRS(v_stack, d_stack) &
      compareCRS(river_grid, d_stack))) {
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
          here(temp_folder, "NetLogo", paste0("Depth_Velocity_Data_Input_",
                 res_file["resolution",],
                 "_", res_file["buffer",], ".csv")),
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