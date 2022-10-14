##### Description #####
# This script runs the necessary functions to make and save a rive grid

library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# load functions used in the script
source(here("scripts","R","river_linear","scripts","Grid_Maker_Functions.R"))
source(here("scripts","R","river_linear","scripts","Map_Maker_Functions.R"))


# load the files
res_file <- read.csv(file = grid_res_path,
                    sep = "=",
                    row.names = 1,
                    header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# Get some variable values
resolution = as.numeric(res_file["resolution",])
max_buffer = as.numeric(res_file["buffer",])

# Load the shape files
shape_files = load_input_files(line = grid_center_line ,
                               top = grid_top_marker)

##### Pre Processing #####
# get a list of distances for buffers
distances_list = make_distances_list(resolution = resolution,
                                     buffer = max_buffer)

##### Main Work #####
# Make the buffers which are the lateral grid dividers
# This next commented line will filter for only polygons
buffers = make_buffers(distances = distances_list,
                       line = shape_files$line)

# Make a file to tell left from right bank
large_buffer = make_large_buffer(distances = distances_list,
                                 line = shape_files$line)

# Place sample points along the line
sample_points = make_sample_points(resolution = resolution,
                                   line = shape_files$line)

# Make the Voronoi cells
vor_cells = make_vor_cells(points = sample_points,
                           top = shape_files$top,
                           resolution = resolution)

# Combine the buffers and vornoi cells to make the grid
grid = make_grid(resolution = resolution,
                 cells = vor_cells,
                 buffers = buffers,
                 large_buffer = large_buffer) %>% 
  # set left and right bank correctly
  mutate(lat_dist = ifelse(lat_dist>0, 
                           ifelse(left_or_right<0, lat_dist, -lat_dist), 0))

##### Save Outputs #####
saveRDS(grid, here(temp_folder, "R", paste0("river_grid_", resolution, "_", max_buffer, ".rds")))

# rename some things to avoid a warning
grid_save = grid %>% 
  rename(dist = distance,
         l_or_r = left_or_right)
write_sf(grid_save, here(temp_folder, "R",paste0("river_grid_", resolution, "_", max_buffer, ".shp")),
         driver ="ESRI Shapefile")

# Write a file for netlogo to read resolution
netlogo_resolution = data.frame(resolution = resolution,
                                buffer = max_buffer)
write.csv(netlogo_resolution, here(temp_folder, "NetLogo","resolution.csv"))

##### make plots #####
make_leaflet_map(grid, "poly")

