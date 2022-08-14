##### Description #####
# This script runs the necessary functions to make and save a rive grid

# Load Libraries and some base parameters
source("./scripts/R/main/load_libraries.R")

# load functions used in the script
source("./scripts/R/river_linear/scripts/Grid_Maker_Functions.R")
source("./scripts/R/river_linear/scripts/Map_Maker_Functions.R")

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

# get the paths for the 2 files 
center_line = paste0(input_folder,
                     input_data["folder",],
                     "/grid/",
                     input_data["line",])  
top_marker = paste0(input_folder,
                    input_data["folder",],
                    "/grid/",
                    input_data["point",])

shape_files = load_input_files(line = center_line ,
                             top = top_marker)

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
saveRDS(grid, paste0(temp_folder, "R/river_grid_", resolution, "_", max_buffer, ".rds"))
write_sf(grid, paste0(temp_folder, "R/river_grid_", resolution, "_", max_buffer, ".shp"),
         driver ="ESRI Shapefile")

# Write a file for netlogo to read resolution
netlogo_resolution = data.frame(resolution = resolution,
                                buffer = max_buffer)
write.csv(netlogo_resolution, paste0(temp_folder, "NetLogo/resolution.csv"))

##### make plots #####
make_leaflet_map(grid, "poly")

