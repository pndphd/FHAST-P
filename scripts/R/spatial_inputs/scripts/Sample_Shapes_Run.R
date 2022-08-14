##### Description #####
# This runs the scripts and functions to sampel shape filees witht he grid

##### Inputs #####
# Load Libraries and some base parameters
source("./scripts/R/main/load_libraries.R")

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
river_grid = readRDS(paste0(temp_folder, "R/river_grid_", input_data["resolution",],
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
          paste0(temp_folder,
                 "NetLogo/Shape_Data_Input_", resolution,"_", max_buffer, ".csv"),
          na = "-9999",
          row.names = FALSE)


  
