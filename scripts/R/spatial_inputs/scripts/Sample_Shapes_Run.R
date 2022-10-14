##### Description #####
# This runs the scripts and functions to sampel shape filees witht he grid

##### Inputs #####
# Load Libraries and some base parameters
library(here)
source(here("scripts","R","main","load_libraries.R"))

# Load the function to sample shapes on the grid
source(here("scripts","R","spatial_inputs","scripts","Sample_Shapes_Functions.R"))

##### Load Files #####

# Get the res values
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

# load the river grid
river_grid = readRDS(here(temp_folder, "R",paste0("river_grid_", resolution,
                            "_", max_buffer, ".rds")))

# Load the cover file
cover_shape = st_read(cover_file, quiet = TRUE) 
# Load the canopy cover zone file
canopy_shape = st_read(canopy_cover_file, quiet = TRUE) 
# Load the vegetation bank file
# Load the shade file
shade_file = readRDS(here(temp_folder, "R", "shade_file.rds")) 

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

shapes_csv = sampled_to_csv(sampeled_shapes)

##### Save Outputs #####
write.csv(shapes_csv,
          here(temp_folder,"NetLogo", paste0("Shape_Data_Input_", resolution,"_", max_buffer, ".csv")),
          na = "-9999",
          row.names = FALSE)


  
