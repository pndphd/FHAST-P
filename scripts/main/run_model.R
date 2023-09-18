########################################
# Runs all the scripts for FHAST calculations
#
# This assumes you've run load_libraries and default_initializaiton
# (and possibly re-initialized to other inputs).
########################################

# make output locations 
message("Making output folders.\n")
source(here("scripts","main","make_output_folder.R"))
message("Making output folders: Done.\n")

# load all the input files, make basic parameter files,
# also do some basic checking of files
message("Loading parameters.\n")
source(here("scripts","format_parameters","load_convert_parameters.R"))
message("Loading parameters: Done.\n")

# Make the daily input file
message("Making daily environment file.\n")
source(here("scripts","daily_inputs","make_enviro_input_file.R"))
message("Making daily environment file: Done.\n")

# Make the daily fish input file
message("Making daily fish file.\n")
source(here("scripts","daily_inputs","make_fish_input_file.R"))
message("Making daily fish file: Done.\n")

# Make preview map
message("Making preview map.\n")
source(here("scripts","cover","make_preview_map.R"))
message("Making preview map: Done.\n")

# Make the grid 
message("Making model grid.\n")
source(here("scripts","river_grid","grid_maker.R"))
message("Making model grid: Done.\n")

# Calculate shade
message("Making shade file.\n")
shadeError <<- FALSE
tryCatch(suppressWarnings(source(here("scripts","shade","calculate_shade.R"))),
         error = function(e) {
           print(e)
           shadeError <<- TRUE
         })
if (shadeError) {
  stop('Error while generating shade, check canopy cover shape file. The canopy shape file has two attributes per shape, a height, and an optional species (used for growth). The height is used in the shade calculations and the species is used when the tree growth module is used to determine the change in the height of the new shape and the area that new shape covers. ')
}
message("Making shade file: Done.\n")

# Sample the flow rasters onto the grid
message("Sampling rasters.\n")
source(here("scripts","depth_velocity","sample_raster.R"))
message("Sampling rasters: Done.\n")

# Sample the cover shapes onto the grid
message("Sampling shape files.\n")
source(here("scripts","cover","sample_shapes.R"))
message("Sampling shape files: Done.\n")
message("Starting NetLogo run.\n")


