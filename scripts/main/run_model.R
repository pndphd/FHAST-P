########################################
# Runs all the scripts for FHAST calculations
#
# This assumes you've run load_libraries and default_initializaiton
# (and possibly re-initialized to other inputs).
########################################

# make output locations 
source(here("scripts","main","make_output_folder.R"))

# load all the input files, make basic parameter files,
# also do some basic checking of files
source(here("scripts","format_parameters","load_convert_parameters.R"))

# Make the daily input file
source(here("scripts","daily_inputs","make_enviro_input_file.R"))

# Make the daily fish input file
source(here("scripts","daily_inputs","make_fish_input_file.R"))

# Make preview map
source(here("scripts","cover","make_preview_map.R"))

# Make the grid 
source(here("scripts","river_grid","grid_maker.R"))

# Calculate shade
shadeError <<- FALSE
tryCatch(source(here("scripts","shade","calculate_shade.R")),
         error = function(e) {
           print(e)
           shadeError <<- TRUE
         })
if (shadeError) {
  stop('Error while generating shade, check canopy cover shape file. The canopy shape file has two attributes per shape, a height, and an optional species (used for growth). The height is used in the shade calculations and the species is used when the tree growth module is used to determine the change in the height of the new shape and the area that new shape covers. ')
}

# Sample the flow rasters onto the grid
source(here("scripts","depth_velocity","sample_raster.R"))

# Sample the cover shapes onto the grid
source(here("scripts","cover","sample_shapes.R"))
