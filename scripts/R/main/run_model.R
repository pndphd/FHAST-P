library(here)

# This assumes you've run load_libraries and default_initializaiton (and
# possibly re-initialized to other inputs).

# Make the daily input file
source(here("scripts","R","daily_inputs","scripts","make_the_input_file.R"))

# Make the daily fish input file
source(here("scripts","R","daily_inputs","scripts","make_fish_input_file.R"))

# Make the parameter fish input file
source(here("scripts","R","fish_parameters","convert_parameters.R"))

# Make the grid 
source(here("scripts","R","river_linear","scripts","Grid_Maker_Run.R"))

# Make the shade shape
source(here("scripts","R","shade_model","scripts","calculate_shade.R"))

# Sample the flow rasters onto the grid
source(here("scripts","R","spatial_inputs","scripts","Sample_Raster_Run.R"))

# Sample the cover shapes onto the grid
source(here("scripts","R","spatial_inputs","scripts","Sample_Shapes_Run.R"))