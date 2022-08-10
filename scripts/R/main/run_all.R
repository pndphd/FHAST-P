##########
# This is just a series or source calls of the scripts
##########

# Make the daily input file
source("./scripts/R/daily_inputs/scripts/make_the_input_file.R")

# Make the grid
source("./scripts/R/river_linear/scripts/Grid_Maker_Run.R")

# Make the shade shape
source("./scripts/R/shade_model/scripts/calculate_shade.R")

# Sample the flow rasters onto the grid
source("./scripts/R/spatial_inputs/scripts/Sample_Raster_Run.R")

# Sample the cover shapes onto the grid
source("./scripts/R/spatial_inputs/scripts/Sample_Shapes_Run.R")
