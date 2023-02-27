########################################
# Sets up the file structure for FHAST run
########################################

initialize_fhast <- function(file_path) {

  # set the main input folders
  fhast_base_folder <<- dirname(file_path)
  if (!dir.exists(fhast_base_folder)) {
    # Directory doesn't exist
    return()
  }

  # Create default file if it doesn't exist
  if (!file.exists(file_path)) {
    # File doesn't exist, make with default values
    write_config_file(
      file_path,
      "base_inputs/fish_population.csv",
      "base_inputs/enviromental_input_dist.txt",
      "base_inputs/fish_params_input.csv",
      "base_inputs/grid_folder/center_line.shp",
      "base_inputs/grid_folder/top_point.shp",
      "base_inputs/cover.shp",
      "base_inputs/canopy.shp",
      "base_inputs/tree_growth_parameters.csv",
      "base_inputs/habitat.txt",
      "base_inputs/interactions.txt",
      "base_inputs/predator_params_input.csv",
      "base_inputs/flow_folder",
      "base_inputs/grid_folder/aoi.shp"
    )
  }

  config_file_path <<- file_path

  # Read in the main input file file to get cores used
  input_data <- load_text_file(file_path)

  # get the name of the input file
  fish_population_path <<- get_path(fhast_base_folder,
                                   input_data["fish population", ])
  daily_path <<- get_path(fhast_base_folder,
                              input_data["daily conditions", ])
  # get the name of the input file
  fish_parameters_path <<- get_path(fhast_base_folder,
                                   input_data["fish parameters", ])
  # get the paths for the 3 files

  grid_center_line_path <<- get_path(fhast_base_folder,
                               input_data["grid centerline", ])
  grid_top_marker_path <<- get_path(fhast_base_folder,
                               input_data["grid top point", ])
  cover_path <<- get_path(fhast_base_folder, input_data["cover", ])
  canopy_path <<- get_path(fhast_base_folder, input_data["canopy", ])
  tree_growth_path <<- get_path(fhast_base_folder, input_data["tree growth", ])
  hab_path <<- get_path(fhast_base_folder, input_data["habitat parameters", ])
  interaction_path <<- get_path(fhast_base_folder,
                               input_data["interaction parameters", ])
  predator_path <<- get_path(fhast_base_folder,
                            input_data["predator parameters", ])

  # get to the aoi path
  aoi_input <- input_data["aoi", ]
  # Checking length is not sufficient (aoi_input can be an array containing a
  # single empty string), so the nzchar check is also needed.
  if (!is.na(aoi_input) && length(aoi_input) > 0 && nzchar(aoi_input)) {
    aoi_path <<- get_path(fhast_base_folder, aoi_input)
  } else {
    aoi_path <<- NA
  }

  # Location of rasters
  raster_folder <<- get_path(fhast_base_folder, input_data["raster folder", ])

  # Reset this stuff to be ready for a new run, the random seed in particular
  # should get set every time.
 
  # Load a color blind friendly pallet
  cbPalette <<- c(
    "#999999", "#0072B2", "#D55E00",
    "#F0E442", "#56B4E9", "#E69F00",
    "#0072B2", "#009E73", "#CC79A7"
  )
  
  # Set default plot width
  plot_widths <<- 5

  # Set the random seed
  set.seed(6806665)
}

write_config_file <- function(file_path, fish_pop, daily, fish_params, line,
                            point, cover, canopy, tree_growth, hab_params,
                            interaction_params, predator, raster, aoi) {
  obj <- data.frame(
    names = c(
      "fish population", "daily conditions",
      "fish parameters", "grid centerline", "grid top point",
      "cover", "canopy", "tree growth", "habitat parameters", "interaction parameters",
      "predator parameters", "raster folder", "aoi"
    ),
    paths = c(
      fish_pop, daily, fish_params, line, point,
      cover, canopy, tree_growth, hab_params, interaction_params, predator,
      raster, aoi
    )
  )
  save_text_file(file_path, obj)
}


