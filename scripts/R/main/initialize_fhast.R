library(here)
library(dplyr)
library(R.utils)

initialize_fhast <- function(folder){
  # set the main input folders
  input_folder <<- here(folder)
  
  # Read in the main input file file to get cores used
  input_data <- read.csv(file = here(input_folder, fhast_config_file),
                         sep = "=",
                         row.names = 1,
                         header = FALSE) %>% 
    # Trim off white spaces form values
    rename(value = 1) %>% 
    mutate(value = str_trim(value, side = c("both")))
  
  project_folder <<- ifelse(isAbsolutePath(input_data["folder",]),input_data["folder",], here(input_folder, input_data["folder",]))
  
  #get the name of the input file
  fish_population_file <<- ifelse(isAbsolutePath(input_data["fish population file",]),input_data["fish population file",], 
                                  here(input_folder,
                                   input_data["folder",],
                                   "fish",
                                   input_data["fish population file",]))
  
  daily_file_name <<- ifelse(isAbsolutePath(input_data["daily file",]),input_data["daily file",], 
                             here(input_folder, input_data["folder",], "daily", input_data["daily file",]))
  
  # get the name of the input file
  fish_parameters_file <<- ifelse(isAbsolutePath(input_data["fish parameters file",]),input_data["fish parameters file",], 
                                  here(input_folder,
                         input_data["folder",],
                         "fish",
                         input_data["fish parameters file",]))
  
  # get the paths for the 3 files 
  grid_center_line <<- ifelse(isAbsolutePath(input_data["line",]),input_data["line",], 
                              here(input_folder,
                      input_data["folder",],
                      "grid",
                      input_data["line",]))
  grid_top_marker <<- ifelse(isAbsolutePath(input_data["point",]), input_data["point",],
                             here(input_folder,
                     input_data["folder",],
                     "grid",
                     input_data["point",]))
  grid_res_path <<- ifelse(isAbsolutePath(input_data["grid resolution file",]), input_data["grid resolution file",],
                           here(input_folder,input_data["folder",], "grid",
                   input_data["grid resolution file",]))
  
  cover_file <<- ifelse(isAbsolutePath(input_data["cover file",]), input_data["cover file",],
                        here(input_folder, 
       input_data["folder",],
       "cover",
       input_data["cover file",]))
  
  canopy_cover_file <<- ifelse(isAbsolutePath(input_data["canopy cover",]), input_data["canopy cover",],
                               here(input_folder, 
       input_data["folder",],
       "cover",
       input_data["canopy cover",]))
  
  hab_path <<- ifelse(isAbsolutePath(input_data["habitat parameters file",]), input_data["habitat parameters file",],
                      here(input_folder,input_data["folder",], "habitat",
                   input_data["habitat parameters file",]))
  
  # Location of rasters
  raster_folder <<- ifelse(isAbsolutePath(input_data["raster folder",]),input_data["raster folder",], 
                           here(input_folder, 
                       input_data["folder",],
                       "flow"))
  
  hydrology_folder <<- ifelse(isAbsolutePath(input_data["hydrology folder",]),input_data["hydrology folder",], 
                              here(input_folder, input_data["folder",], "daily"))
  
  # Reset this stuff to be ready for a new run, the random seed in particular
  # should get set every time.
  # Load a color blind friendly pallet
  cbPalette <<- c("#999999", "#0072B2", "#D55E00",
                  "#F0E442", "#56B4E9", "#E69F00",
                  "#0072B2", "#009E73", "#CC79A7")
  
  # Set the random seed
  set.seed(6806665)
  
  # select <<- dplyr::select
}