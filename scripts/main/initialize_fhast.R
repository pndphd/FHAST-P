########################################
# Run the initalization
########################################

# set the main input folders
input_folder <- file.path(here(), "default_input")
fhast_config_file <- "input_file.txt"
temp_folder <- here("temporary")


# Make sure temp directories exist
make_dir_if_missing(here(temp_folder, "file.txt"))

# Turn on printing plots
print_plots <<- FALSE

file_path <- here(input_folder, fhast_config_file)
# initialize_fhast sets up all the directory paths for future scripts.
initialize_fhast(file_path)

# Setup furrr
pick_num_cores <- function(ratio = 0.75){
  total_cores <- parallel::detectCores()
  cores_to_use <- floor(total_cores * ratio)
  if(cores_to_use <= 1){
    return(total_cores)
  } else {
    return(cores_to_use)
  }
}
num_cores <- pick_num_cores()



future::plan(
  strategy = multisession,
  workers = num_cores
)

future.seed <- FALSE
