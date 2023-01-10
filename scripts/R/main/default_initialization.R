########################################
# Run the initalization
########################################

# set the main input folders
input_folder <- file.path(dirname(here()), "input_data")
fhast_config_file <- "input_file.txt"
temp_folder <- here("temporary")

# Make sure temp directories exist
make_dir_if_missing(here(temp_folder, "file.txt"))
make_dir_if_missing(here(temp_folder, "R", "file.txt"))
make_dir_if_missing(here(temp_folder, "NetLogo", "file.txt"))

# Turn on printing plots
print_plots <<- TRUE

file_path <- here(input_folder, fhast_config_file)
# initialize_fhast sets up all the directory paths for future scripts.
initialize_fhast(file_path)

# Setup furrr
source(file = here("scripts", "R", "main", "num_cores_func.R"))
num_cores <- pick_num_cores()

future::plan(
  strategy = multisession,
  workers = num_cores
)

future.seed <- FALSE
