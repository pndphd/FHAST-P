source(here("scripts","R","main","initialize_fhast.R"))

# set the main input folders
input_folder = file.path(dirname(here()),"input_data")
fhast_config_file = "input_file.txt"
temp_folder = here("temporary")

# initialize_fhast sets up all the directory paths for future scripts.
initialize_fhast(input_folder)

# Setup furrr

source(file = here("scripts", "R", "main", "num_cores_func.R"))
num_cores <- pick_num_cores()

future::plan(strategy = multisession,
             workers = num_cores)

# plan("future::multisession")
future.seed = FALSE