########################################
# This is just a series or source calls of the scripts
# It will run the necessary files to initialize the FHAST program
# and then run the various fast scripts in order
########################################

##### Run the initilization scripts #####
# install and load the here package if necessary
if(!require(c("here"), character.only = T)){
  install.packages(package)
}

# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load some common functions used in running FHAST
source(here("scripts", "R", "main", "fhast_file_functions.R"))

# Load some plotting functions
source(here("scripts", "R", "main", "plot_functions.R"))

# Sets up the file structure for FHAST run
source(here("scripts", "R", "main", "initialize_fhast.R"))

# Run the initialization 
source(here("scripts","R","main","default_initialization.R"))

# load all the input files and make basic parameter files
# also do some basic checking of files
source(here("scripts","R","parameters","load_convert_parameters.R"))

##### Run the calculation scripts #####
source(here("scripts","R","main","run_model.R"))