##########
# This is just a series or source calls of the scripts
##########

# install and load the here package if necessary
if(!require(c("here"), character.only = T)){
  install.packages(package)
}

# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))
source(here("scripts","R","main","default_initialization.R"))

source(here("scripts","R","main","run_model.R"))