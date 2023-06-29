########################################
# This is just a series or source calls of the scripts
# It will run the necessary files to initialize the FHAST program
# and then run the various fast scripts in order
########################################

##### Run the initialization scripts #####
# install and load the here package if necessary
if(!require(c("here"), character.only = T)){install.packages(package)}

##### Run the setup scripts #####
source(here("scripts","main","run_setup.R"))

##### Run the calculation scripts #####
source(here("scripts","main","run_model.R"))

##### Run NetLogo #####
# Shut off futures for this
if(juvenile_run == TRUE){
  future::plan(strategy = sequential)
  source(here("scripts", "netlogo", "NetLogo_Controller.R"))
  results <- run_netlogo_model()
  future::plan(strategy = multisession,
               workers = num_cores)
}

##### Run the post processing scripts #####
source(here("scripts","main","run_post.R"))
