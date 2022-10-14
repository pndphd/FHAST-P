# This script just reads in the parameters and formats them for NetLogo

# Load libraries
library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))


# Check to see if the file exists
if (!file.exists(fish_parameters_file))
  stop('The input file does not exist.')

# Read in the file
input_file <- read.csv(file = fish_parameters_file,
                       sep = ",",
                       header = TRUE) 

# make into a list
species_count = NCOL(input_file)-1
parameter_input <- read.csv(file = fish_parameters_file,
                           sep = ",",
                           header = FALSE,
                           row.names = 1) 
# Make each parameter set it own list
parameter_list <- map(seq(1, species_count  , 1), ~select(parameter_input, c(.x)))


# write the file
write.csv(input_file,  
          file = here(temp_folder, "NetLogo","fish_params_input.csv"),
          row.names = FALSE)

# make a list (1 entry species to pass)
saveRDS(parameter_list,
        file = here(temp_folder, "NetLogo","fish_params_list.rds") )