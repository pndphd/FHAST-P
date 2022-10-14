##### Description #####
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.

library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load the functions
source(here("scripts","R","daily_inputs","scripts","functions_make_the_fish_input_file.R"))

# Check to see if the file exists
if (!file.exists(fish_population_file))
  stop('The input file does not exist.')

# Read in the file
input_file <- read.csv(file = fish_population_file,
                      sep = ",",
                      header = TRUE) %>% 
  mutate(date = mdy(date))

##### Make the Files #####
# make the list of days and fish
fish_schedule = input_file %>% 
  pmap_dfr(~spread_out_fish(...)) %>% 
  arrange(date) %>% 
  mutate(date = format(date, "%m/%d/%Y"))

##### Save the result #####
write.csv(fish_schedule, 
          file = here(temp_folder, "NetLogo","daily_fish_input.csv"),
          row.names = FALSE)

