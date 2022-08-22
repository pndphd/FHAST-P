##### Description #####
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.
library(here)

# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load the functions
source(here("scripts","R","daily_inputs","scripts","functions_make_the_fish_input_file.R"))

##### Load Files #####
# Load data files
# Read in the main input file file
input_data <- read.csv(file = here(input_folder, input_file),
                       sep = "=",
                       row.names = 1,
                       header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

#get the name of the input file
input_file_name = here(input_folder,
                         input_data["folder",],
                         "fish",
                         input_data["fish population file",])

# Check to see if the file exists
if (!file.exists(input_file_name))
  stop('The input file does not exist.')

# Read in the file
input_file <- read.csv(file = input_file_name,
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

