
# 0. read in libraries ----------------------------------------------------

library(here)
library(tidyverse)

# 1. set paths ------------------------------------------------------------

# load the project input file to get the predator input file paths
main_input_file_path <- here(dirname(here()),"input_data", "input_file.txt")
main_input_file <- read_delim(main_input_file_path, delim = "=", col_names = c("file", "path"))

# folders related the initial predation modeling in R
pred_submodel_folder <- here("scripts/R/predation")
pred_submodel_output_folder <- here(pred_submodel_folder, "output")
pred_submodel_inputs_folder <- here(pred_submodel_folder, "data", "input_files")

# folders and files created for the Netlogo portion of the project
# based on the relative path listed in the main input file
pred_txt_file_rel_path <- main_input_file %>% filter(grepl("predator",file)) %>% pull(path)
pred_txt_file_abs_path <- here(dirname(main_input_file_path), pred_txt_file_rel_path)
pred_txt_file_name <- basename(pred_txt_file_rel_path)
txt_to_csv_name <- str_replace(pred_txt_file_name, ".txt", ".csv")

# File with params used to calculate maximum prey length; based on literature rather than modeling
gape_params_file_name <- "gape_limitation_params.csv"
gape_params_file_path <- here(pred_submodel_inputs_folder, gape_params_file_name)

# various modeling outputs from the initial work in R
temp_model_file_name <- "pred_temperature_params.csv"
temp_params_file_path <- here(pred_submodel_output_folder, temp_model_file_name)

length_dist_file_name <- "pred_length_dist_params.csv"
length_dist_file_path <- here(pred_submodel_output_folder, length_dist_file_name)

log_model_file_name <-  "pred_log_params.csv"
log_model_file_path <- here(pred_submodel_output_folder, log_model_file_name)

# Folder path for files used by the Netlogo script
output_folder <- here("scripts/R/predation/output")

# List of predator species
spp <- c("bass", "pikeminnow")

# 2. read in function -----------------------------------------------------

source(here("scripts", "R", "predation", "R", "input_file", "txt_to_tibble.R"))

# 3. make new files -------------------------------------------------------

# read in the .txt file without column names
txt_as_csv <- txt_to_tibble(pred_txt_file_abs_path)

# files called "simple" because their values are the same for both species
simple_files <- list(gape_params_file_path, temp_params_file_path)

# combine all simple files into one df with columns for each species
simple_file_df <- map_dfr(simple_files, pivot_and_clean) %>% 
  mutate(species = if_else(species == "intercept", "intercept_temp", species))

# files that have different params for both species
multispecies_files <- list(log_model_file_path, length_dist_file_path)

# combine all multispecies files into one df
multispecies_df <- map_dfr(multispecies_files, reshape_multi_spp_df) %>% 
  rename(species = term) %>% 
  mutate(species = if_else(species == "intercept", "intercept_glm", species))

# Note that "intercept" was renamed to avoid confusion 

total_model_df <- bind_rows(simple_file_df, multispecies_df)

# 4. save files -----------------------------------------------------------

files_to_save <- list(txt_as_csv, total_model_df)
filenames <- list(txt_to_csv_name, "all_pred_model_params.csv")
# save all files to the Netlogo temporary folder
output_file_paths <- map(filenames, ~here(output_folder, .x))

walk2(
  .x = files_to_save,
  .y = output_file_paths,
  .f = write_csv
  )
