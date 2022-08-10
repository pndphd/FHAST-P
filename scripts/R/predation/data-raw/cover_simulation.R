
# 0. load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
library(furrr)
options(readr.show_col_types = FALSE)
plan(multisession)

# 1. load source scripts -----------------------------------------------------

file_paths <- list(here('data-raw/dis_to_cov_sim_funcs.R'), 
                   here('data-raw/cov_sim_params.R'))
walk(file_paths, source)

# 2. run simulation ----------------------------------------------------------

# path to the cover simulation data
cover_sim_path <- here('data', 'simulation', 'cover_simulation_data.csv')

# the simulation takes a couple minutes, so saving the data is helpful
if(!file.exists(cover_sim_path)){
  write_csv(get_cover_vs_dis(cover_sim_params), cover_sim_path)
}
