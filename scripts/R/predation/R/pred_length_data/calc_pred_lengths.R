
# 0. load libraries -------------------------------------------------------

library(tidyverse) # data manipulation
library(here) # simplifies file paths
library(readxl) # reads... excel files....
library(fitdistrplus) # fitting distributions to data

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files

# 1. functions ------------------------------------------------------------
pred_proj_path <- here::here("scripts", "R", "predation")
source(file = here::here(pred_proj_path, "R", "pred_length_data", "pred_length_funcs.R"))

# 2. load data and params -------------------------------------------------

upper_sac_folder <- here::here(pred_proj_path, "data", "fishbio", "pred_length")

params <- list(
  csvs = here::here(upper_sac_folder, list.files(upper_sac_folder, pattern = "*.csv")),
  excel = here::here(upper_sac_folder, list.files(upper_sac_folder, pattern = "*.xlsx")),
  preds = c(
    "Bass",
    "Pikeminnow",
    "Sacramento Pikeminnow",
    "Spotted Bass",
    "Largemouth Bass",
    "Smallmouth Bass"
  )
)

# 3. data cleaning + saving --------------------------------------------------------

get_data(params) %>% # put all data into one dataframe
  recode_all(params) %>% # update species names
  subset_pred_species(params) %>% # select species of interest
  lump_fish_names() %>% # lump all bass and pikeminnow labels
  tidyr::drop_na() %>%
  dplyr::filter(!(length_mm == 0)) %>% 
  dplyr::rename_all(.funs = tolower) %>% # lowercase column names
  dplyr::group_nest(species) %>%
  dplyr::mutate(
    fit = purrr::map(data, ~ fitdistrplus::fitdist(.$length_mm, distr = "lnorm")), # fit exponential distributions to the length data per species
    meanlog = purrr::map_dbl(fit, ~ get_dist_param(., param = 1)), # select the parameter from the distributions
    sdlog = purrr::map_dbl(fit, ~ get_dist_param(., param = 2)), # select the parameter from the distributions
    length_mm = purrr::map2(meanlog, sdlog, ~ stats::rlnorm(100000, meanlog = .x, sdlog = .y)) # draw 100,000 fish from the distribution
  ) %>%
  dplyr::mutate(max_len = map_dbl(data, get_max_length)) %>% # find max observed length per species
  tidyr::unnest(length_mm) %>%
  dplyr::group_by(species) %>%
  dplyr::filter(length_mm >= 150 & length_mm <= max_len) %>% # select all piscivorous fish that are smaller than or equal to the observed data
  dplyr::ungroup() %>%
  dplyr::select(c(species, length_mm)) %>% # select only the cols of interest
  dplyr::mutate(max_prey_length = prey_conv(a = 4.64, B = 2.46e-6, length_mm)) %>% # calculate the max size of prey per fish
  save_all_outputs() # save outputs; 1 csv file per species
