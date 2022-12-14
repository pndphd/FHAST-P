
# 0. load libraries and options -----------------------------------------------

library(tidymodels)
library(tidyverse)
library(here)
doParallel::registerDoParallel()

pred_proj_path <- here::here("scripts", "R", "predation")
source(file = here(pred_proj_path, "R", "pred_log_model", "log_model_functions.R"))


# 1. read in data ------------------------------------------------

source(file = here(pred_proj_path, "R", "pred_log_model", "pred_param_data_cleaning.R")) # clean and load the data

# 2. analysis ----------------------------------------------------------------

param_table <- combined_data %>%
  dplyr::group_nest(species) %>%
  dplyr::mutate(
    fit = purrr::map(.x = data, .f = fit_model),
    summary = purrr::map(.x = fit, .f = get_model_summary),
    params = purrr::map(.x = summary, .f = make_param_table)
  ) %>%
  dplyr::select(species, params) %>%
  tidyr::unnest(cols = c(params)) %>% 
  dplyr::mutate(term = str_replace(term, "\\(Intercept\\)", "intercept_glm"))
# make a wide version for easier handling in Netlogo
param_table_netlogo <- param_table %>% 
  pivot_wider(names_from = "term", values_from = "estimate")

# 3. export data ----------------------------------------------------------
filenames <- list("pred_log_params.csv", "pred_log_params_netlogo.csv")
files <- list(param_table, param_table_netlogo)

export_folder <- here(pred_proj_path, "output")

filepaths <- map(filenames, ~ here(export_folder, .x))

walk2(files,filepaths, ~write_csv(.x, .y))
