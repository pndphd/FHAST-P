
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
  mutate(term = case_when(term == '(Intercept)' ~ 'intercept',
         TRUE ~ term))

# 3. export data ----------------------------------------------------------

export_path <- here(pred_proj_path, "output", "pred_log_params.csv")

write_csv(param_table, export_path)
