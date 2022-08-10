
# 0. read in libraries ----------------------------------------------------

library(tidyverse) # data manipulation
library(here) # simplifies file paths
library(broom)

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files


# 1. set file path --------------------------------------------------------

pred_proj_path <- here::here("scripts", "R", "predation")

path <- here::here(pred_proj_path, "data", "lit_search", "temp_data.csv")

# 2. data analysis --------------------------------------------------------

# readr::read_csv(path) %>%
#   dplyr::group_by(author, year, journal, species, experiment) %>% # group by various factors so only data from the same experiments are changed
#   dplyr::mutate(unitless_y = y_value / max(y_value)) %>% # convert to values relative to the max of each experiment
#   dplyr::ungroup() %>%
#   dplyr::select(temperature_C, unitless_y) %>%
#   tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
#   dplyr::mutate(fit = purrr::map(
#     .x = data,
#     .f = ~ stats::glm(
#       unitless_y ~ temperature_C,
#       family = stats::quasibinomial(logit),
#       data = .
#     )
#   )) %>%
#   dplyr::bind_cols(
#     tibble::tibble(temperature_C = seq(0, 35, 0.001)) %>% # adds a column of new data to make predictions on
#       tidyr::nest(temperature_C = temperature_C) # nesting it to use the map trick again, but for predict.glm
#   ) %>%
#   dplyr::mutate(relative_activity = purrr::map2(
#     .x = fit,
#     .y = temperature_C,
#     .f = stats::predict.glm,
#     type = "response"
#   )) %>%
#   tidyr::unnest(-c(data, fit)) %>%
#   dplyr::select(-c(data, fit)) %>%
#   readr::write_csv(here::here(pred_proj_path, "output", "pred_temperature_effects.csv"))


readr::read_csv(path) %>%
  dplyr::group_by(author, year, journal, species, experiment) %>% # group by various factors so only data from the same experiments are changed
  dplyr::mutate(unitless_y = y_value / max(y_value)) %>% # convert to values relative to the max of each experiment
  dplyr::ungroup() %>%
  dplyr::select(temperature_C, unitless_y) %>%
  tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
  dplyr::mutate(fit = purrr::map(
    .x = data,
    .f = ~ stats::glm(
      unitless_y ~ temperature_C,
      family = stats::quasibinomial(logit),
      data = .
    )
  ), tidy = purrr::map(fit, broom::tidy)) %>% 
  tidyr::unnest(tidy) %>% 
  dplyr::select(term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>% 
  dplyr::rename(intercept = `(Intercept)`) %>% 
  readr::write_csv(here::here(pred_proj_path, "output", "pred_temperature_params.csv"))
