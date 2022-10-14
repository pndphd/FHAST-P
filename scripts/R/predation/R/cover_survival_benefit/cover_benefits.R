
# 0. load libraries ----------------------------------------------------

library(tidyverse)
library(here)

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files

# 1. functions ------------------------------------------------------------
pred_proj_path <- here::here("scripts", "R", "predation")

source(here::here(pred_proj_path, "R", "cover_survival_benefit", "cover_benefit_funcs.R"))

# file paths and params ---------------------------------------------------

cover_data_path <- here::here(pred_proj_path, "data", "lit_search", "cover_data.csv")
cover_sim_path <- here::here(pred_proj_path, "data", "simulation", "cover_simulation_data.csv")

cell_size_m <- 1

# 3. analysis -------------------------------------------------------------

readr::read_csv(cover_data_path) %>%
  # this section cleans and fits a glm to observed data of fish vs. distance to cover
  dplyr::group_by(fish_size_mm) %>% # group by fish size
  get_unitless_y(cumulative_fraction) %>%
  dplyr::ungroup() %>%
  dplyr::select(dis_to_cover_m, unitless_y) %>%
  tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
  dplyr::mutate(fit = purrr::map( # fit glm
    .x = data,
    .f = ~ stats::glm(
      unitless_y ~ dis_to_cover_m,
      family = stats::quasibinomial(logit),
      data = .
    )
  )) %>%
  # this section adds and analyzes the cover simulation data
  dplyr::bind_cols(
    readr::read_csv(cover_sim_path) %>%
      dplyr::select(pct_cover, mean_dis_w_0) %>% # mean_dis_w_0 is just one of the two simulations run; includes all values with 0 dis to cover
      dplyr::mutate(dis_to_cover_m = mean_dis_w_0 * cell_size_m) %>% # adjust cell dimensions
      tidyr::nest(data = dplyr::everything()) %>% # use the nesting trick to git a model
      dplyr::mutate(fit = purrr::map( # fit a polynomial: x^0.5 + x + x^1.5
        .x = data,
        .f = ~ stats::lm(
          dis_to_cover_m ~ sqrt(pct_cover) * pct_cover,
          data = .
        )
      )) %>%
      dplyr::bind_cols(tibble::tibble( # add a list of new synthetic pct_cover data to make dis_to_cover predictions for
        pct_cover = seq(0, 1, 0.00001)
      ) %>%
        tidyr::nest(pct_cover = pct_cover)) %>%
      fit_synth_data(
        synth_data = pct_cover,
        func = stats::predict,
        output_col = dis_to_cover_m
      ) %>%
      clean_up_df() %>%
      dplyr::filter(!(pct_cover == 0)) %>% # no cover benefit when cover = 0, so this value is irrelevant
      tidyr::nest(dis_to_cover_sim = everything())
  ) %>%
  # this section fits the glm to the simulated distance to cover data
  fit_synth_data(
    synth_data = dis_to_cover_sim,
    func = stats::predict.glm,
    output_col = cover_bonus,
    type = "response"
  ) %>%
  clean_up_df() %>%
  readr::write_csv(here::here(pred_proj_path, "output", paste0("prey_cover_bonus_", cell_size_m, "m_cell.csv")))

readr::read_csv(cover_sim_path) %>%
  dplyr::select(pct_cover, mean_dis_w_0) %>% # mean_dis_w_0 is just one of the two simulations run; includes all values with 0 dis to cover
  dplyr::mutate(dis_to_cover_m = mean_dis_w_0 * cell_size_m) %>% # adjust cell dimensions
  tidyr::nest(data = dplyr::everything()) %>% # use the nesting trick to git a model
  dplyr::mutate(fit = purrr::map( # fit a polynomial: x^0.5 + x + x^1.5
    .x = data,
    .f = ~ stats::lm(
      dis_to_cover_m ~ sqrt(pct_cover) * pct_cover,
      data = .
    )
  )) %>% 
  dplyr::pull(fit) %>% 
  .[[1]] %>% 
  saveRDS(here::here(pred_proj_path, "output", "pct_cov_convers_model.RDS"))

readr::read_csv(cover_data_path) %>%
  # this section cleans and fits a glm to observed data of fish vs. distance to cover
  dplyr::group_by(fish_size_mm) %>% # group by fish size
  get_unitless_y(cumulative_fraction) %>%
  dplyr::ungroup() %>%
  dplyr::select(dis_to_cover_m, unitless_y) %>%
  tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
  dplyr::mutate(fit = purrr::map( # fit glm
    .x = data,
    .f = ~ stats::glm(
      unitless_y ~ dis_to_cover_m,
      family = stats::quasibinomial(logit),
      data = .
    )
  )) %>% 
  dplyr::pull(fit) %>% 
  .[[1]] %>% 
  saveRDS(here::here(pred_proj_path, "output", "dis_to_cov_model.RDS"))

