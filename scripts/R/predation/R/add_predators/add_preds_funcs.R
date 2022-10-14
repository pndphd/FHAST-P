# Load Libraries and some base parameters
source("./scripts/R/main/load_libraries.R")

# 1. functions ---------------------------------------------------------------

# 1a. pred log model predictions ------------------------------------------

# selects model paramaters for a given species
subset_params <- function(species_name, model_params) {
  model_params %>%
    filter(species == {{ species_name }})
}

# widens the model param data to make selecting particular values easier
widen_model_params <- function(model_params) {
  model_params %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )
}

# selects the intercept value from the widened param data
get_intercept <- function(params_wider) {
  params_wider %>%
    pull(intercept)
}

# selects only the parameter data from the widened param value
get_param_data <- function(params_wider, vals_to_drop = c(intercept, species)) {
  params_wider %>%
    select(-{{ vals_to_drop }})
}

# turns the output of get_param_data into a vector of values
get_param_vector <- function(param_data) {
  param_data %>%
    pivot_longer(cols = everything(), names_to = "term") %>%
    pull("value")
}

# uses the intercept and param vector to calculate all predicted habitat values for a given predator species
make_predictions <- function(df, model_params, species_name) {
  subset_model_params <- subset_params(species_name, model_params)
  model_params_wide <- widen_model_params(subset_model_params)
  intercept <- get_intercept(model_params_wide)
  param_data <- get_param_data(model_params_wide) %>%
    get_param_vector()

  # turn tibble into matrix to perform linear algebra
  t(t(df) * param_data) %>%
    as_tibble() %>% # back to tibble to use the mutate function
    mutate(
      intercept = intercept,
      sum = rowSums(across(.cols = everything())),
      hab_rating = 1 / (1 + exp(-sum))
    ) %>%
    select(hab_rating) %>%
    rename_pred_cols(species_name)
}

# 1b. adding predators to cells -------------------------------------------

# determines number of predators per cell based on predicted habitat rating and
# the number of predators in the system
# calc_pred_count <- function(df, model_params, species_name, reach_preds) {
#   make_predictions(df, model_params, species_name) %>%
#     mutate(
#       scaled = hab_rating / sum(hab_rating),
#       pred_prob = scaled * reach_preds,
#       pred = rbinom(n = nrow(.), size = 1, prob = pred_prob)
#     ) %>%
#     select(hab_rating, pred) %>%
#     rename_pred_cols(species_name)
# }

calc_pred_count <- function(df, reach_preds) {
  df %>%
    mutate(
      across(
        .cols = ends_with("hab_rating"),
        .fns = ~ if_else(.x >= 0.5, 1, 0) * .x,
        .names = "{.col}_pred"
      ),
      # across(
      #   .cols = ends_with("hab_rating_pred"),
      #   ~ .x / max(.x)
      # ),
      across(
        .cols = ends_with("hab_rating_pred"),
        .fns = ~ round((.x * wetted_area) / sum(.x * wetted_area) * reach_preds)
      )
    ) %>%
    rename_with(
      .fn = ~ str_remove(.x, "_hab_rating_pred"),
      .cols = ends_with("_hab_rating_pred")
    )
  # commented-out code below for a slightly different approach to adding predators
  # mutate(across(.cols = ends_with("hab_rating"),
  #             .fns = ~ rbinom(n = length(.x), size = 1, prob = .x) * .x,
  #             .names = "{.col}_pred"),
  #      across(.cols = ends_with("hab_rating_pred"),
  #             .fns = ~ round(.x / sum(.x) * reach_preds)
  #      )) %>%
  # select(ends_with("_hab_rating_pred")) %>%
  # rename_with(.fn = ~str_remove(.x, "_hab_rating_pred"),
  #             .cols = ends_with("_hab_rating_pred"))
}

# relabels cols with number of preds and hab rating with the predator species name
rename_pred_cols <- function(df, species_name) {
  df %>%
    rename(
      "{species_name}_hab_rating" := hab_rating,
      # "{species_name}" := pred
    )
}

# creates a vector of predator species from the model params
get_list_of_species <- function(model_params) {
  model_params %>%
    distinct(species) %>%
    pull(species)
}

# selects a vector of habitat parameter names to pass to the select_habitat_data func
select_hab_vars <- function(model_params) {
  model_params %>%
    distinct(term) %>%
    filter((!term == "intercept")) %>%
    pull(term)
}

# uses the model parameter names to subset matching columns from the input habitat dataframe
select_hab_data <- function(df, ...) {
  df %>%
    select(all_of(...))
}

# calculates predicted predator habitat rating and number of predators for all species
calc_all_pred_data <- function(df, species_list, model_params, script_params) {
  hab_vars <- select_hab_vars(model_params)
  hab_data <- select_hab_data(df, hab_vars)
  total_wetted_area <- calc_total_wetted_area(df)
  reach_preds <- get_total_reach_preds(
    total_wetted_area,
    pred_num = script_params$preds_per_hectare,
    conv = script_params$conv
  )

  # calculate habitat ratings and predator counts
  pred_data <- map_dfc(
    .x = species_list,
    .f = make_predictions,
    df = hab_data,
    model_params = model_params
  )
  bind_cols(df, pred_data) %>% 
    mutate(across(contains(species_list), ~if_else(wetted_fraction > 0, .x, 0)))%>% 
    calc_pred_count(reach_preds)
}

calc_total_wetted_area <- function(df) {
  df %>%
    #dplyr::mutate(wetted_area = area * wetted_fraction) %>%
    dplyr::pull(wetted_area) %>%
    sum()
}

# calculates total number of predators for the entire study reach
# based on the number of preds per unit area, the cell size, the number of cells,
# and a conversion factor; e.g. to convert preds/ha to preds/m2
get_total_reach_preds <- function(wetted_area, pred_num, conv) {
  round(pred_num * wetted_area / conv)
}

# nests the input dataframe by time (e.g., per day) and calculates the predator predicitons
# for each time unit; adds totals and accounts for depths <= 0
calc_preds_per_time <- function(df, time_col, model_params, script_params) {
  depth_col <- get_depth_col_name(df)
  depth_col <- ensym(depth_col)
  df %>%
    dplyr::filter(!!depth_col > 0) %>%
    dplyr::group_nest({{ time_col }}) %>%
    dplyr::mutate(preds = future_map(
      .x = data,
      .f = calc_all_pred_data,
      model_params = model_params,
      script_params = script_params,
      .options = furrr_options(seed = TRUE)
    )) %>%
    # preds = future_map2(
    #   .x = data,
    #   .y = hab_rating,
    #   .f = calc_pred_count,
    #   script_params = script_params,
    #   .options = furrr_options(seed = TRUE)
    # )) %>%
    tidyr::unnest(everything()) # %>%
  # drop_when_depth_zero(model_params) %>%
  #   add_pred_totals(model_params) %>%
  #   add_pred_areas(script_params, model_params, temp_params)
  # df %>%
  #   full_join(preds)
}

calc_pred_length <- function(prey_length, a = 4.64e-0, B = 2.48e-6) {
  sqrt((log(prey_length) - a) / B)
}

calc_prey_length <- function(pred_length, a = 4.64, B = 2.48e-6){
  exp(a + B * pred_length^2)
  }

get_pred_length <- function(n, meanlog, sdlog) {
#   lengths <- pred_length_data %>%
#     pull({{ pred_species }})
# 
#   sample(x = lengths, size = num_preds, replace = TRUE)

  rlnorm(n, meanlog, sdlog)
}

get_all_pred_lengths <- function(df, pred_length_data) {
  all_lengths <- df %>%
    count(species) %>%
    left_join(pred_length_data, by = "species") %>% 
    mutate(pred_length = pmap(
      .l = list(n, meanlog, sdlog),
      .f = rlnorm
    )) %>%
    select(pred_length) %>% 
    unnest(pred_length)

  df %>%
    bind_cols(all_lengths)
}


adjust_preds_for_length <- function(df, species_list, pred_length_data) {
  new_preds <- df %>%
   # drop_na() %>%
    pivot_longer(all_of(species_list), names_to = "species") %>%
    uncount(value) %>%
    arrange(species) %>%
    get_all_pred_lengths(pred_length_data) %>%
    filter(pred_length >= 150) %>%
    group_by(day, distance, lat_dist) %>%
    mutate(pred_length = mean(pred_length)) %>%
    group_by(day, distance, lat_dist, species) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    distinct() %>%
    pivot_wider(names_from = species, values_from = n)
  
  # check if both predator columns exist; if not, create a col of 0s
  new_preds[species_list[!(species_list %in% colnames(new_preds))]] <- 0
  
  df %>%
    select(-all_of(species_list)) %>%
    left_join(new_preds) %>%
    mutate(across(all_of(species_list), ~ replace_na(.x, 0))) %>%
    suppressMessages()
}

# adds totals for number of predators and a scaled, total predator habitat rating
add_pred_totals <- function(df, species_list, model_params) {
  df %>%
    mutate(
      total_preds = rowSums(across(all_of(species_list))),
      total_pred_hab_rating = rowSums(across(ends_with("hab_rating"))),
      # total_pred_hab_rating = total_pred_hab_rating / max(total_pred_hab_rating)
    )
}

# sets predator predictions to 0 if depth <= 0
drop_when_depth_zero <- function(df, model_params) {
  depth_col <- get_depth_col_name(df)
  depth_col <- ensym(depth_col)
  species_names <- get_list_of_species(model_params)
  df %>%
    mutate(
      across(
        .cols = contains(species_names),
        .fns = ~ ifelse(!!depth_col > 0, .x, 0)
      )
    )
}

# gets the name of the depth column (in case it is depth_m, depth_ft, etc.)
get_depth_col_name <- function(df) {
  df %>%
    select(contains("depth")) %>%
    colnames()
}

# calculates the area occupied by predators in a given cell, based on predator reaction distance
calc_predator_area <- function(n_preds, reaction_distance, cell_area, wetted_fraction, temp_effect) {
  pmin(n_preds * reaction_distance^2 * pi * temp_effect / (cell_area * wetted_fraction), n_preds)

}

# adds predator area values to all cells
add_pred_areas <- function(df, species_list, script_params, model_params, temp_params) {
  df %>%
    calc_all_temp_effects(temp_params) %>%
    mutate(across(c(ends_with(species_list)),
      .fns = ~ calc_predator_area(.x, script_params$reaction_distance, area, wetted_fraction, temp_effect),
      .names = "{.col}_prop_area"
    ),
    across(ends_with("prop_area"), ~ if_else(.x > 1, floor(.x), .x)),
    total_pred_prop_area = rowSums(across(ends_with("prop_area"))),
    across(ends_with("prop_area"), ~ replace_na(.x, 0))

    ) %>%
    select(-temp_effect)
}


# 1c. temperature effects -------------------------------------------------

# gets the temperature column name from the input dataset
get_temp_col_name <- function(df) {
  df %>%
    select(contains("temp")) %>%
    colnames()
}

# calculates the predicted effect of temperature using a simple GLM
calc_temp_effect <- function(temp, param, intercept) {
  1 / (1 + exp(-(temp * param + intercept)))
}

# calculates all temperature effects for the whole input dataframe
calc_all_temp_effects <- function(df, temp_params) {
  intercept <- get_intercept(temp_params)
  param <- get_param_data(temp_params, vals_to_drop = intercept) %>%
    get_param_vector()
  temp_col <- get_temp_col_name(df)
  temp_col <- ensym(temp_col)
  df %>%
    group_nest(!!temp_col) %>%
    mutate(temp_effect = calc_temp_effect(!!temp_col, param, intercept)) %>%
    unnest(data)
}

# for testing ----------------------------------------------------------

# # inputs for development
#
# script_params <- list(
#   resolution = 20,
#   buffer = 500,
#   preds_per_hectare = 30,
#   conv = 1e5,
#   reaction_distance = 1
# )
#
# # predator glm parameters
# pred_proj_path <- here::here("scripts", "R", "predation")
# model_params <- read_csv(here(pred_proj_path, "output", "pred_log_params.csv"))
# temp_data <- read_csv(here(pred_proj_path, "output", "pred_temperature_effects.csv"))
# temp_params <- read_csv(here(pred_proj_path, "output", "pred_temperature_params.csv"))
#
#
# make_tbl_w_days <- function(day, n) {
#   tibble(
#     day = day,
#     shade = sample(c(0, 1), size = n, replace = TRUE),
#     vegetation = sample(seq(0, 1, 0.0001), size = n, replace = TRUE),
#     cover = sample(seq(0, 1, 0.0001), size = n, replace = TRUE),
#     depth = sample(seq(0, 5, 0.0001), size = n, replace = TRUE),
#     velocity = sample(seq(0, 2, 0.0001), size = n, replace = TRUE),
#     substrate = sample(c(0, 1), size = n, replace = TRUE),
#     not_habitat_stuff = "test"
#   ) %>%
#     group_nest(day) %>%
#     mutate(temp = sample(seq(10,25,0.0001), size = nrow(.), replace = TRUE)) %>%
#     unnest(data)
# }
#
# #
# days <- 1:5
#
# n <- 1e3
#
# test_csv <- map_dfr(days, make_tbl_w_days, n = n)
#
#
#
#
# # parallel
# tic()
# d <- calc_preds_per_time(test_csv, day, model_params, script_params, temp_params)
# toc()
#
#
