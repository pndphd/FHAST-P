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
    select(hab_rating)
}

# 1b. adding predators to cells -------------------------------------------

# determines number of predators per cell based on predicted habitat rating and
# the number of predators in the system
calc_pred_count <- function(df, model_params, species_name, reach_preds) {
  make_predictions(df, model_params, species_name) %>%
    mutate(
      scaled = hab_rating / sum(hab_rating),
      pred_prob = scaled * reach_preds,
      pred = rbinom(n = nrow(.), size = 1, prob = pred_prob)
    ) %>%
    select(hab_rating, pred) %>%
    rename_pred_cols(species_name)
}

# relabels cols with number of preds and hab rating with the predator species name
rename_pred_cols <- function(df, species_name) {
  df %>%
    rename(
      "{species_name}_hab_rating" := hab_rating,
      "n_{species_name}" := pred
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
calc_all_pred_data <- function(df, model_params, script_params) {
  all_species <- get_list_of_species(model_params)
  hab_vars <- select_hab_vars(model_params)
  hab_data <- select_hab_data(df, hab_vars)
  reach_preds <- get_total_reach_preds(
    num_cells = nrow(df),
    pred_num = script_params$preds_per_hectare,
    cell_width = script_params$resolution,
    conv = script_params$conv
  )
  map_dfc(
    .x = all_species,
    .f = calc_pred_count,
    df = hab_data,
    model_params = model_params,
    reach_preds = reach_preds
  )
}

# calculates total number of predators for the entire study reach
# based on the number of preds per unit area, the cell size, the number of cells,
# and a conversion factor; e.g. to convert preds/ha to preds/m2
get_total_reach_preds <- function(num_cells, pred_num, cell_width, conv) {
  pred_num * (cell_width^2 * num_cells) / conv
}

# nests the input dataframe by time (e.g., per day) and calculates the predator predicitons
# for each time unit; adds totals and accounts for depths <= 0
calc_preds_per_time <- function(df, time_col, model_params, script_params, temp_params) {
  depth_col <- get_depth_col_name(df) 
  depth_col <- ensym(depth_col)
  preds <- df %>%
    filter(!!depth_col > 0) %>%
    group_nest({{ time_col }}) %>%
    mutate(preds = future_map(
      .x = data,
      .f = calc_all_pred_data,
      model_params = model_params,
      script_params = script_params,
      .options = furrr_options(seed = TRUE)
    )) %>%
    unnest(everything()) %>%
    # drop_when_depth_zero(model_params) %>%
    add_pred_totals(model_params) %>%
    add_pred_areas(script_params, model_params, temp_params)
  df %>%
    full_join(preds)
}

# adds totals for number of predators and a scaled, total predator habitat rating
add_pred_totals <- function(df, model_params) {
  df %>%
    mutate(
      total_preds = rowSums(across(ends_with(get_list_of_species(model_params)))),
      total_pred_hab_rating = rowSums(across(starts_with(get_list_of_species(model_params)))),
      total_pred_hab_rating = total_pred_hab_rating / max(total_pred_hab_rating)
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
calc_predator_area <- function(n_preds, reaction_distance, cell_width, temp_effect) {
  n_preds * reaction_distance^2 * pi * temp_effect / cell_width
}

# adds predator area values to all cells
add_pred_areas <- function(df, script_params, model_params, temp_params) {
  df %>%
    calc_all_temp_effects(temp_params) %>%
    mutate(across(c(ends_with(get_list_of_species(model_params)), total_preds),
      .fns = ~ calc_predator_area(.x, script_params$reaction_distance, script_params$resolution, temp_effect),
      .names = "{.col}_prop_area"
    )) %>%
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
