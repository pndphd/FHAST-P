
# 0. load libraries -------------------------------------------------------

library(tidymodels)
library(tidyverse)

# 1. functions ------------------------------------------------------------

make_recipe <- function(data) {
  recipes::recipe(count ~ ., data = data) %>%
    themis::step_rose(count)
}

make_model_spec <- function(engine = "glm") {
  parsnip::logistic_reg() %>%
    parsnip::set_engine(engine)
}

make_workflow <- function(recipe, model_spec) {
  workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec)
}

validate_model <- function(workflow, split) {
  workflow %>%
    tune::last_fit(split)
}

fit_model <- function(data) {

  # split data
  split <- rsample::initial_split(data, strata = count)
  train <- rsample::training(split)
  test <- rsample::testing(split)

  # data recipe
  recipe <- make_recipe(train)

  # model specification
  model_spec <- make_model_spec()

  # workflow using data recipe
  workflow <- make_workflow(recipe, model_spec)

  # fit the model using CV
  validate_model(workflow, split)
}

get_model_summary <- function(last_fit_data) {
  last_fit_data %>%
    purrr::pluck(".workflow", 1) %>%
    workflows::extract_fit_parsnip()
}

make_param_table <- function(model_summary, fish_species) {
  model_summary %>%
    broom::tidy() %>%
    dplyr::select(term:estimate)
}
