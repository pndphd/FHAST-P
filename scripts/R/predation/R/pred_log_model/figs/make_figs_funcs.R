# function that subsets the combined_data based on pred_species
subset_data <- function(df, pred_species) {
  df %>%
    dplyr::filter(species == pred_species) %>%
    dplyr::select(-species)
}

# builds a logistic model using the tidy models framework and k-fold cross-validation
build_model <- function(df, pred_species) {
  df <- subset_data(df, pred_species)
  
  set.seed(123)
  split <- initial_split(df, strata = count)
  train <- training(split)
  test <- testing(split)
  
  set.seed(234)
  folds <- vfold_cv(train, strata = count)
  
  # create recipe -----------------------------------------------------------
  set.seed(345)
  rec <- recipe(count ~ ., data = train) %>%
    themis::step_upsample(count)
  
  # logistic regression -----------------------------------------------------
  
  log_spec <-
    logistic_reg() %>%
    set_engine("glm")
  
  wf <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(log_spec)
  
  doParallel::registerDoParallel()
  
  log_results <-
    wf %>%
    fit_resamples(
      resamples = folds,
      control = control_resamples(save_pred = TRUE, verbose = TRUE)
    )
}

# returns a dataframe of roc data for each species with a new species column
roc_res <- function(df, pred_species) {
  build_model(df, pred_species) %>%
    augment() %>%
    roc_curve(count, .pred_absent) %>%
    mutate(species = pred_species)
}