#TODO finish up curves for logistic
library(tidymodels)
library(tidyverse)
library(poissonreg)
library(here)
source(file = here("R", "data_cleaning.R")) # clean and load the data
doParallel::registerDoParallel()


# split the data ----------------------------------------------------------


id_vars <- bass_data %>%
  select(c(bass:date, sample))

# id_vars <- count_data %>%
#   select(c(site:date, sample, species, count))

predictors <- bass_data %>%
  select(c(shade:substrate), -sample) %>%
  rename_if(is.factor, .funs = ~ paste(., "_Dummy", sep = "")) %>%
  rename_if(is.numeric, .funs = ~ paste(., "_Numeric", sep = ""))

bass_data <- bind_cols(id_vars, predictors)

set.seed(123)
split <- initial_split(bass_data, strata = bass)
train <- training(split)
test <- testing(split)

set.seed(234)
# folds <- bootstraps(train, strata = bass)
folds <- vfold_cv(train, strata = bass)

# create recipe -----------------------------------------------------------
set.seed(345)
rec <- recipe(bass ~ ., data = train) %>%
  update_role(c(sample, site, date, design_type), new_role = "ID") %>%
  #step_poly(all_numeric_predictors(), degree = 3) %>% 
  step_dummy(all_nominal_predictors()) %>%
  #step_interact(~contains("Dummy"):contains("Numeric")) %>%
  #step_interact(~contains('emerg'):contains("substrate")) %>% 
  themis::step_upsample(bass)

rec %>% prep() %>%  juice()

# logistic regression -----------------------------------------------------

log_spec <-
  logistic_reg() %>%
  set_engine("glm")

wf <-
  workflow() %>%
  add_recipe(rec)

doParallel::registerDoParallel()

log_results <-
  wf %>%
  add_model(log_spec) %>%
  fit_resamples(
    resamples = folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )


log_results %>% collect_metrics()

log_fit <-
  wf %>%
  add_model(log_spec) %>%
  last_fit(split)

log_fit %>%
  conf_mat_resampled()

log_fit %>%  collect_metrics()


augment(log_results) %>%
  roc_curve(bass, .pred_absent) %>%
  autoplot()

log_fit$.workflow[[1]] %>%
  tidy()
