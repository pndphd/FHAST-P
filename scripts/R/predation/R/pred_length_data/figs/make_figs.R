library(tidyverse) # data manipulation
library(here) # simplifies file paths
library(readxl) # reads... excel files....
library(fitdistrplus) # fitting distributions to data

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files

# 1. functions ------------------------------------------------------------

source(file = here::here("R", "pred_length_data", "pred_length_funcs.R"))

##### stuff for plotting
# plots and saves goodness-of-fit plots for a fitted distribution
plot_dist <- function(dist, data, species) {
  tiff(
    filename = here::here("output", "figs", "pred_length_data", paste0(species, "_", dist, "_distribution.tiff")),
    width = 9,
    height = 7,
    units = "in",
    res = 800
  )
  data %>%
    pull(length_mm) %>%
    fitdist(distr = dist) %>%
    plot()
  dev.off()
}

# runs through a list of distribution types and executes the plot_dist function
plot_all_dists <- function(dist_list, data, species) {
  walk(.x = dist_list, ~ plot_dist(dist = .x, data = data, species = species))
}

##### stuff for getting goodness-of-fit stats
# fits a distribution for a given set of length data
make_dist <- function(data, dist) {
  data %>%
    pull(length_mm) %>%
    fitdist(distr = dist)
}

# fits all distributions from a list of distributions to the dataset
make_all_dists <- function(data, dist_list) {
  map(.x = dist_list, .f = ~ make_dist(data = data, dist = .x))
}

# gets goodness of fit stats for a given distribution
get_fit_stats <- function(fit_distribution) {
  fit_distribution %>% gofstat()
}

# 2. load data and params -------------------------------------------------

upper_sac_folder <- here::here("data", "fishbio", "pred_length")

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

dist_list <- list("norm", "lnorm", "gamma", "weibull", "exp")

# 3. data cleaning --------------------------------------------------------

data <- get_data(params) %>% # put all data into one dataframe
  recode_all(params) %>% # update species names
  subset_pred_species(params) %>% # select species of interest
  lump_fish_names() %>% # lump all bass and pikeminnow labels
  tidyr::drop_na() %>%
  dplyr::filter(!(length_mm == 0)) %>%
  dplyr::rename_all(.funs = tolower)

# 4. save plots -----------------------------------------------------------

grouped_data <- data %>%
  dplyr::group_nest(species)

walk2(
  .x = grouped_data$data,
  .y = grouped_data$species,
  .f = plot_all_dists,
  dist_list = dist_list
)

# 5. get goodness-of-fit stats --------------------------------------------

data %>%
  dplyr::group_nest(species) %>%
  dplyr::mutate(
    fit = purrr::map(data, ~ make_all_dists(data = .x, dist = dist_list))
  ) %>%
  unnest(fit) %>%
  mutate(stats = map(fit, get_fit_stats)) %>%
  select(stats) %>%
  map(., print)
