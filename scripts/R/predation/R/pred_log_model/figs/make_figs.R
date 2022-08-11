# TODO finish up curves for logistic
library(tidymodels)
library(tidyverse)
library(here)

pred_folder <- here("scripts", "R", "predation")
source(file = here(pred_folder, "R", "pred_log_model", "pred_param_data_cleaning.R")) # clean and load the data

# functions ---------------------------------------------------------------

source(file = here(pred_folder, "R", "pred_log_model", "figs", "make_figs_funcs.R"))

# split the data ----------------------------------------------------------

species_roc_data <- c("bass", "sasq") %>%
  map_dfr(roc_res, df = combined_data)

species_roc_data %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "red", size = 1.0, alpha = 0.6) +
  geom_abline(lty = 3) +
  coord_equal() +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 10)) +
  facet_wrap(~species)
ggsave(here(pred_folder, "output", "figs", "pred_log_model", "pred_roc_plots.jpg"), width = 5, height = 3)
