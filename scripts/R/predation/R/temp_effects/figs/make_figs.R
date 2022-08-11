
# 0. read in libraries ----------------------------------------------------

library(tidyverse) # data manipulation
library(here) # simplifies file paths

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files


# 1. set file path --------------------------------------------------------
proj_dir <- here::here("scripts", "R", "predation")

data <-  here::here(proj_dir, "data", "lit_search", "temp_data.csv")

# 2. data analysis --------------------------------------------------------

readr::read_csv(data) %>%
  dplyr::group_by(author, year, journal, species, experiment) %>% # group by various factors so only data from the same experiments are changed
  dplyr::mutate(unitless_y = y_value / max(y_value)) %>% # convert to values relative to the max of each experiment
  dplyr::ungroup() %>%
  dplyr::select(temperature_C, unitless_y) %>%
  tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
  dplyr::mutate(
    fit = purrr::map(.x = data, .f = ~ stats::glm(unitless_y ~ temperature_C, family = stats::quasibinomial(logit), data = .)),
    augment = purrr::map(fit, broom::augment, type.predict = "response")
  ) %>%
  tidyr::unnest(augment) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(temperature_C, unitless_y), alpha = 0.5, size = 2) +
  ggplot2::geom_line(ggplot2::aes(temperature_C, .fitted), color = "red", alpha = 0.6, size = 1.0, show.legend = FALSE) +
  ggplot2::labs(x = "temperature (°C)", y = "relative activity") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 10))
  
ggplot2::ggsave(here::here(proj_dir,"output", "figs", "pred_temperature_effects", "temp_vs_pred_activity.jpg"), width = 5, height = 3)
