
# 0. load libraries ----------------------------------------------------

library(tidyverse)
library(here)

options(readr.show_col_types = FALSE) # turns off notifications when reading csv files

# 1. functions ------------------------------------------------------------
proj_dir <- here::here("scripts", "R", "predation")
source(here::here(proj_dir,"R", "cover_survival_benefit", "cover_benefit_funcs.R"))

# file paths and params ---------------------------------------------------

cover_data_path <- here::here(proj_dir, "data", "lit_search", "cover_data.csv")
cover_sim_path <- here::here(proj_dir, "data", "simulation", "cover_simulation_data.csv")

cell_size_m <- 5

# 3. analysis -------------------------------------------------------------

readr::read_csv(cover_data_path) %>%
  # this section cleans and fits a glm to observed data of fish vs. distance to cover
  dplyr::group_by(fish_size_mm) %>% # group by fish size
  get_unitless_y(cumulative_fraction) %>%
  dplyr::ungroup() %>%
  dplyr::select(dis_to_cover_m, unitless_y) %>%
  tidyr::nest(data = dplyr::everything()) %>% # nesting allows the glm function to piped onto the dataframe
  dplyr::mutate(fit = purrr::map(     .x = data,    .f = ~ stats::glm(      unitless_y ~ dis_to_cover_m,      family = stats::quasibinomial(logit),      data = .    )  ),
                augment = purrr::map(fit, broom::augment, type.predict = "response")) %>% 
  tidyr::unnest(augment)%>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(dis_to_cover_m, unitless_y), alpha = 0.5, size = 2) +
  ggplot2::geom_line(ggplot2::aes(dis_to_cover_m, .fitted), color = "red", alpha = 0.6, size = 1, show.legend = FALSE) +
  ggplot2::labs(x = "distance to cover (m)", y = "scaled prop. of observed salmonids") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 10))
ggplot2::ggsave(here::here(proj_dir,"output", "figs", "cover_benefits", "cover_benefits.jpg"), width = 5, height = 3)
