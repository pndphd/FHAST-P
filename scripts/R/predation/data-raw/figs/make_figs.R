
# load libraries ----------------------------------------------------------

library(broom)
library(here)

# source scripts ----------------------------------------------------------

source(file = here('scripts/R/predation/data-raw/dis_to_cov_sim_funcs.R'))
source(file = here('scripts/R/predation/data-raw/cov_sim_params.R'))

# load data and set paths -------------------------------------------------

df <- read_csv(here('scripts/R/predation/data/simulation/cover_simulation_data.csv'), show_col_types = FALSE)
output_path <- here('scripts/R/predation/output/figs/cover_simulation/')

# make fig of example polygon ---------------------------------------------

# create a polygon out of a specified number of shapes (n_poly) and a maximum possible area (max_area); 
# for reference, the cell size is 1
poly <- create_polygon(cover_sim_params, max_area = .3, n_poly =  4)

# generate a number of random points within the cell
pts <- generate_fish_locs(cover_sim_params) %>% sample_n(30)

# plot the data

ggplot() +
  geom_point(data = pts, 
             mapping = aes(x = x, y = y)) +
  geom_sf(data = poly,
          fill = "#003300", 
          color = "green", 
          alpha = 0.6) +
  theme_bw() +
  coord_sf(xlim = c(0, 1),
           ylim = c(0, 1)) + 
  labs(x = 'cell width',
       y = 'cell height',
       color = 'cover') +
  theme(axis.title = element_text(size = 10))
ggsave(here(output_path, 'polygon_sim.jpeg'), width = 3, height = 3)


# plot the full simulation data set and model fit -------------------------

model_fit <- df %>%
  dplyr::select(pct_cover, mean_dis_w_0) %>% # mean_dis_w_0 is just one of the two simulations run; includes all values with 0 dis to cover
  tidyr::nest(data = dplyr::everything()) %>% # use the nesting trick to git a model
  dplyr::mutate(fit = purrr::map( # fit a polynomial: x^0.5 + x + x^1.5
    .x = data,
    .f = ~ stats::lm(
      mean_dis_w_0 ~ sqrt(pct_cover) * pct_cover,
      data = .
    )))

model_fit %>% 
  dplyr::mutate(pred = map(fit, predict)) %>% 
  select(data, pred) %>% 
  unnest(everything()) %>% 
  ggplot()  +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title = element_text(size = 10)) +
  geom_point(aes(x = pct_cover, y = mean_dis_w_0), alpha = 0.1) +
  geom_line(aes(x = pct_cover, y = pred), color = "red", alpha = 0.6, size = 1.2) +
  labs(x = "percent cover",
       y = "mean distance to cover")
ggsave(here(output_path, 'model_fit.jpeg'), width = 5, height = 3)

