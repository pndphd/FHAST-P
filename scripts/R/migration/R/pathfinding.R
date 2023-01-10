library(tidyverse)
library(here)
library(igraph)
library(sf)
library(microbenchmark)

source(here("scripts/R/migration/R/pathfinding_functions.R"))
# swim speed stuff
f_length <- 150 / 100
beta <- 0.148
adg <- 9.81
rho <- 1
body_mass <- 6234
swim_speed_max <- 2 * f_length
swim_speed_opt <- beta * rho ^ (-1/6) * adg ^ (1/2) * body_mass ^ (1/6)

# Martin et al. burst speed / exhaustion approach

c_0 <- 65.51
b <- -0.217
d <- 0.068
c_1 <- 420.6
beta_2 <- -0.369
gamma <- 2.130
pc <- 633
N_max <- 103
gamma_r <- 1.82

get_pm <- function(mass, temp){
  c_0 * (mass ^ b) * exp(d * temp)
}

get_ps <- function(mass, swim_speed){
  c_1 * (mass ^ beta_2) * (swim_speed ^ gamma)
}

get_cot <- function(mass, swim_speed, water_velocity, temp){
  pm <- get_pm(mass, temp)
  ps <- get_ps(mass, swim_speed)
  
  (pm + ps + ps * max(0, gamma_r * (pm + ps + pc) / (water_velocity - swim_speed)) + max(0, (gamma_r - 1) * (pm + ps + pc))) / (swim_speed - water_velocity)
}

# Jesse's equations

chinook <- function(mass, temp, body_length, velocity){
  exp(2.76 + 0.862 * log(mass) + 0.633 * log(temp) + 0.09 * (velocity / body_length) + 0.045 * log(mass) * (velocity / body_length))
}

min_depth <- 0.5 * f_length
# 
st_read('scripts/R/migration/data/test_ap_small.shp', quiet=TRUE) %>%
  select(starts_with("dist"), starts_with("lat"), starts_with("velo"), starts_with("dept"), area) %>%
  rename(distance = starts_with("dist"), lat_dist = starts_with("lat"), velocity = starts_with("velo")) %>%
  arrange(distance, lat_dist) %>%
  mutate(id = seq(1, nrow(.))) %>%
  mutate(velocity = if_else(velocity > 1, 2, 1.8)) %>%
  filter(depth >= min_depth) %>% 
  st_write(here('scripts/R/migration/data/high_vel.shp'),append=F)

shape <- st_read(here('scripts/R/migration/data/test_ap_small.shp'), quiet=TRUE) %>%
  select(starts_with("dist"), starts_with("lat"), starts_with("velo"), starts_with("dept"), area, temp) %>%
  rename(distance = starts_with("dist"), lat_dist = starts_with("lat"), velocity = starts_with("velo")) %>%
  arrange(distance, lat_dist) %>%
  filter(depth >= min_depth) %>% 
  mutate(id = seq(1, nrow(.)))

df <- shape %>% 
  as_tibble() %>% 
  select(-geometry)

cell_width <- 20

min_dis <- df %>% pull(distance) %>% min()
max_dis <- df %>% pull(distance) %>% max()
min_lat <- df %>% pull(lat_dist) %>% min()
max_lat <- df %>% pull(lat_dist) %>% max()

dis <- seq(min_dis, max_dis, by = cell_width)
lat <- seq(min_lat, max_lat, by = cell_width)
box <- expand_grid(distance = seq(min_dis, max_dis, by = cell_width), lat_dist = seq(min_lat, max_lat, by = cell_width))

df_pos <- df %>% mutate(possible = TRUE)

box_full <- left_join(box, df_pos, by = c("distance", "lat_dist")) %>% 
  select(distance, lat_dist, possible, id, velocity, area) %>% 
  mutate(possible = replace_na(possible, FALSE)) %>% 
  rename(to = id, to_velocity = velocity)

relations <- df_pos %>% 
  mutate(coords = map2(distance, lat_dist, ~ get_node_coords(.x, .y, cell_width))) %>% unnest(coords)%>% 
  mutate(node_dist = map_dbl(coords, ~ .x$dist),
         node_lat_dist = map_dbl(coords, ~ .x$lat_dist),
         node_dir = map_chr(coords, ~ .x$dir)) %>% 
  inner_join(box_full, by = c("node_dist" = "distance", "node_lat_dist" = "lat_dist", "possible")) %>% 
  mutate(to_velocity = if_else(to_velocity > 1, 2, 1.8),
         travel_dis_a = if_else(
           node_dir == "f", area.x / cell_width / 2,
           sqrt(cell_width^2 + (area.x/cell_width)^2) / 2
         ),
         travel_dis_b = if_else(
           node_dir == "f", area.y/ cell_width / 2,
           sqrt(cell_width^2 + (area.y/cell_width)^2) / 2)) %>% 
  select(id,  to, node_dir,velocity, to_velocity, travel_dis_a, travel_dis_b, temp) %>%
  rename(from = id) %>% 
  mutate(swim_speed_a = case_when((swim_speed_opt - velocity > 0 & node_dir == "f") ~ swim_speed_opt,
                                  (swim_speed_opt / sqrt(2) - velocity > 0 & node_dir == "s") ~ swim_speed_opt,
                                  TRUE ~ swim_speed_max),
         swim_speed_b = case_when((swim_speed_opt - to_velocity > 0 & node_dir == "f") ~ swim_speed_opt,
                                  (swim_speed_opt / sqrt(2) - to_velocity > 0 & node_dir == "s") ~ swim_speed_opt,
                                  TRUE ~ swim_speed_max),
         overground_vel_a = if_else(node_dir == "f", swim_speed_a - velocity, swim_speed_a / sqrt(2) - velocity),
         overground_vel_b = if_else(node_dir == "f", swim_speed_b - to_velocity, swim_speed_b / sqrt(2) - to_velocity),
         time_cost_a = travel_dis_a / overground_vel_a,
         time_cost_b = travel_dis_b / overground_vel_b,
         cot = get_cot(body_mass/1000, swim_speed_a, velocity, temp) * travel_dis_a * body_mass / 1000 + get_cot(body_mass/1000, swim_speed_b, to_velocity, temp) * travel_dis_b * body_mass / 1000,
         jesse_cost = chinook(body_mass, temp, f_length, swim_speed_a) / (24 * 60 * 60) * time_cost_a + chinook(body_mass, temp, f_length, swim_speed_b) / (24 * 60 * 60) * time_cost_b)

actors <- df %>% 
  select(id)

g <- graph_from_data_frame(relations, vertices = actors)

to <- df %>% filter(distance == max(distance)) %>% pull(id)
from <- df %>% filter(distance == min(distance)) %>% pull(id)
weights <-  relations %>% pull(jesse_cost)

distances(g, weights = weights, mode = "out", v = from, to = to) %>% 
  as_tibble(rownames = 'from') %>% 
  pivot_longer(cols = -from, names_to = "to", values_to = "distance")

path_data <- 
  map(from, ~ shortest_paths(g, weights = weights, mode = "out", from = .x, to = to)$vpath %>% 
      map(as.numeric) %>% 
      map(~ tibble(id = .x)) %>% 
      map(~ left_join(.x, df, by = "id"))) %>% 
  bind_rows() %>% 
  count(id)

left_join(df, path_data, by = "id") %>% 
  mutate(n = replace_na(n, 0))

path <- tibble(id = as.numeric(shortest_paths(g, weights = weights, mode = "out", from = 3, to = to[[4]])[[1]][[1]]))

left_join(path, shape)  %>% st_write(here("scripts/R/migration/data/path.shp"), append=FALSE)

tibble(test = 1:10) %>% write_delim(here("scripts/R/migration/data/test.txt"), delim = "=")
tibble(test = 1:10) %>% fwrite(here("scripts/R/migration/data/test.txt"), sep = "=")

pos_coords <- map2(shape$lat_dist, shape$distance, ~ c(.x, .y))
