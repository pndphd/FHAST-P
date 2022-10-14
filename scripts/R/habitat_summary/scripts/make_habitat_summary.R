##### Description #####
# This is the main folder to summarize habitat over the time window
# libraries

##### Load Libraries and functions #####
# Load Libraries and some base parameters
# Load Libraries and some base parameters
# source(here("scripts","R","main","load_libraries.R"))
# source(here("scripts","R","main","default_initialization.R"))
library(here)
source(here("scripts", "R", "main", "load_libraries.R"))

# Load the functions
source(here("scripts", "R", "habitat_summary", "scripts", "functions_habitat_summary.R"))

# Load the pred risk functions
source(file = here("scripts", "R", "predation", "R", "add_predators", "add_preds_funcs.R"))

##### Load Files #####
# load the basic input file
## This is still here for now because of the predation parameters directly loaded from input_data
input_data <- read.csv(
  file = here(input_folder, fhast_config_file),
  sep = "=",
  row.names = 1,
  header = FALSE
) %>%
  # Trim off white spaces form values
  rename(value = 1) %>%
  mutate(value = str_trim(value, side = c("both")))

# load the res files
res_file <- read.csv(
  file = grid_res_path,
  sep = "=",
  row.names = 1,
  header = FALSE
) %>%
  # Trim off white spaces form values
  rename(value = 1) %>%
  mutate(value = str_trim(value, side = c("both")))

# Load the habitat file
hab_file <- read.csv(
  file = hab_path,
  sep = "=",
  row.names = 1,
  header = FALSE
) %>%
  # Trim off white spaces form values
  rename(value = 1) %>%
  mutate(value = str_trim(value, side = c("both")))

# Load the daily input file
daily_file_name <- here("temporary", "NetLogo", "daily_input_file.csv")

# Load the fish parameter lists
fish_parameter_list_path <- here("temporary", "NetLogo", "fish_params_list.rds")

# Load predator glm parameters
pred_proj_path <- here::here("scripts", "R", "predation")

# Parameters for the logestic model
model_params_name <- here(pred_proj_path, "output", "pred_log_params.csv")

# Parameters for the temperature
temp_params_name <- here(pred_proj_path, "output", "pred_temperature_params.csv")
# csv file of predator lengths
pred_length_name <- here(pred_proj_path, "output", "pred_length_dist_params.csv")

pct_cover_name <- here(pred_proj_path, "output", "pct_cov_convers_model.RDS")

dis_to_cover_name <- here(pred_proj_path, "output", "dis_to_cov_model.RDS")

# place relevant input parameters into a list
script_params <- list(
  velocity_cutoff = hab_file["velocity cutoff", ],
  depth_cutoff = hab_file["depth cutoff", ],
  resolution = res_file["resolution", ],
  buffer = res_file["buffer", ],
  preds_per_hectare = input_data["predator density", ],
  conv = input_data["conversion", ],
  reaction_distance = input_data["reaction distance", ]
) %>% map(as.numeric) # make sure values are numeric rather than strings

# Load data files (these require the resolution to be loaded first)
raster_file_name <- here(
  "temporary", "NetLogo",
  paste0(
    "Depth_Velocity_Data_Input_",
    script_params$resolution, "_",
    script_params$buffer, ".csv"
  )
)

shape_file_name <- here(
  "temporary", "NetLogo",
  paste0(
    "Shape_Data_Input_",
    script_params$resolution, "_",
    script_params$buffer, ".csv"
  )
)

grid_file_name <- here("temporary", "R", paste0(
  "river_grid_",
  script_params$resolution, "_", script_params$buffer, ".rds"
))

# Check to see if the temporaty files exists
if (0 < sum(!file.exists(
  shape_file_name,
  raster_file_name,
  daily_file_name,
  grid_file_name,
  model_params_name,
  temp_params_name
))) {
  stop("An input file does not exist.")
}

# Load the temporaty files data files
shape_file <- read.csv(shape_file_name)
raster_file <- read.csv(raster_file_name)
daily_file <- read.csv(daily_file_name) %>%
  mutate(date = mdy(date))
grid_file <- readRDS(grid_file_name) %>%
  select(distance, lat_dist)
model_params <- read_csv(model_params_name)
# Parameters for the temperature
temp_params <- read_csv(temp_params_name)
fish_parameter_list <- readRDS(fish_parameter_list_path)
pred_length_data <- read_csv(pred_length_name)
dis_to_cover_model <- readRDS(dis_to_cover_name)
pct_cover_model <- readRDS(pct_cover_name)

##### Pre Processing #####
# Join the shape and raster files
# (both are just csv files made from shape and raster files)
habitat_temp <- shape_file %>%
  select(-area) %>%
  left_join(raster_file, by = c("distance", "lat_dist"))

# make the data just one depth, vel and wetted area per flow per cell
input_variables <- c("mean.D", "mean.V", "wetd.")
output_variables <- c("depth", "velocity", "wetted_fraction")

spread_data <- future_map2(
  input_variables, output_variables,
  ~ spread_flows(habitat_temp, .x, .y)
) %>%
  reduce(left_join, by = c("lat_dist", "distance", "flow"))

habitat <- habitat_temp %>%
  select(
    -starts_with("mean.D"),
    -starts_with("mean.V"),
    -starts_with("wetd.D")
  ) %>%
  right_join(spread_data, by = c("lat_dist", "distance"))

# remove some unused things
rm(spread_data, habitat_temp)

# get all the flows for which we have a raster
flows <- as.numeric(unique(habitat$flow))

# get all the fish combos
fish_combos <- expand.grid(
  species = map(fish_parameter_list, ~ return(.x[1, ])),
  life_stage = c("juvenile", "adult")
)

# get the total number or days
final_day <- max(daily_file$day)

##### Main Work #####

##### Run the time series #####
# Make the time series data
time_series_data <- daily_file %>%
  split(seq(nrow(.))) %>%
  reduce(~ make_time_series_data(
    past_list = .x,
    current = .y,
    habitat_data = habitat,
    flows_list = flows,
    fish_params = fish_parameter_list,
    sig_figs = 10,
    max_day = final_day,
    model_params = model_params,
    script_params = script_params,
    temp_params = temp_params,
    pred_length_data = pred_length_data,
    pct_cover_model = pct_cover_model
  ),
  .init = 0
  )

# Divide out the outputs
# First Habitat map
mean_map <- time_series_data$map_habitat 
# make a zero depth screen
depth_screen <- mean_map %>%
  select(distance, lat_dist)

# Second fish map
mean_fish_maps <- time_series_data$map_fish %>%
  map(~ right_join(.x, depth_screen, by = c("distance", "lat_dist"))) %>%
  map(~ st_as_sf(.x))
# Put into df format
fish_maps_df <- mean_fish_maps %>% 
  reduce(~bind_rows(.x, .y))

# Third the daily habitat
daily_habitat_data <- time_series_data$daily_habitat %>% 
  left_join(select(daily_file, date, day), by = "day")

# Fourth the daily fish
daily_fish_data <- time_series_data$daily_fish %>% 
  map(~left_join(.x, select(daily_file, date, day), by = "day"))

##### Basic Calculations #####
# Do daily calculations
daily_wetted_area <- multiply_and_sum(
  data_frame = daily_habitat_data,
  col_1 = wetted_fraction,
  col_2 = area,
  new_col = wetted_area
)

shallow_area <- time_series_data$map_habitat %>%
  filter(depth < script_params$depth_cutoff) %>%
  sum_daily_data(
    col_1 = area,
    new_col = shallow_area
  )

##### Summary Stats #####

summary_stats <- data.frame(cover_area_m2 = sum((mean_map$veg + mean_map$wood) *
  mean_map$area * mean_map$wetted_fraction)) %>%
  mutate(
    area_below_v_cutoff_m2 = sum(mean_map$below_v_cutoff *
      mean_map$area * mean_map$wetted_fraction) /
      sum(mean_map$area * mean_map$wetted_fraction),
    area_below_d_cutoff_m2 = sum(mean_map$below_d_cutoff *
      mean_map$area * mean_map$wetted_fraction) /
      sum(mean_map$area * mean_map$wetted_fraction),
    near_shore_area_m2 = sum(mean_map$area * mean_map$wetted_fraction *
      mean_map$near_shore),
    c_b_c_fraction = sum((mean_map$veg + mean_map$wood) *
      mean_map$area * mean_map$wetted_fraction *
      mean_map$below_v_cutoff * mean_map$below_d_cutoff) /
      sum(mean_map$area * mean_map$wetted_fraction)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Value") %>% 
  mutate(Item = str_replace_all(Item, "_", " "))

# Currently this function is here for ease of editing
calc_fish_summary_stats <- function(df) {
  output <- data.frame(species = df$species[1]) %>%
    mutate(
      species = str_replace_all(species, "_", " "),
      life_stage = df$life_stage[1],
      met_j_per_day = mean(df$fish_met_j_per_day, na.rm = TRUE)
    )
}

fish_summary_stats <- mean_fish_maps %>%
  map_df(~ calc_fish_summary_stats(.x))

##### Plots #####
##### Line Plots #####
# 1D wetted plot area
cover_scatter_plot <- make_scatter_plot(
  data_frame = daily_habitat_data,
  x_axis = flow_cms,
  y_axis = total_cover,
  x_lab = "Flow (cms)",
  y_lab = expression(Cover ~ Area ~ (m^2))
)
x11()
print(cover_scatter_plot)

##### Heat maps #####
# 2D area cover heat map
heat_map_plot <- make_heat_map(data_frame = mean_map,
                               x_axis = depth,
                               y_axis = velocity,
                               z_axis = wetted_area,
                               x_lab = "Depth (m)",
                               y_lab = "Velcoity (m/s)",
                               z_lab = expression(Area ~ (m^2)),
                               resolution = 100)
x11()
print(heat_map_plot)

##### Maps #####
# wetted map
cover_map <- make_map(
  data_frame = mean_map,
  fill = veg + wood,
  scale_name = "Cover\nFraction"
)
x11()
print(cover_map)

# Cutoff map
cutoff_map <- make_map(
  data_frame = mean_map %>%
    filter(
      depth < script_params$depth_cutoff,
      velocity < script_params$velocity_cutoff
    ),
  fill = (wood + veg),
  scale_name = "Cover\nFraction"
)
x11()
print(cutoff_map)

# predator maps
pred_map <- make_map(
  data_frame = mean_map,
  fill = pikeminnow_hab_rating,
  scale_name = "Predator\nHabitat Rating"
)
x11()
print(pred_map)

# Slant map
slant_map <- make_map(
  data_frame = mean_map,
  fill = mean.correction_factor,
  scale_name = "Area Correction\nFactor"
)
x11(height = 10, width = 20)
print(slant_map)

# cover facte map
d_cutoff_label <- c("1" = "Below D", "0" = "Above D")
v_cutoff_label <- c("1" = "Below V", "0" = "Above V")
cover_facet_map  <- make_map(
  data_frame = mean_map,
  fill = (veg + wood),
  scale_name = "Cover"
) + facet_grid(below_d_cutoff ~ below_v_cutoff,
               labeller = labeller(below_d_cutoff = d_cutoff_label,
                                   below_v_cutoff = v_cutoff_label))
x11(height = 10, width = 20)
print(cover_facet_map)

# Metabolic map
metabolic_map <- mean_fish_maps %>% map(~make_map(
  data_frame = .x,
  fill = fish_met_j_per_day,
  scale_name = "Metabolic Rate\n(j/day)",
  title = str_replace_all(paste0(.x$species[1],"-",.x$life_stage[1]),
                          "_", " ")
)) %>% 
  wrap_plots(ncol = 1)
x11(height = 15, width = 15)
print(metabolic_map)

# Predation map
predation_map <- mean_fish_maps %>% map(~make_map(
  data_frame = .x,
  fill = pred_mort_risk,
  scale_name = "Mortality\nRisk",
  title = str_replace_all(paste0(.x$species[1],"-",.x$life_stage[1]),
                          "_", " ")
)) %>% 
  wrap_plots(ncol = 1)
x11(height = 15, width = 15)
print(predation_map)

##### Save Outputs #####

# List of objects and names for the save files 
object_list = list(summary_stats,
                   fish_summary_stats,
                   cover_scatter_plot,
                   cover_map,
                   cover_facet_map,
                   pred_map,
                   cutoff_map,
                   heat_map_plot,
                   metabolic_map,
                   predation_map)
name_list = list("summary_stats_table.rds",
              "summary_fish_stats_table.rds",
              "cover_scatter_plot.rds",
              "cover_map.rds",
              "cover_facet_map.rds",
              "predator_map.rds",
              "cutoff_map.rds",
              "depth_velocity_heatmap.rds",
              "metabolic_map.rds",
              "predation_map.rds")

# Save all the outputs 
walk2(object_list, name_list, ~saveRDS(
  object = .x,
  file = here("temporary", "R", .y)))

