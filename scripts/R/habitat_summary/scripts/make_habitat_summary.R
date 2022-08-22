##### Description #####
# This is the main folder to summarize habitat over the time window
# libraries 

# Load Libraries and some base parameters
source("./scripts/R/main/load_libraries.R")

# Load the functions
source("./scripts/R/habitat_summary/scripts/functions_habitat_summary.R")

# Load the pred risk functions
source(file = here("scripts", "R", "predation", "R", "add_predators", "add_preds_funcs.R"))

##### Load Files #####
input_data <- read.csv(
  file = paste0(input_folder, input_file),
  sep = "=",
  row.names = 1,
  header = FALSE
) %>%
  # Trim off white spaces form values
  rename(value = 1) %>%
  mutate(value = str_trim(value, side = c("both")))

# inputs for development
script_params <- list(
  resolution = input_data["resolution", ],
  buffer = input_data["buffer", ],
  preds_per_hectare = input_data["predator density", ],
  conv = input_data["conversion", ],
  reaction_distance = input_data["reaction distance", ]
) %>% map(as.numeric) # make sure values are numeric rather than strings

# Load data files
raster_file_name <- paste0(
  "./temporary/NetLogo/Depth_Velocity_Data_Input_",
  script_params$resolution, "_", script_params$buffer, ".csv"
)
shape_file_name <- paste0(
  "./temporary/NetLogo/Shape_Data_Input_",
  script_params$resolution, "_", script_params$buffer, ".csv"
)
daily_file_name <- "./temporary/NetLogo/daily_input_file.csv"
grid_file_name <- paste0(
  "./temporary/R/river_grid_",
  script_params$resolution, "_", script_params$buffer, ".rds"
)

# Load predator glm parameters
pred_proj_path <- here::here("scripts", "R", "predation")
# Parameters for the logestic model
model_params_name <- here(pred_proj_path, "output", "pred_log_params.csv")
# Parameters for the temperature
temp_params_name <- here(pred_proj_path, "output", "pred_temperature_params.csv")

# Check to see if the files exists
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

##### Load Files #####
# Load data files
shape_file <- read.csv(shape_file_name)
raster_file_nas <- read.csv(raster_file_name)
daily_file <- read.csv(daily_file_name) %>%
  mutate(date = mdy(date))
grid_file <- readRDS(grid_file_name)
model_params <- read_csv(model_params_name)
# Parameters for the temperature
temp_params <- read_csv(temp_params_name)

##### Pre Processing #####
# Remove all the -9999's form the raster derived csv
raster_file <- raster_file_nas %>%
  mutate(across(
    starts_with(c("mean.D", "mean.V")),
    ~ dplyr::if_else(.x < 0, 0, .x)
  ))

# Join the shape and raster files
# (both are just csv files made from shape and raster files)
habitat_temp <- shape_file %>%
  select(-area) %>%
  left_join(raster_file, by = c("distance", "lat_dist"))

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

# get all the flows for which we have a raster
flows <- as.numeric(unique(habitat$flow))

##### Main Work #####

##### Run the time series #####
# Make the time series data
time_series_data <- make_time_series_data(
  daily_file,
  habitat,
  flows,
  sig_figs = 2
) %>%
  mutate(
    wetted = dplyr::if_else(depth > 0, 1, 0),
    substrate = rowSums(across(gravel:rock)),
    substrate = dplyr::if_else(substrate >= fine & substrate > 0, 1, 0)
  ) %>%
  rename(shade_orig = shade) %>% 
  mutate(shade = dplyr::if_else(shade_orig >= 0.5, 1, 0)) %>% 
  # calculate predation risk
  calc_preds_per_time(
    day,
    model_params,
    script_params,
    temp_params
  ) %>%
  select(-c(substrate, shade)) %>% 
  rename(shade = shade_orig)

# TODO Check area vs bottom area

##### Do daily calculations #####
daily_wetted_area <- multiply_and_sum(
  data_frame = time_series_data,
  col_1 = wetted,
  col_2 = area,
  new_col = wetted_area
)

shallow_area <- time_series_data %>%
  filter(
    depth < 1,
    depth > 0
  ) %>%
  sum_daily_data(
    col_1 = area,
    new_col = shallow_area
  )

##### Do Average Area Calculations #####
average_map <- average_and_map(
  data_frame = time_series_data,
  grid = grid_file
) %>%
  filter(depth > 0)

##### Plots #####
# 1D wetted plot area
shallow_area_plot <- make_line_plot(
  data_frame = shallow_area,
  x_axis = day,
  y_axis = shallow_area,
  x_lab = "Date",
  y_lab = expression(Shallow ~ Area ~ (m^2))
)
x11(xpos = 2000)
print(shallow_area_plot)

# 2D area cover heat map
heat_map_plot <- time_series_data %>%
  filter(
    depth > 0.2,
    velocity > 0.2
  ) %>%
  make_heat_map(
    x_axis = depth,
    y_axis = velocity,
    z_axis = area,
    x_lab = "Depth (m)",
    y_lab = "Velcoity (m/s)",
    z_lab = expression(Area ~ (m^2)),
    resolution = 100
  )
x11(xpos = 2000)
print(heat_map_plot)

# Depth map
depth_map <- ggplot(data = average_map) +
  theme_classic(base_size = 20) +
  geom_sf(aes(fill = wetted), color = NA) +
  scale_fill_viridis(name = "Wetted")
x11(height = 10, width = 20, xpos = 2000)
print(depth_map)

# Cutoff map
cutoff_data <- average_map %>%
  filter(
    depth < 2,
    lwd > 0.1
  )
cutoff_map <- ggplot(data = cutoff_data) +
  theme_classic(base_size = 20) +
  geom_sf(aes(fill = depth), color = NA) +
  scale_fill_viridis(name = "Depth (m)")
x11(height = 10, width = 20, xpos = 2000)
print(cutoff_map)

# predator maps
pred_map <- ggplot(data = average_map) +
  theme_classic(base_size = 20) +
  geom_sf(aes(fill = preds), color = NA) +
  scale_fill_viridis(name = "predators")
x11(height = 10, width = 20, xpos = 2000)
print(pred_map)

bass_map <- ggplot(data = average_map) +
  theme_classic(base_size = 20) +
  geom_sf(aes(fill = bass), color = NA) +
  scale_fill_viridis(name = "bass")
x11(height = 10, width = 20, xpos = 2000)
print(bass_map)

sasq_map <- ggplot(data = average_map) +
  theme_classic(base_size = 20) +
  geom_sf(aes(fill = sasq), color = NA) +
  scale_fill_viridis(name = "sasq")
x11(height = 10, width = 20, xpos = 2000)
print(sasq_map)
