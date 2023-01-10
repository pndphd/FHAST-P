########################################
# This is the main script to summarize habitat over the time window
########################################

##### Load Functions #####
source(here("scripts", "R", "habitat_summary", "scripts",
            "functions_habitat_summary.R"))
source(here("scripts", "R", "predation", "R", "add_predators",
            "add_preds_funcs.R"))
source(here("scripts", "R", "habitat_summary", "scripts",
            "make_cell_data.R"))
source(here("scripts", "R", "habitat_summary", "scripts",
            "get_daily_v_and_d.R"))
source(here("scripts", "R", "habitat_summary", "scripts",
            "calculate_predators.R"))
source(here("scripts", "R", "habitat_summary", "scripts",
            "calculate_fish_summary.R"))
source(here("scripts", "R", "habitat_summary", "scripts",
            "make_time_series_data.R"))

##### Load Files #####
# The name for the daily input file
daily_input_path <- here("temporary", "NetLogo", "daily_input_file.csv")

# Percent Cover conversion model
pct_cover_name <- here(temp_folder, "NetLogo", "pct_cov_convers_model.rds")

# distance to cover model
dis_to_cover_name <- here(temp_folder, "NetLogo", "dis_to_cov_model.rds")

# The name of the raster file data
raster_file_name <- here("temporary", "NetLogo", paste0(
  "Depth_Velocity_Data_Input_",
  habitat_parm$resolution, "_",
  habitat_parm$buffer, ".csv"))

# The name for the shape file data
shape_file_name <- here("temporary", "NetLogo", paste0(
  "Shape_Data_Input_",
  habitat_parm$resolution, "_",
  habitat_parm$buffer, ".csv"))

# The grid file (for georeferencing)
grid_file_name <- here("temporary", "R", paste0(
  "river_grid_",
  habitat_parm$resolution, "_",
  habitat_parm$buffer, ".rds"))

# Load the files named above
shape_file <- read.csv(shape_file_name)
raster_file <- read.csv(raster_file_name)
daily_file <- read.csv(daily_input_path) %>%
  mutate(date = mdy(date))
grid_file <- readRDS(grid_file_name) %>%
  select(distance, lat_dist)
dis_to_cover_model <- read_rds(dis_to_cover_name)
pct_cover_model <- read_rds(pct_cover_name)

##### Pre Processing #####
# Make a data frame with each cell at each flow value
habitat <- make_cell_data()

# get all the flows for which we have a raster
flows <- as.numeric(unique(habitat$flow))

# get all the fish combos
fish_combos <- expand.grid(
  species = fish_parm$specie,
  life_stage = c("juvenile", "adult"))

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
    fish_params_in = fish_parm,
    sig_figs = 10,
    max_day = final_day,
    model_params = pred_model_params,
    script_params = habitat_parm,
    temp_params = pred_temp_params,
    pred_length_data = pred_length_data,
    pct_cover_model = pct_cover_model,
    gape_params = gape_params),
  .init = 0)

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
  filter(depth < habitat_parm$dep_cutoff) %>%
  sum_daily_data(col_1 = area,
                 new_col = shallow_area)

##### Summary Stats #####
# A set of summary stats for the habitat
summary_stats <- data.frame(cover_area_m2 = sum((mean_map$veg + mean_map$wood) *
  mean_map$area * mean_map$wetted_fraction)) %>%
  mutate(
    near_shore_cover_area_m2 = sum(mean_map$area *
                                     mean_map$wetted_fraction *
                                     mean_map$near_shore *
                                     (mean_map$veg + mean_map$wood)),
    near_shore_cover_area_bellow_v_m2 = sum(mean_map$area *
                                     mean_map$wetted_fraction *
                                       mean_map$below_v_cutoff*
                                     mean_map$near_shore *
                                     (mean_map$veg + mean_map$wood)),
    percent_area_below_v_cutoff = sum(mean_map$below_v_cutoff *
      mean_map$area * mean_map$wetted_fraction) /
      sum(mean_map$area * mean_map$wetted_fraction)*100,
    percent_area_below_d_cutoff = sum(mean_map$below_d_cutoff *
      mean_map$area * mean_map$wetted_fraction) /
      sum(mean_map$area * mean_map$wetted_fraction)*100,
    percent_near_shore_area = sum(mean_map$area * mean_map$wetted_fraction *
      mean_map$near_shore)/
      sum(mean_map$area * mean_map$wetted_fraction)*100,
    CBC_percent = sum((mean_map$veg + mean_map$wood) *
      mean_map$area * mean_map$wetted_fraction *
      mean_map$below_v_cutoff * mean_map$below_d_cutoff) /
      sum(mean_map$area * mean_map$wetted_fraction)*100,
    average_cover_percent = sum((mean_map$veg + mean_map$wood) *
                                  mean_map$area * mean_map$wetted_fraction) /
      sum(mean_map$area * mean_map$wetted_fraction)*100
  ) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Value") %>% 
  mutate(Item = str_replace_all(Item, "_", " "),
         Item = str_replace_all(Item, "m2", "(m\u00B2)"),
         Value = round(Value,2))

# Currently this function is here for ease of editing
calc_fish_summary_stats <- function(df) {
  output <- data.frame(species = df$species[1]) %>%
    mutate(
      species = str_replace_all(species, "_", " "),
      life_stage = df$life_stage[1],
      met_j_per_day = round(mean(df$fish_met_j_per_day, na.rm = TRUE),2)
    )
}

# A set of summary stats for the fish
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
display_plot(cover_scatter_plot)

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
display_plot(heat_map_plot)

##### Maps #####
# Set the plot widths
plot_widths = 5

# wetted map
cover_map <- make_map(
  data_frame = mean_map,
  fill = veg + wood,
  scale_name = "Cover\nFraction"
)
display_plot(cover_map)

# Cutoff map
cutoff_map <- make_map(
  data_frame = mean_map %>%
    filter(
      depth < habitat_parm$dep_cutoff,
      velocity < habitat_parm$vel_cutoff
    ),
  fill = (wood + veg),
  scale_name = "Cover\nFraction"
)
display_plot(cutoff_map)

# predator maps
pred_map <- make_map(
  data_frame = mean_map,
  fill = pikeminnow_hab_rating,
  scale_name = "Predator\nHabitat Rating"
)
display_plot(pred_map)

# Slant map
slant_map <- make_map(
  data_frame = mean_map,
  fill = mean.correction_factor,
  scale_name = "Area Correction\nFactor"
)
display_plot(slant_map, 10, 20)

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
display_plot(cover_facet_map, 10, 20)

# cover facet histogram
cover_facet_hist  <- make_hist(
  data_frame = mean_map,
  bins = pct_cover,
  x_label = "Percent Cover",
  weights = wetted_area) +
  facet_grid(below_d_cutoff ~ below_v_cutoff,
             labeller = labeller(below_d_cutoff = d_cutoff_label,
                                   below_v_cutoff = v_cutoff_label))
display_plot(cover_facet_hist, 10, 20)

# Metabolic map
metabolic_map <- mean_fish_maps %>% map(~make_map(
  data_frame = .x,
  fill = fish_met_j_per_day,
  scale_name = "Metabolic Rate\n(j/day)",
  title = str_replace_all(paste0(.x$species[1],"-",.x$life_stage[1]),
                          "_", " "))) %T>% 
  assign(x = "metabolic_map_length", value = length(.), envir = .GlobalEnv)%>% 
  wrap_plots(ncol = 1, widths = plot_widths,
             heights = metabolic_map_length * plot_widths)
display_plot(metabolic_map, 15, 15)

# Net energy map
net_energy_map <- mean_fish_maps %>% map(~make_map(
  data_frame = .x %>% mutate(net_energy = ifelse(net_energy >=0, net_energy, 0)),
  fill = net_energy,
  scale_name = "Net Energy\n(j/day)",
  title = str_replace_all(paste0(.x$species[1],"-",.x$life_stage[1]),
                          "_", " "))) %T>% 
  assign(x = "net_energy_map_length", value = length(.), envir = .GlobalEnv)%>% 
  wrap_plots(ncol = 1, widths = plot_widths,
             heights = net_energy_map_length * plot_widths)
display_plot(net_energy_map, 15, 15)

# Predation map
predation_map <- mean_fish_maps[c(which(fish_summary_stats$life_stage == "juvenile"))] %>%
  map(~make_map(
  data_frame = .x,
  fill = pred_mort_risk,
  scale_name = "Mortality\nRisk",
  title = str_replace_all(paste0(.x$species[1],"-",.x$life_stage[1]),
                          "_", " "))) %T>% 
  assign(x = "predation_map_length", value = length(.), envir = .GlobalEnv) %>% 
  wrap_plots(ncol = 1, widths = plot_widths,
             heights = predation_map_length * plot_widths)
display_plot(predation_map, 15, 15)

##### Save Outputs #####

# List of objects and names for the save files 
plot_list = list(cover_scatter_plot,
                 cover_map,
                 cover_facet_map,
                 cover_facet_hist,
                 pred_map,
                 cutoff_map,
                 heat_map_plot,
                 metabolic_map,
                 predation_map,
                 net_energy_map)
data_list = list(summary_stats,
                 fish_summary_stats)
plot_name_list = list("cover_scatter_plot",
                      "cover_map",
                      "cover_facet_map",
                      "cover_facet_hist",
                      "predator_map",
                      "cutoff_map",
                      "depth_velocity_heatmap",
                      "metabolic_map",
                      "predation_map",
                      "net_energy_map")
data_name_list = list("summary_stats_table",
                      "summary_fish_stats_table")
object_list = c(data_list, plot_list)
object_name_list = c(data_name_list, plot_name_list)
plot_dimeshions = list(1.2,1.2,1.2,1.2,1.2,1.2,1.2,
                       metabolic_map_length+1,
                       predation_map_length+1,
                       net_energy_map_length+1)

# Save all the outputs 
walk2(object_list, object_name_list, ~saveRDS(
  object = .x,
  file = here("temporary", "R", paste0(.y, ".rds"))))
pwalk(list(plot_list, plot_name_list, plot_dimeshions), ~ggsave(
  height = ..3*5,
  plot = ..1,
  filename = here("temporary", "R", paste0(..2, ".png")),
  limitsize = FALSE,
  device = "png"))

