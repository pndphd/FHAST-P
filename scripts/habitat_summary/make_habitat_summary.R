########################################
# This is the main script to summarize habitat over the time window
########################################

##### Load Functions #####
source(here("scripts", "habitat_summary", "functions_habitat_summary.R"))
source(here("scripts", "habitat_summary", "make_cell_data.R"))
source(here("scripts", "habitat_summary", "get_daily_data.R"))
source(here("scripts", "habitat_summary", "calc_all_preds.R"))
source(here("scripts", "habitat_summary", "make_data_summary.R"))
source(here("scripts", "migration", "pathfinding_functions.R"))

##### Load Files #####
# Load the shape file names
shape_file <- read.csv(here("temporary", 
                            paste0("Shape_Data_Input.csv")))

# Load the raster file
raster_file <- read.csv(here("temporary",  
                             paste0("Depth_Velocity_Data_Input.csv")))

# Load the daily enviromental inputs 
daily_file <- read.csv(here("temporary",  "daily_input_file.csv")) %>%
  mutate(date = mdy(date))

# Load the Grid file
grid_file <- readRDS(here("temporary", 
                          paste0("river_grid.rds"))) %>%
  select(distance, lat_dist)

# Distance to cover model
dis_to_cover_model <- read_rds(here(temp_folder, "dis_to_cov_model.rds"))

# Load the percent cover model
cover_fra_model <- read_rds(here(temp_folder, "pct_cov_convers_model.rds"))

##### Pre Processing #####
# Make a data frame with each cell at each flow value
habitat_flows_all <- make_cell_data() 

# Get all the flows for which we have a raster
flows <- as.numeric(unique(habitat_flows_all$flow))

# Make a screen for cells that are never wet
# Get the mad flow from the dialy enviroemntal file
max_flow = flows[min(which(flows > max(daily_file$flow_cms)))]
# Get all cells that are wet at that max flow
habitat_open <- habitat_flows_all %>% 
  filter(flow == max_flow,
         wetted_fraction != 0) %>% 
  select(lat_dist, distance) 
# Filter the habitat file
habitat_flows = habitat_open %>% 
  left_join(habitat_flows_all)
# Remove large unused file
rm(habitat_flows_all)

# Get all the fish combos
fish_combos <- expand.grid(
  species = fish_parm$specie,
  life_stage = c("juvenile", "adult"))

# Get migration codes over to shape file
 migration_area = raster_file %>% 
  select(starts_with("mig_path_"),
         distance, lat_dist)

# Get a habitat file for the fixerd items
habitat_fixed = shape_file %>% 
  right_join(habitat_open,  by = c("distance", "lat_dist")) %>% 
  select(-starts_with("shade_")) 

# Make an average predator form the predator parameter file
pred_parm <- read_csv(file = predator_path,
                      col_types = cols(.default = "d", species = "c")) %>%
  # the 1 ensures the names are kept
  rename(species_temp = species) %>% 
  pivot_longer(cols=c(-species_temp), names_to="specie") %>%
  pivot_wider(names_from=c(species_temp)) %>% 
  mutate(area_pred_a = convert_logistic_parameters(area_pred_10, area_pred_90)[[1]],
         area_pred_b = convert_logistic_parameters(area_pred_10, area_pred_90)[[2]]) %>%
  select(-specie) %>% 
  as.list() 

##### Main Processing #####

# Make a list with one item being the daily habitat data 
# and the second being the migration data
daily_data_sets = daily_w_photo_period %>% 
  pmap(.f = ~get_daily_data(..., 
                            habitat_data = habitat_flows,
                            fixed_data = shape_file,
                            flows_list = flows,
                            fish_schedule = fish_schedule,
                            migration_area = migration_area,
                            sig_figs = 10)) 

# Extract the habitat data that varies each day
habitat_variable = daily_data_sets %>% 
  map_df(.f = ~.x$v_and_d) %>%
  select(-area) %>% 
  left_join(habitat_fixed, by = c("lat_dist", "distance")) %>% 
  group_by(date) %>% 
  mutate(reach_preds = round(habitat_parm$pred_per_area * sum(wetted_area))) %>% 
  ungroup() 

if(adult_run == TRUE){
  # extract the migration data
  migration_data = daily_data_sets %>% 
    map_df(.f = ~.x$mig_data)
  
  # Get the Final adult migration data
  final_adult_migration <- na.omit(migration_data)
  # Sum all paths in each cell by species
  paths <- final_adult_migration[, rbindlist(paths), by = .(species, number)] %>%
    .[, num_paths := num_paths * number] %>%
    .[, .(num_paths = sum(num_paths)), by = .(species, distance, lat_dist)] %>%
    as_tibble()
  # combine all energy costs into one list per species
  energy_costs <- final_adult_migration[, .(energy_cost = list(unlist(energy_cost))), by = .(species)] %>%
    as_tibble()
  adult_migration <- list(paths = paths, energy_costs = energy_costs)
}

# Remove the large daily file and collect trash
rm(daily_data_sets)
gc()

# Get the sammries of the dailyu data
data_summary = fish_combos %>% 
  pmap(.f=~make_data_summary(...,
                             habitat = habitat_variable))

# Collect the garbage
gc()

##### Convert the generated data #####
# Project the map data onto a shape file
map_data <- data_summary %>%
  map(~ left_join(.x$average_map, grid_file, by = c("distance", "lat_dist"))) %>% 
  map(~ st_as_sf(.x)) %>% 
  setNames(map(data_summary, ~paste0(.x$species, "-", .x$lifestage)))

# Get a mean habitat map
mean_map <- map_data[[1]] %>%  
  mutate(below_d_cutoff = ifelse(depth < habitat_parm$dep_cutoff, 1, 0) ,
         below_v_cutoff = ifelse(velocity < habitat_parm$vel_cutoff, 1, 0)) 

# make the fish maps into a df for export
fish_maps_df <- map_data %>% 
  reduce(~bind_rows(.x, .y))

# add flow data to daily averages
daily_data = data_summary %>%
  map(~ left_join(.x$average_day, select(daily_file, date, day, flow_cms),
                  by = "date"))%>% 
  setNames(map(data_summary, ~paste0(.x$species, "-", .x$lifestage)))

# Format adult migration data 
if (adult_run == 1){
  adult_migration_energy_data = adult_migration$energy_costs %>% 
    unnest(energy_cost)
  adult_migration_map_data = adult_migration$paths %>% 
    group_by(species) %>% 
    mutate(num_paths = num_paths/max(num_paths)) %>% 
    ungroup() %>% 
    left_join(grid_file, by = c("distance", "lat_dist")) %>% 
    st_as_sf() %>% 
    split(f = .$species)
}

##### Summary Stats #####
# A set of summary stats for the habitat
summary_stats <- data.frame(cover_area_m2 = sum(mean_map$cover_fra *
                                                  mean_map$wetted_area)) %>%
  mutate(
    near_shore_cover_area_m2 = sum(mean_map$wetted_area *
                                     (mean_map$area > mean_map$wetted_area) *
                                     mean_map$cover_fra),
    near_shore_cover_area_below_v_m2 = sum(mean_map$wetted_area *
                                             (mean_map$velocity < habitat_parm$vel_cutoff) *
                                             (mean_map$area > mean_map$wetted_area) *
                                             mean_map$cover_fra),
    percent_area_below_v_cutoff = sum((mean_map$velocity < habitat_parm$vel_cutoff) *
                                        mean_map$wetted_area) /
      sum(mean_map$wetted_area) * 100,
    percent_area_below_d_cutoff = sum((mean_map$depth < habitat_parm$dep_cutoff) *
                                        mean_map$wetted_area) /
      sum(mean_map$wetted_area) * 100,
    percent_near_shore_area = sum(mean_map$wetted_area *
                                    (mean_map$area > mean_map$wetted_area))/
      sum(mean_map$wetted_area) * 100,
    CBC_percent = sum((mean_map$cover_fra) *
      mean_map$wetted_area *
        (mean_map$depth < habitat_parm$dep_cutoff) *
        (mean_map$velocity < habitat_parm$vel_cutoff)) /
      sum(mean_map$wetted_area) * 100,
    average_cover_percent = sum((mean_map$cover_fra) *
                                  mean_map$wetted_area) /
      sum(mean_map$wetted_area) * 100) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Value") %>% 
  mutate(Item = str_replace_all(Item, c("-" = " ", "_" = " ")),
         Item = str_replace_all(Item, "m2", "(m\u00B2)"),
         Value = round(Value,2))

# A set of summary stats for the fish
fish_summary_stats <- map_data %>%
  map_df(~ data.frame(met_j_per_day =
                        round(mean(.x$fish_met_j_per_day, na.rm = TRUE),2))) %>% 
  mutate(species = word(names(map_data),1,1, sep = fixed("-")),
         lifestage = word(names(map_data),2,2, sep = fixed("-"))) %>% 
  relocate(species, lifestage)

# and for the migrating adults
if (adult_run == 1){
  migration_summary_stats = adult_migration_energy_data %>% 
    group_by(species) %>% 
    summarise(migration_energy_cost = mean(energy_cost, na.rm = TRUE)) %>% 
    ungroup()
} else{
  migration_summary_stats = data.frame(migration_energy_cost = 0, Species = "None")
}

##### Plots #####
##### Line Plots #####
# 1D wetted plot area
cover_scatter_plot <- make_scatter_plot(
  data_frame = daily_data[[1]],
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
  fill = cover_fra,
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
  fill = (cover_fra),
  scale_name = "Cover\nFraction"
)
display_plot(cutoff_map)

if (juvenile_run == TRUE){
  # predator maps
  pred_map <- make_map(
    data_frame = mean_map,
    fill = hab_rating,
    scale_name = "Predator\nHabitat Rating"
  )
  display_plot(pred_map)
  
  # Predation map
  mort_map <- map_data[grep("juvenile", names(map_data))] %>%
    map2(.x = ., .y = names(.), .f = ~make_map(
      data_frame = .x,
      fill = pred_mort_risk,
      scale_name = "Mortality\nRisk",
      title = str_replace_all(.y, c("-" = " ", "_" = " ")))) %T>% 
    assign(x = "predation_map_length", value = length(.), envir = .GlobalEnv) %>% 
    wrap_plots(ncol = 1, widths = plot_widths,
               heights = predation_map_length * plot_widths)
  display_plot(mort_map, 15, 15)
}

# cover facte map
d_cutoff_label <- c("1" = "Below D", "0" = "Above D")
v_cutoff_label <- c("1" = "Below V", "0" = "Above V")
cover_facet_map  <- make_map(
  data_frame = mean_map,
  fill = (cover_fra),
  scale_name = "Cover"
) + facet_grid(below_d_cutoff ~ below_v_cutoff,
               labeller = labeller(below_d_cutoff = d_cutoff_label,
                                   below_v_cutoff = v_cutoff_label))
display_plot(cover_facet_map, 10, 20)

# adult migration map
if (adult_run == 1){
  migration_map <- adult_migration_map_data %>% 
    map(~make_map(
      data_frame = .x,
      fill = num_paths,
      scale_name = "Frequency of Use",
      title = str_replace_all(paste0(.x$species[1]),
                              c("-" = " ", "_" = " ")))) %T>% 
    assign(x = "migration_map_length", value = length(.), envir = .GlobalEnv)%>% 
    wrap_plots(ncol = 1, widths = plot_widths,
               heights = migration_map_length * plot_widths)
  display_plot(migration_map, 15, 15)
}


# Metabolic map
metabolic_map <- map_data %>% 
  map2(.x = ., .y = names(map_data), .f = ~make_map(
    data_frame = .x,
    fill = fish_met_j_per_day,
    scale_name = "Metabolic Rate\n(j/day)",
    title = str_replace_all(.y, c("-" = " ", "_" = " ")))) %T>% 
  assign(x = "metabolic_map_length", value = length(.), envir = .GlobalEnv)%>% 
  wrap_plots(ncol = 1, widths = plot_widths,
             heights = metabolic_map_length * plot_widths)
display_plot(metabolic_map, 15, 15)

# Net energy map
net_energy_map <-  map_data %>%
  map2(.x = ., .y = names(map_data), .f = ~make_map(
    data_frame = .x %>% mutate(net_energy = ifelse(net_energy>=0, net_energy, 0)),
    fill = net_energy,
    scale_name = "Net Positive\nEnergy Areas\n(j/day)",
    title = str_replace_all(.y, c("-" = " ", "_" = " ")))) %T>% 
  assign(x = "net_energy_map_length", value = length(.), envir = .GlobalEnv)%>% 
  wrap_plots(ncol = 1, widths = plot_widths,
             heights = net_energy_map_length * plot_widths)
display_plot(net_energy_map, 15, 15)



##### Histograms and bar plots #####
# cover facet histogram
cover_facet_hist  <- make_hist(
  data_frame = mean_map,
  bins = cover_fra,
  x_label = "Percent Cover",
  weights = wetted_area) +
  facet_grid(below_d_cutoff ~ below_v_cutoff,
             labeller = labeller(below_d_cutoff = d_cutoff_label,
                                 below_v_cutoff = v_cutoff_label))
display_plot(cover_facet_hist, 10, 20)

if (adult_run == 1){
  migration_energy_hist <- ggplot(adult_migration_energy_data) +
    theme_classic(base_size = 20) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_density(aes(x = energy_cost/1000,
                     y=after_stat(scaled)),
                 fill = cbPalette[1]) +
    labs(x = "Energy Cost (kJ)") +
    facet_wrap(species~., scales = "free", ncol = 1)
  display_plot(migration_energy_hist)
} 


##### Save Data #####
# List of the simple data frames to write
df_outputs = list(mean_map,
                  fish_maps_df,
                  daily_data[[1]])

# Names of the simple data frames
df_outputs_names = list("habitat_map_data.csv",
                        "fish_map_data.csv",
                        "habitat_daily_data.csv")

if (adult_run == TRUE){
  df_outputs = append(df_outputs, list(adult_migration_energy_data))
  df_outputs_names = append(df_outputs_names, list("adult_migration_energy_data.csv"))
}

# Save all the basic outputs 
walk2(df_outputs,
      df_outputs_names,
      ~write.csv(x = .x,
                 file = here(output_folder,
                             paste0(.y, ".csv")),
                 row.names = FALSE))

# Write all the fish daily data sets
walk2(.x = daily_data, .y = names(daily_data),
      .f = ~write.csv(x = .x, file = here(output_folder, paste0(.y,".csv")),
                      row.names = FALSE))

if (adult_run == TRUE){
  # Write the adult migration map to csv
  walk(adult_migration_map_data, ~write.csv(x = .x,
                                   file = here(output_folder,
                                               paste0("adult_migration_path_",
                                                      .x$species[1],
                                                      ".csv")),
                                   row.names = FALSE))
}

##### Save GIS Maps #####
# Write the habitat map
write_sf(mean_map, here(output_shape_folder, "habitat_map.shp"),
         driver ="ESRI Shapefile")

# Write the mean fish maps
walk2(.x = map_data, .y = names(map_data),
      ~write_sf(obj = .x, 
                dsn = here(output_shape_folder, paste0("fish_map_", .y, ".shp"))))
if (adult_run == TRUE){
  # Write the adult migration maps
  walk(adult_migration_map_data,
       ~write_sf(obj = .x,
                 dsn = here(output_shape_folder,
                            paste0("adult_migration_path_",
                                   .x$species[1],
                                   ".shp"))))
}

##### Save Plots #####
# List of objects and names for the save files 
plot_list = list(cover_scatter_plot,
                 cover_map,
                 cover_facet_map,
                 cover_facet_hist,
                 cutoff_map,
                 heat_map_plot,
                 metabolic_map,
                 net_energy_map)

data_list = list(summary_stats,
                 fish_summary_stats)

plot_name_list = list("cover_scatter_plot",
                      "cover_map",
                      "cover_facet_map",
                      "cover_facet_hist",
                      "cutoff_map",
                      "depth_velocity_heatmap",
                      "metabolic_map",
                      "net_energy_map")

data_name_list = list("summary_stats_table",
                      "summary_fish_stats_table")

plot_dimeshions = list(1.2,1.2,1.2,1.2,1.2,1.2,
                       metabolic_map_length+1,
                       net_energy_map_length+1)

if (juvenile_run == TRUE){
  plot_list = append(plot_list, list(pred_map,mort_map))
  plot_name_list = append(plot_name_list, list("predator_map", "predation_map"))
  plot_dimeshions = append(plot_dimeshions, list(1.2, predation_map_length+1))
}

if (adult_run == TRUE){
  plot_list = append(plot_list, list(migration_map, migration_energy_hist))
  plot_name_list = append(plot_name_list, list("migration_map",
                                               "migration_energy_hist"))
  plot_dimeshions = append(plot_dimeshions, list((migration_map_length+1)/2,
                                                 (migration_map_length+1)/2))
  data_list = append(data_list, list(migration_summary_stats))
  data_name_list = append(data_name_list, list("summary_migration_table"))
}

object_list = c(data_list, plot_list)
object_name_list = c(data_name_list, plot_name_list)

# Save all the outputs 
walk2(object_list, object_name_list, ~saveRDS(
  object = .x,
  file = here("temporary", paste0(.y, ".rds"))))

# Save for output
pwalk(list(plot_list, plot_name_list, plot_dimeshions), ~ggsave(
  height = ..3*5,
  plot = ..1,
  filename = here(output_folder, paste0(..2, ".png")),
  limitsize = FALSE,
  device = "png"))

# Save for future processing
pwalk(list(plot_list, plot_name_list, plot_dimeshions), ~ggsave(
  height = ..3*5,
  plot = ..1,
  filename = here("temporary", paste0(..2, ".png")),
  limitsize = FALSE,
  device = "png"))
