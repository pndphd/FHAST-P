##### Description #####
# This contains the functions used in make_habitat_summary

##### spread_flows #####
# this function spreads out the flows so they are just one depth and v per flow
spread_flows <- function(habitat_data, input_variable, output_variable) {
  spread_df <- habitat_data %>%
    # spread out the flows
    select(
      starts_with(input_variable),
      distance,
      lat_dist
    ) %>%
    pivot_longer(
      cols = starts_with(input_variable),
      names_to = "temp_flow",
      values_to = output_variable
    ) %>%
    mutate(flow = as.numeric(str_sub(string = temp_flow, start = 7))) %>%
    select(-temp_flow)
}

##### Main Time series/averaging data making functions ######
make_time_series_data <- function(past_list,
                                  current,
                                  habitat_data,
                                  flows_list,
                                  fish_params,
                                  sig_figs,
                                  max_day,
                                  model_params,
                                  script_params,
                                  temp_params,
                                  pred_length_data,
                                  pct_cover_model,
                                  base_pred_success = 0.6) {
  # get the depth, velocity, and shade data for each cell this day
  habitat_d_v_data <- get_daily_v_and_d(current, habitat_data, flows_list, sig_figs)
  # do the predator calculations for each cell
  habitat_pred_data <- calculate_predators(habitat_d_v_data,
                                           model_params,
                                           script_params,
                                           temp_params,
                                           pred_length_data) 
  # do the calculations for each combo of fish lifestage and species
  fish_phys_data <- map(
    fish_params,
    ~ calculate_fish_summary(habitat_pred_data, .x, pct_cover_model, base_pred_success)
  ) %>%
    unlist(recursive = FALSE)
  # get the single day summaries first for the habitat data
  single_pt_summary_habitat <- habitat_pred_data %>%
    mutate(
      total_cover = sum((pct_cover) * area * wetted_fraction),
      total_wetted_area = sum(wetted_area)
    ) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  # get the single day summaries first for the fish data
  single_pt_summary_fish <- fish_phys_data %>%
    map(~ summarise(.x, across(where(is.numeric), mean, na.rm = TRUE)) %>%
          mutate(
            day = current$day,
            # add in species labels
            species = .x$species[1],
            life_stage = .x$life_stage[1]
          ))
  # make a place holder for outputs
  output_list <- list(
    daily_habitat = NA, daily_fish = NA,
    map_habitat = NA, map_fish = NA
  )
  
  if (current$day == 1) {
    output_list$daily_habitat <- single_pt_summary_habitat
    output_list$daily_fish <- single_pt_summary_fish
    output_list$map_habitat <- data.matrix(habitat_pred_data)
    output_list$map_fish <- map(fish_phys_data, ~ data.matrix(.x))
  } else {
    output_list$daily_habitat <- bind_rows(
      past_list$daily_habitat,
      single_pt_summary_habitat
    )
    output_list$daily_fish <- map2(
      past_list$daily_fish, single_pt_summary_fish,
      ~ bind_rows(.x, .y)
    )
    # Average predator length is set to NA's so maybe look into replacing in important
    output_list$map_habitat <- data.matrix(habitat_pred_data) +
      past_list$map_habitat
    output_list$map_fish <- map2(
      past_list$map_fish, fish_phys_data,
      function(x, y) x + data.matrix(y)
    )
  }
  if (current$day == max_day) {
    output_list$map_habitat <- data.frame(output_list$map_habitat / max_day) %>%
      filter(depth > 0) %>%
      left_join(grid_file, by = c("distance", "lat_dist")) %>%
      mutate(below_d_cutoff = ifelse(depth < script_params$depth_cutoff,1,0),
             below_v_cutoff = ifelse(velocity < script_params$velocity_cutoff,1,0),
             near_shore = ifelse(wetted_fraction < 1,1,0)) %>% 
      st_as_sf()
    
    
    output_list$map_fish <- map(output_list$map_fish, ~ data.frame(.x / max_day)) %>%
      map2(fish_phys_data, ~ mutate(.x,
                                    # add species labeles back in
                                    species = .y$species[1],
                                    life_stage = .y$life_stage[1]
      ))
  }
  
  return(output_list)
}

# Function to get daily V and D
get_daily_v_and_d <- function(current, habitat_data, flows_list, sig_figs) {
  # Get the month for later use in shade
  this_month <- month(current$date)
  high_flow <- min(subset(flows_list, flows_list > current$flow_cms))
  low_flow <- max(subset(flows_list, flows_list <= current$flow_cms))
  flow_fraction <- (current$flow_cms - low_flow) / (high_flow - low_flow)
  # Get the low flow match first
  filter_low_flow <- habitat_data %>%
    filter(flow == low_flow) %>%
    mutate(
      fraction_low_depth = depth * (1 - flow_fraction),
      fraction_low_velocity = velocity * (1 - flow_fraction),
      fraction_low_wetted = wetted_fraction * (1 - flow_fraction)
    )
  
  mid_flow <- habitat_data %>%
    filter(flow == high_flow) %>%
    mutate(
      fraction_high_depth = depth * (flow_fraction),
      fraction_high_velocity = velocity * (flow_fraction),
      fraction_high_wetted = wetted_fraction * (flow_fraction)
    ) %>%
    select(
      fraction_high_depth, fraction_high_velocity, distance,
      lat_dist, fraction_high_wetted
    ) %>%
    left_join(filter_low_flow, by = c("distance", "lat_dist")) %>%
    mutate(
      depth = round(fraction_low_depth + fraction_high_depth, sig_figs),
      velocity = round(fraction_low_velocity + fraction_high_velocity, sig_figs),
      wetted_fraction = round(fraction_low_wetted + fraction_high_wetted, sig_figs)
    ) %>%
    mutate(
      date = current$date,
      day = current$day,
      this_month = month(date),
      temp = current$temp_c,
      flow_cms = current$flow_cms
    ) %>%
    mutate(across(
      .cols = ends_with(paste0("shade_", this_month)),
      ~.x,
      .names = "shade"
    )) %>%
    select(
      -fraction_low_velocity,
      -fraction_high_velocity,
      -fraction_low_depth,
      -fraction_high_depth,
      -fraction_low_wetted,
      -fraction_high_wetted,
      -this_month,
      -starts_with("shade_")
    ) %>% 
    mutate(wetted_area = area*wetted_fraction,
           pct_cover = wood+veg)

  return(mid_flow)
}

# Function to do some predation calculations
calculate_predators <- function(df, model_params, script_params, temp_params, pred_length_data) {
  species_list <- get_list_of_species(model_params)
  preds <-
    df %>%
    # get all cells with water
    #dplyr::filter(wetted_fraction > 0) %>%
    dplyr::mutate(
      # combine rocky substrate into one variable
      substrate = rowSums(across(gravel:rock)),
      # if rocky substrate is the majority in a cell, then 1, else 0
      substrate = dplyr::if_else(substrate >= fine & substrate > 0, 1, 0)
    ) %>%
    # rename original shade column so a new one can be made with 1/0 values
    dplyr::rename(shade_orig = shade) %>%
    dplyr::mutate(shade = dplyr::if_else(shade_orig >= 0.5, 1, 0)) %>%
    # add model predictions and predator counts
    calc_all_pred_data(species_list, model_params, script_params) %>%
    # assign lengths to preds and keep only those at least 150 mm
    adjust_preds_for_length(species_list, pred_length_data) %>%
    # calc relative area of cell occupied by predators
    add_pred_areas(species_list, script_params, model_params, temp_params) %>%
    # add columns with totals
    add_pred_totals(species_list, model_params) %>%
    # drop uneeded temp columns
    dplyr::select(-c(substrate, shade)) %>%
    # reset the actual shade value column to its original
    dplyr::rename(shade = shade_orig) 
  
  # check if both predator columns exist; if not, create a col of 0s
  preds[species_list[!(species_list %in% colnames(preds))]] <- 0
  
  # df %>%
  #   dplyr::left_join(preds) %>%
  #   suppressMessages()
  return(preds)
}

# function to calculate probability of surviving an encounter with a predator

survival_prob <- function(cover_benefit, base_pred_success){
  (1 - base_pred_success) + (base_pred_success * cover_benefit)
}

# function to do the fish species calculations #
# the over all function
calculate_fish_summary <- function(df, parameter_list, pct_cover_model, base_pred_success) {
  #browser()
  adult_length <- grab(parameter_list, "eg_adult_length")
  juv_length <- grab(parameter_list, "eg_juvenile_length")
  length_list <- list(
    adult_length,
    juv_length
  )
  lifestage_list <- list("adult", "juvenile")
  min_prey_size <- calc_prey_length(pred_length = 150)
  
  df_out <- lifestage_list %>%
    map2_dfr(length_list, ~ calc_one_lifestage(df, parameter_list, .x, .y)) %>%
    mutate(pred_length = replace_na(pred_length, 0),
           prey_length = calc_prey_length(pred_length),
           total_pred_prop_area = if_else(
             prey_length >= fish_length * 10, # convert to cm
             total_pred_prop_area,
             0), 
           dis_to_cover_m = sqrt(area) * predict(pct_cover_model, newdata = data.frame(pct_cover)),
           dis_to_cover_m = if_else(dis_to_cover_m < 0, 0, dis_to_cover_m),
           survival_bonus = predict(dis_to_cover_model, newdata = data.frame(dis_to_cover_m), type = "response"),
           survival_bonus = if_else(pct_cover > 0, survival_bonus, 0),
           survival_prob = survival_prob(survival_bonus, base_pred_success),
           pred_mort_risk = if_else(
             total_pred_prop_area > 1,
             1 - (survival_prob ^ total_pred_prop_area),
             1 - (total_pred_prop_area * survival_prob + (1 - total_pred_prop_area))
           )
           ) %>% 
    select(
      species, life_stage, fish_length, fish_mass, fish_met_j_per_day,
      distance, lat_dist, total_pred_prop_area, pred_mort_risk
    )
  output_list <- lifestage_list %>%
    map(~ filter(df_out, life_stage == .x))
  
  return(output_list)
}

# function to do calculations for each lifestage
calc_one_lifestage <- function(df, pl, ls, length) {
  df_out <- df %>%
    mutate(
      species = pl["species", ],
      life_stage = ls,
      fish_length = length,
      fish_mass = grab(pl, "length_to_mass_a") *
        fish_length^grab(pl, "length_to_mass_b"),
      fish_met_log = grab(pl, "met_int") +
        grab(pl, "met_lm") * log(fish_mass) +
        grab(pl, "met_lt") * log(temp) +
        # velocity is in body lengths/sec
        grab(pl, "met_v") * velocity / (fish_length / 100) +
        grab(pl, "met_lm_lt") * log(fish_mass) * log(temp) +
        grab(pl, "met_lm_v") * log(fish_mass) * velocity / (fish_length / 100) +
        grab(pl, "met_sqv") * sqrt(velocity / (fish_length / 100)) +
        grab(pl, "met_lm_sqv") * log(fish_mass) * sqrt(velocity / (fish_length / 100)) +
        grab(pl, "met_t") * temp +
        grab(pl, "met_lm_t") * log(fish_mass) * temp,
      fish_met_j_per_day = exp(fish_met_log)
    )
}

# function to get values form list
grab <- function(df, name) {
  value <- as.numeric(df[name, ])
}

##### Daily average functions #####
multiply_and_sum <- function(data_frame, col_1, col_2, new_col) {
  output <- data_frame %>%
    mutate(temp_col = {{ col_1 }} * {{ col_2 }}) %>%
    group_by(date, day) %>%
    summarize({{ new_col }} := sum(temp_col, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(day)
  
  return(output)
}

sum_daily_data <- function(data_frame, col_1, new_col) {
  output <- data_frame %>%
    mutate(temp_col = {{ col_1 }}) %>%
    group_by(date, day) %>%
    summarize({{ new_col }} := sum(temp_col, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(day)
  
  return(output)
}

average_daily_data <- function(data_frame, col_1, new_col) {
  output <- data_frame %>%
    mutate(temp_col = {{ col_1 }}) %>%
    group_by(date, day) %>%
    summarize({{ new_col }} := mean(temp_col, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(day)
  
  return(output)
}


##### Plotting Functions #####
# Line plot
make_line_plot <- function(data_frame, x_axis, y_axis, x_lab, y_lab) {
  output_plot <- ggplot(data_frame, aes(x = {{ x_axis }}, y = {{ y_axis }})) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    geom_path(
      color = cbPalette[1],
      size = 1,
      linetype = "solid"
    ) +
    scale_y_continuous(y_lab) +
    labs(x = x_lab)
  
  return(output_plot)
}

make_scatter_plot <- function(data_frame, x_axis, y_axis, x_lab, y_lab) {
  output_plot <- ggplot(data_frame, aes(x = {{ x_axis }}, y = {{ y_axis }})) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    geom_point(
      color = cbPalette[1],
      size = 1
    ) +
    scale_y_continuous(y_lab) +
    labs(x = x_lab)
  
  return(output_plot)
}

# Function to make a map
# Depth map
make_map <- function(data_frame = NULL,
                     fill = NULL,
                     scale_name = NULL,
                     title = NULL) {
  output_map <- ggplot(data = data_frame) +
    theme_classic(base_size = 12) +
    geom_sf(aes(fill = {{ fill }}, color = {{ fill }})) +
    scale_fill_viridis(name = scale_name) +

    scale_color_viridis(name = scale_name, guide = "none")+
    ggtitle(title)

  return(output_map)
}



# # Function to process an make the heat map
# # the z value is the sum

make_heat_map = function(data_frame,
                         x_axis, y_axis, z_axis,
                         x_lab, y_lab, z_lab,
                         resolution){
# browser()
  # Make a list for the x and y axis
  x_axis_select = deparse(substitute(x_axis))
  x_bins_list = seq(min(data_frame[[x_axis_select]]),
                    max(data_frame[[x_axis_select]]),
                    length.out = resolution)
  y_axis_select = deparse(substitute(y_axis))
  y_bins_list = seq(min(data_frame[[y_axis_select]]),
                    max(data_frame[[y_axis_select]]),
                    length.out = resolution)

  # Make a blank data frame and assign bins which will be used in the join
  blank_data_frame = data.frame(expand.grid(x_value = x_bins_list,
                                            y_value = y_bins_list,
                                            base_value = NA)) %>%
    mutate(y_bin = cut(y_value, breaks = seq(min(y_value),
                                             max(y_value),
                                             length.out = resolution),
                       include.lowest = TRUE),
           x_bin = cut(x_value, breaks = seq(min(x_value),
                                             max(x_value),
                                             length.out = resolution),
                       include.lowest = TRUE))

  # Now bin all the data in the data frame
  plot_df = data_frame  %>%
    st_drop_geometry() %>% 
    mutate(x_bin = cut({{x_axis}}, breaks = seq(min({{x_axis}}),
                                                max({{x_axis}}),
                                                length.out = resolution),
                       include.lowest = TRUE),
           y_bin = cut({{y_axis}}, breaks = seq(min({{y_axis}}),
                                                max({{y_axis}}),
                                                length.out = resolution),
                       include.lowest = TRUE)) %>%
    group_by(x_bin, y_bin) %>%
    summarise(total_z = sum({{z_axis}}, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(z_fraction = total_z/sum(total_z)) %>%
    full_join(blank_data_frame, by = c("x_bin", "y_bin")) %>%
    mutate(z_fraction_final = ifelse(is.na(z_fraction), 0 , z_fraction))

  # Make the plot
  heat_map_plot = ggplot(plot_df, aes(x = x_value,
                                      y = y_value,
                                      fill = z_fraction_final)) +
    theme_classic(base_size = 20) +
    geom_raster(interpolate = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis(name = z_lab) +
    labs(x = x_lab, y = y_lab)


}
