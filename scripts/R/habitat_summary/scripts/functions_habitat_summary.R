##### Description #####
# This contains the functions used in make_habitat_summary 

##### spread_flows #####
spread_flows = function(habitat_data, input_variable, output_variable){
  spread_df <- habitat_data %>%
    # spread out the flows
    select(starts_with(input_variable),
           distance,
           lat_dist) %>% 
    pivot_longer(
      cols = starts_with(input_variable),
      names_to = "temp_flow",
      values_to = output_variable) %>%
    mutate(flow = as.numeric(str_sub(string = temp_flow, start = 7))) %>% 
    select(-temp_flow)
}

##### The main time series function #####
make_time_series_data = function(hydrograph, habitat_data, flows_list, sig_figs){

  hydrograph = hydrograph[1:3,]
  time_series_data = hydrograph %>%
    future_pmap_dfr(function(...) {
      current = tibble(...)
      # Get the month for later use in shade
      this_month = month(current$date)
      high_flow = min(subset(flows_list, flows_list > current$flow_cms))
      low_flow = max(subset(flows_list, flows_list <= current$flow_cms))
      flow_fraction = (current$flow_cms - low_flow)/(high_flow - low_flow)
      # Get the low flow match first
      filter_low_flow = habitat_data %>% 
        filter(flow == low_flow) %>%
        mutate(fraction_low_depth = depth * (1-flow_fraction),
               fraction_low_velocity = velocity * (1-flow_fraction))

      mid_flow = habitat_data %>% 
        filter(flow == high_flow) %>%
        mutate(fraction_high_depth = depth * (flow_fraction),
               fraction_high_velocity = velocity * (flow_fraction)) %>% 
        select(fraction_high_depth, fraction_high_velocity, distance, lat_dist) %>% 
        left_join(filter_low_flow, by = c("distance", "lat_dist")) %>%
        mutate(depth = round(fraction_low_depth + fraction_high_depth, sig_figs),
               velocity = round(fraction_low_velocity + fraction_high_velocity, sig_figs)) %>%
        mutate(date = current$date,
               day = current$day,
               this_month = month(date),
               temp = current$temp_c,
               flow_cms = current$flow_cms) %>% 
        # Select the shade data for the relevant month
        rename_with(~return("shade"),
                    starts_with(paste0("shade_", this_month))) 

      return(mid_flow)
    }) %>% 
    select(-fraction_low_velocity,
           -fraction_high_velocity,
           -fraction_low_depth,
           -fraction_high_depth,
           -this_month,
           -starts_with("shade_"))

  return(time_series_data)
}

##### Daily average functions #####
multiply_and_sum = function(data_frame, col_1, col_2, new_col ){
  output = data_frame %>% 
    mutate(temp_col = {{col_1}} * {{col_2}}) %>% 
    group_by(date, day) %>% 
    summarize({{new_col}} := sum(temp_col, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(day)
  
  return(output)
}

sum_daily_data = function(data_frame, col_1, new_col ){
  output = data_frame %>% 
    mutate(temp_col = {{col_1}}) %>% 
    group_by(date, day) %>% 
    summarize({{new_col}} := sum(temp_col, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(day)
  
  return(output)
}

average_daily_data = function(data_frame, col_1, new_col ){
  output = data_frame %>% 
    mutate(temp_col = {{col_1}}) %>% 
    group_by(date, day) %>% 
    summarize({{new_col}} := mean(temp_col, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(day)
  
  return(output)
}

##### Spatial Average Functions #####
average_and_map = function(data_frame, grid, col_1, new_col){
  mean_calc = data_frame %>% 
    group_by(distance, lat_dist) %>% 
    summarize(velocity = mean(velocity, na.rm = TRUE),
              wetted = mean(wetted_fraction, na.rm = TRUE),
              depth = mean(depth, na.rm = TRUE)) 
  output = grid %>% 
    left_join(mean_calc, by = c("distance", "lat_dist"))

  return(output)
} 

##### Plotting Functions #####
# Line plot
make_line_plot = function(data_frame, x_axis, y_axis, x_lab, y_lab){
  output_plot = ggplot(data_frame, aes(x = {{x_axis}}, y = {{y_axis}})) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_path(color = cbPalette[1],
              size=1,
              linetype = "solid") +
    scale_y_continuous(y_lab) +
    labs(x = x_lab) 
  
  return(output_plot)
}

# Function to process an make the heat map
# the z value is the sum
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