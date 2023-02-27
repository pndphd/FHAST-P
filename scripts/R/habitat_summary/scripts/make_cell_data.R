# This function makes dataframe with info on each cell at each flow
make_cell_data <- function(){
  habitat_temp <- shape_file %>%
    #take out columns that are duplicated
    select(-area) %>%
    left_join(raster_file, by = c("distance", "lat_dist"))

  # Make the data just one depth, vel and wetted area per flow per cell
  input_variables <- c("mean.D", "mean.V", "wetd.")
  output_variables <- c("depth", "velocity", "wetted_fraction")
  spread_data <- future_map2(
    input_variables, output_variables,
    ~ spread_flows(habitat_temp, .x, .y)) %>%
    reduce(left_join, by = c("lat_dist", "distance", "flow"))
  
  #construct a data fream with all possibel flows over the model area
  habitat <- habitat_temp %>%
    select(-starts_with("mean.D"),
           -starts_with("mean.V"),
           -starts_with("wetd.D")) %>%
    right_join(spread_data, by = c("lat_dist", "distance")) %>% 
    # get only the AOI parts
    filter(aoi == 1)
  
  # remove some unused things
  rm(spread_data, habitat_temp)
  
  return(habitat)
}

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
    select(-temp_flow) %>% 
    distinct()
}