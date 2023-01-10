# Function to get daily V and D
get_daily_v_and_d <- function(current, habitat_data, flows_list, sig_figs) {

  # Get the month for later use in shade
  this_month <- month(current$date)

  # Get the flows just above and below this days flow
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
      turb = current$turb_ntu,
      flow_cms = current$flow_cms
    ) %>%
    # Select out this month's shade
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
      -flow,
      -starts_with("shade_")
    ) %>% 
    mutate(wetted_area = area*wetted_fraction,
           pct_cover = wood+veg)

  return(mid_flow)
}