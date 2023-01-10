##### Main Time series/averaging data making functions ######
make_time_series_data <- function(past_list,
                                  current,
                                  habitat_data,
                                  flows_list,
                                  fish_params_in,
                                  sig_figs,
                                  max_day,
                                  model_params,
                                  script_params,
                                  temp_params,
                                  gape_params,
                                  pred_length_data,
                                  pct_cover_model) {

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
    seq(1, length(fish_params_in$specie), 1),
    ~ calculate_fish_summary(habitat_pred_data,
                             .x,
                             fish_params_in,
                             pct_cover_model, 
                             gape_params, 
                             turbidity = current$turb_ntu,
                             script_params)) %>%
    unlist(recursive = FALSE)

  # get the single day summaries first for the habitat data
  single_pt_summary_habitat <- habitat_pred_data %>%
    mutate(
      total_cover = sum((pct_cover) * area * wetted_fraction),
      total_wetted_area = sum(wetted_area)) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))

  # get the single day summaries for the fish data
  single_pt_summary_fish <- fish_phys_data %>%
    map(~ summarise(.x, across(where(is.numeric), mean, na.rm = TRUE)) %>%
          mutate(
            day = current$day,
            # add in species labels
            species = .x$species[1],
            life_stage = .x$life_stage[1]))

  # make a place holder for outputs
  output_list <- list(
    daily_habitat = NA, daily_fish = NA,
    map_habitat = NA, map_fish = NA)
  
  # Do the calculations to sum up data
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
      left_join(grid_file, by = c("distance", "lat_dist")) %>%
      filter(depth > 0) %>%
      mutate(below_d_cutoff = ifelse(depth < script_params$dep_cutoff,1,0),
             below_v_cutoff = ifelse(velocity < script_params$vel_cutoff,1,0),
             near_shore = ifelse(wetted_fraction < 1,1,0)) %>% 
      st_as_sf()
    
    
    output_list$map_fish <- map(output_list$map_fish, ~ data.frame(.x / max_day)) %>%
      map2(fish_phys_data, ~ mutate(.x,
                                    # add species labeles back in
                                    species = .y$species[1],
                                    life_stage = .y$life_stage[1]))
  }
  
  return(output_list)
}