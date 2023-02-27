# function to do the fish species calculations #
# the over all function
calculate_fish_summary <- function(df,
                                   species_id,
                                   parameter_list,
                                   pct_cover_model,
                                   gape_params,
                                   current,
                                   script_params) {
  # Get the example fish
  adult_length <- parameter_list$eg_adult_length[species_id]
  juv_length <- parameter_list$eg_juvenile_length[species_id]
  
  # make two lists of life stages
  length_list <- list(adult_length, juv_length)
  lifestage_list <- list("adult", "juvenile")
  
  # get some predator parameters
  a <- gape_params %>% pull("gape_a")
  B <- gape_params %>% pull("gape_b")
 
  # Do some predator calculations
  min_prey_size <- calc_prey_length(a = a, B = B, pred_length = 150)
  
  turb_bonus <- calc_turbidity_bonus(current$turb_ntu,
                                     habitat_parm$turbidity_int,
                                     habitat_parm$turbidity_slope)

  df_out <- lifestage_list %>%
    map2_dfr(length_list, ~ calc_one_lifestage(df, species_id, parameter_list, current, .x, .y)) %>%
    mutate(pred_length = replace_na(pred_length, 0),
           prey_length = calc_prey_length(a = a, B = B, pred_length),
           total_pred_prop_area = if_else(
             prey_length >= fish_length * 10, # convert to cm
             total_pred_prop_area,
             0), 
           dis_to_cover_m = sqrt(area) * predict(pct_cover_model, newdata = data.frame(pct_cover)),
           dis_to_cover_m = if_else(dis_to_cover_m < 0, 0, dis_to_cover_m),
           survival_bonus = predict(dis_to_cover_model, newdata = data.frame(dis_to_cover_m), type = "response"),
           # reset the cover survival bonus to 0 if not cover
           survival_bonus = if_else(pct_cover > 0, survival_bonus, 0),
           survival_prob = map_dbl(survival_bonus, ~ calc_survival_prob(script_params$pred_success, .x, turb_bonus)),
           pred_mort_risk = if_else(
             total_pred_prop_area > 1,
             1 - (survival_prob ^ total_pred_prop_area),
             total_pred_prop_area * (1 - survival_prob))) %>% 
    select(
      species, life_stage, fish_length, fish_met_j_per_day, net_energy, energy_intake,
      distance, lat_dist, total_pred_prop_area, pred_mort_risk
    )
  output_list <- lifestage_list %>%
    map(~ filter(df_out, life_stage == .x))

  return(output_list)
}

# function to do calculations for each lifestage
calc_one_lifestage <- function(df, id, pl, current, ls, length) {

  # If the fish is benthic get it's experienced velocity 
  wall_factor = ifelse(pl$benthic_fish[id] == 0, 1,
                       # all the following hard coded constants from the
                       # relationship for the law of the wall
                       habitat_parm$base_wall_factor)
  
  # Make a flag for the smolt flag in the metabolic equation
  smolt_status = ifelse(ls == "adult", 1, 0)
  
  # Calculate the fish mass
  fish_mass = pl$length_mass_a[id] *
    length^pl$length_mass_b[id]

  # Check if they eat
  feeding_flag = ifelse(ls == "adult" & pl$adult_feeding[id] == 0, 0, 1)

  # Calculate the cmax temperature function value
  cmax_temp_value = calc_beta_sig(parm_A = pl$cmax_c[id],
                                  parm_B = pl$cmax_d[id],
                                  temp = current$temp_c)

  # Calculate cmax
  cmax = pl$cmax_a[id] * fish_mass^(1 + pl$cmax_b[id]) *  cmax_temp_value
  
  # Calculate the turbidity function
  turbidity_fun = ifelse(current$turb_ntu <= pl$turbid_threshold[id], 1,
                         pl$turbid_min[id] + (1 - pl$turbid_min[id]) *
                           exp(pl$turbid_exp[id] *
                                 (current$turb_ntu - pl$turbid_threshold[id])))
  # Calculate detection distance
  detection_dist = (pl$react_dist_a[id] +
                      pl$react_dist_b[id] * length) * turbidity_fun
  
  # Calculate max swim speed temperature function value
  max_swim_speed_temp = calc_beta_sig(parm_A = pl$ucrit_c[id],
                                       parm_B = pl$ucrit_d[id],
                                       temp = current$temp_c)
  
  max_swim_speed = (pl$ucrit_a[id] / length +
    pl$ucrit_b[id]) *
    max_swim_speed_temp*length/100

  df_out <- df %>%
    # Do the metabolic calculations
    mutate(
      is_benthic = pl$benthic_fish[id],
      # reduce velocity for benthic fish and in cover fish if habitat is available
      # benthic fish don't use
      shelter_fraction = ifelse(length^2/1e4 <= wetted_area*pct_cover &
                                  wall_factor == 1,
                                habitat_parm$shelter_frac,
                                1),
      experienced_vel = wall_factor*velocity*shelter_fraction,
      species = pl$specie[id],
      life_stage = ls,
      fish_length = length,
      fish_met_j_per_day_active = calc_met(params = pl,
                                    fish_index = id,
                                    length = length,
                                    temp = temp,
                                    velocity = experienced_vel,
                                    smolt_flag = smolt_status),
      fish_met_j_per_day_passive = calc_met(params = pl,
                                           fish_index = id,
                                           length = length,
                                           temp = temp,
                                           velocity = 0,
                                           smolt_flag = smolt_status),
      # benthic fish are assumed to be active at night 
      fish_met_j_per_day_ur = ifelse(is_benthic == 0,
                                  fish_met_j_per_day_active * (current$photoperiod) +
                                    fish_met_j_per_day_passive * (1 - current$photoperiod),
                                  fish_met_j_per_day_active * (1 - current$photoperiod) +
                                    fish_met_j_per_day_passive * (current$photoperiod)),
      fish_met_j_per_day = ifelse(experienced_vel>max_swim_speed, NA, fish_met_j_per_day_ur),
      # Do the food intake calculations
      capture_area = 2 * detection_dist * pmin(depth, detection_dist),
      capture_success = calc_logistic(parm_10 = pl$capture_V1[id],
                                      parm_90 = pl$capture_V9[id],
                                      value = velocity/max_swim_speed),
      drift_eaten = capture_success *capture_area * habitat_parm$hab_drift_con * velocity *
        86400 * current$photoperiod,
      ben_eaten = pi * pl$feeding_speed[id] * 86400 * (1 - current$photoperiod) /
        log(pl$feeding_speed[id] * (1 - current$photoperiod) * 86400) *
        length^2/1E4 * habitat_parm$hab_bentic_con,
      ben_avaiable = habitat_parm$hab_bentic_con *
        ben_food_fra * wetted_area,
      ben_intake = pmin(ben_eaten, ben_avaiable, cmax),
      drift_intake = pmin(drift_eaten, cmax),
      intake_ben_energy = ben_intake * habitat_parm$hab_bentic_ene,
      intake_drift_energy = drift_intake * habitat_parm$hab_drift_ene,
      energy_intake = intake_ben_energy *is_benthic + 
        intake_drift_energy * (1 - is_benthic),
      net_energy = energy_intake * feeding_flag - fish_met_j_per_day) 
}

# function to calculate probability of surviving an encounter with a predator
calc_survival_prob <- function(pred_success, ...){
  hab_params <- c(...)
  1 - pred_success + (pred_success * (1 - prod(1 - hab_params)))
}

# calculate the turbidity function
calc_turbidity_bonus <- function(turbidity, int, slope){
  1 / (1 + exp(-1 *(int + turbidity * slope)))
}

#TODO integrate into calcs above
calc_pred_mort_risk <- function(survival_prob, encounter_prob, num_preds){
  
  if (encounter_prob > 1) {
    new_enc_num <- (min((encounter_prob), sum(num_preds)))
    guaranteed_num_encounters <- floor(new_enc_num)
    left_over_prob <- new_enc_num - guaranteed_num_encounters
    
    return(1 - (survival_prob ^ guaranteed_num_encounters) * (1 - left_over_prob * (1 - survival_prob)))
  } else {
    return( encounter_prob * (1 - survival_prob))
  }
}

# Functions to calculate each energy type
# First benthic energy
# calc_benthic_energy = function(fish_mass_in,
#                                script_params_in,
#                                pl_in,
#                                ben_food_fra_in,
#                                wetted_area_in,
#                                cmax_in){
#   eaten = fish_mass_in * grab(pl_in, "fish_fr_A") * script_params_in$benthic_density /
#     (1 + grab(pl_in, "fish_fr_B") * script_params_in$benthic_density)
#   avaiable = script_params_in$benthic_density *
#     ben_food_fra_in * wetted_area_in
# 
#   intake = min(eaten, avaiable, cmax_in)
#   intake_energy = intake * script_params_in$benthic_energy
#   return(intake_energy)
# }
# 
# # Second drift intake
# # !!!!!! Need to add capture success to this !!!!!!!!!!
# calc_drift_energy = function(detection_dist_in,
#                              depth_in,
#                              velocity_in,
#                              script_params_in,
#                              cmax_in){
#   capture_area = 2 * detection_dist_in * min(depth_in, detection_dist_in)
#   eaten = capture_area * script_params_in$drift_density * velocity_in *86400
# 
#   intake = min(eaten,  cmax_in)
#   intake_energy = intake * script_params_in$drift_energy
#   return(intake_energy)
# }
