# function to do the fish species calculations #
# the over all function
calculate_fish_summary <- function(df, species_id, parameter_list, pct_cover_model, gape_params, turbidity, script_params) {

  # Get the example fish
  adult_length <- parameter_list$eg_adult_length[species_id]
  juv_length <- parameter_list$eg_juvenile_length[species_id]
  
  # make two lists of life stages
  length_list <- list(adult_length, juv_length)
  lifestage_list <- list("adult", "juvenile")
  
  # get some predator parameters
  a <- gape_params %>% pull("a")
  B <- gape_params %>% pull("B")
 
  # Do some predator calculations
  min_prey_size <- calc_prey_length(a = a, B = B, pred_length = 150)
  
  turb_bonus <- calc_turbidity_bonus(turbidity,
                                     habitat_parm$turbidity_int,
                                     habitat_parm$turbidity_slope)

  df_out <- lifestage_list %>%
    map2_dfr(length_list, ~ calc_one_lifestage(df, species_id, parameter_list, .x, .y)) %>%
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
calc_one_lifestage <- function(df, id, pl, ls, length) {

  wall_factor = ifelse(pl$fish_benthic[id] == 0,
                       1,
                       # all the following hard coded constants from the relation sshif for the law of the wall
                       0.07/0.41*log((habitat_parm$ben_vel_height/habitat_parm$d84_size)*(30/3.5)))
  
  # Calculate the fish mass
  fish_mass = pl$length_to_mass_a[id] *
    length^pl$length_to_mass_b[id]

  # Check if they eat
  feeding_flag = ifelse(ls == "adult" & pl$adult_feeding[id] == 0,
                        0, 1)

  # Get the reach temp
  reach_temp = df$temp[1]
  
  # Get reach turbidity
  reach_tur = df$turb[1]
  
  # Calculate the cmax temperature function value
  cmax_temp_value = (1 + (pl$fish_cmax_C[id] - reach_temp)/(pl$fish_cmax_C[id] - pl$fish_cmax_D[id]))*
    (reach_temp/pl$fish_cmax_C[id])^(pl$fish_cmax_C[id] / (pl$fish_cmax_C[id] - pl$fish_cmax_D[id]))
  
  # Calculate cmax
  cmax = pl$fish_cmax_A[id] * fish_mass^(1 + pl$fish_cmax_B[id]) *  cmax_temp_value
  
  # Calculate the turbidity function
  turbidity_fun = ifelse(reach_tur <= pl$fish_turbid_threshold[id], 1,
                         pl$fish_turbid_min[id] + (1 - pl$fish_turbid_min[id]) *
                           exp(pl$fish_turbid_exp[id] *
                                 (reach_tur - pl$fish_turbid_threshold[id])))
  # Calculate detection distance
  detection_dist = (pl$fish_react_dist_A[id] +
                      pl$fish_react_dist_B[id] * length) * turbidity_fun
  
  # Calculate max swim speed
  max_swim_speed = pl$fish_max_swim_param_A[id] * length +
    pl$fish_max_swim_param_B[id] *
    pl$fish_max_swim_param_C[id] * reach_temp^2 +
    pl$fish_max_swim_param_D[id] * reach_temp +
    pl$fish_max_swim_param_E[id]

  df_out <- df %>%
    # Do the metabolic calculations
    mutate(
      # reduce velocity for benthic fish and in coiver fish if habitat is avaiabel
      shelter_fraction = ifelse(length^2/1e4 <= wetted_area*pct_cover &
                                  wall_factor == 1,
                                habitat_parm$shelter_frac,
                                1),
      experienced_vel = wall_factor*velocity*shelter_fraction,
      species = pl$specie[id],
      life_stage = ls,
      fish_length = length,
      fish_met_log = pl$met_int[id] +
        pl$met_lm[id] * log(fish_mass) +
        pl$met_lt[id] * log(temp) +
        # experienced_vel is in body lengths/sec
        pl$met_v[id] * experienced_vel / (fish_length/100) +
        pl$met_lm_lt[id] * log(fish_mass) * log(temp) +
        pl$met_lm_v[id] * log(fish_mass) * experienced_vel / (fish_length/100) +
        pl$met_sqv[id] * sqrt(experienced_vel / (fish_length/100)) +
        pl$met_lm_sqv[id] * log(fish_mass) * sqrt(experienced_vel / (fish_length/100)) +
        pl$met_t[id] * temp +
        pl$met_lm_t[id] * log(fish_mass) * temp,
      fish_met_j_per_day = exp(fish_met_log), 
      # Do the food intake calculations
      capture_area = 2 * detection_dist * pmin(depth, detection_dist),
      capture_success = calc_logistic(parm_10 = pl$fish_capture_1[id],
                                      parm_90 = pl$fish_capture_9[id],
                                      value = velocity/max_swim_speed),
      drift_eaten = capture_success *capture_area * habitat_parm$hab_drift_con * velocity *86400,
      ben_eaten = fish_mass * pl$fish_fr_A[id] * habitat_parm$hab_bentic_ene /
        (1 + pl$fish_fr_B[id] * habitat_parm$hab_bentic_con),
      ben_avaiable = habitat_parm$hab_bentic_con *
        ben_food_fra * wetted_area,
      ben_intake = pmin(ben_eaten, ben_avaiable, cmax),
      drift_intake = pmin(drift_eaten, cmax),
      intake_ben_energy = ben_intake * habitat_parm$hab_bentic_ene,
      intake_drift_energy = drift_intake * habitat_parm$hab_drift_ene,
      energy_intake = intake_ben_energy *pl$fish_benthic[id] + 
        intake_drift_energy * (1 - pl$fish_benthic[id]),
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
