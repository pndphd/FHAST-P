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
    # assign lengths to preds and keep only those at least 15 cm
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