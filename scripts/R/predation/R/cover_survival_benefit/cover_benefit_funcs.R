

# transform cumulative fraction data to unitless y values -----------------

get_unitless_y <- function(df, y_val) {
  df %>%
    dplyr::mutate(
      fraction = {{ y_val }} - dplyr::lag({{ y_val }}), # get proportion for each group
      fraction = ifelse(is.na(fraction), {{ y_val }}, fraction),
      unitless_y = fraction / max(fraction, na.rm = T) # set y var to unitless scale
    )
}

# little helper function to remove unnecessary data from the df -----------

clean_up_df <- function(df) {
  df %>%
    tidyr::unnest(-c(data, fit)) %>%
    dplyr::select(-c(data, fit))
}

# fit a model to synthetic data points ------------------------------------

fit_synth_data <- function(df, synth_data, func, output_col, ...) {
  output_col <- enquo(output_col)
  df %>%
    dplyr::mutate(!!output_col := purrr::map2( # predict the cover bonus for the simulation dis_to_cover data
      .x = fit,
      .y = {{ synth_data }},
      .f = {{ func }},
      ...
    ))
}
