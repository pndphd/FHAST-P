##### Description ######################
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.
########################################

##### Load the functions #####
source(here("scripts", "daily_inputs", "functions_make_enviro_input_file.R"))

##### Main Work #####
# Convert the input file into the desired CSV output
daily_input_data <- load_daily_conditions(daily_inputs)

# calculate photo period use grid top marker as location
daily_w_photo_period = calc_photo_period(grid_top_marker, daily_input_data)

##### Make the Files #####
# make one file to be used in the rest of the process
write.csv(x = daily_w_photo_period,
          file = here(temp_folder, "daily_input_file.csv"),
          row.names = FALSE)

# make one file to for output
write.csv(x = daily_w_photo_period,
          file = here(output_folder, "daily_conditions_processed.csv"),
          row.names = FALSE)

##### Plots #####
# Make the time series plot
time_series_plot <- make_daily_timeseries_plot(daily_w_photo_period)
# Print in outside window
display_plot(time_series_plot, 12, 6)

# Make the histogram plot
hist_plot <- make_daily_histogram_plot(daily_w_photo_period)
# Print in outside window using patchwork
display_plot(hist_plot, 10, 10)
