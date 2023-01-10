##### Description #####
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.

##### Load the functions #####
source(here("scripts", "R", "daily_inputs", "scripts","functions_make_enviro_input_file.R"))

output_data <- load_daily_conditions(daily_inputs)

##### Make the Files #####
daily_input_file = output_data %>% 
  mutate(month = month(as_date(date, format = "%m/%d/%Y")))

write.csv(x = daily_input_file,
          file = here(temp_folder,"R","daily_input_file.csv"),
          row.names = FALSE)

##### Plots #####
# Make the time series plot
time_series_plot <- make_daily_timeseries_plot(output_data)

# Print in outside window
display_plot(time_series_plot, 12, 6)

# Make the histogram plot
hist_plot <- make_daily_histogram_plot(output_data)

# Print in outside window using patchwork
display_plot(hist_plot, 10, 10)
