##### Description #####
# This script takes an input file and reads form it what type of flow and
# temperature input file the program should make. It then makes it.

##### Load the functions
source(here("scripts", "daily_inputs", 
            "functions_make_fish_input_file.R"))

##### Run the main function
fish_schedule <- load_fish_timeseries(fish_daily_inputs)

# Get flags to check what runs to do
juvenile_run = ifelse(NROW(filter(fish_schedule, lifestage == "juvenile"))>0, T ,F)
adult_run = ifelse(NROW(filter(fish_schedule, lifestage == "adult"))>0, T ,F)

##### Save the result #####
write.csv(fish_schedule,
          file = here(temp_folder, "daily_fish_input.csv"),
          row.names = FALSE)

# make one file to for output
write.csv(x = fish_schedule,
          file = here(output_folder, "daily_fish_processed.csv"),
          row.names = FALSE)

##### Make a plot #####
fish_plot_data <- fish_schedule %>%
  mutate(Group = str_c(species, " ", lifestage))

time_series_plot_fish <- plot_fish_timeseries(fish_schedule)

display_plot(time_series_plot_fish)
