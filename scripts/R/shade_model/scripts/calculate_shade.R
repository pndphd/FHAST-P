##### Description #####
# This program takes in a ahsde shape file and calculates shade values
# for mid day, half way between sun rise and mid day, and half way
# between mid-day and sun set
# any where shaded in these times is considered shaded

# the units of tolerance to simplify shape
# units are whatever the crs of the shape file is
simplfly_tolarence = 5

library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load Libraries and some base parameters
source(here("scripts","R","shade_model","scripts","shade_functions.R"))

##### Load Files #####
# load the daily files
daily_file <- read.csv(file = here(temp_folder, "R", "daily_input_file.csv"))

# load the res files
res_file <- read.csv(file = grid_res_path,
                     sep = "=",
                     row.names = 1,
                     header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

# Load the canopy cover zone file 
# simplify it to speed up
shade_shape = st_read(canopy_cover_file, quiet = TRUE) %>% 
  st_simplify(dTolerance = simplfly_tolarence) %>% 
  # Filter out empty ones
  filter(!st_is_empty(.)) %>% 
  group_by(height) %>%
  summarize() %>% 
  ungroup()

# Make a list on months but in time format
# also add in an arbitary year and time
times_list = as.list(paste0("2010-", seq(1,12,1), "-15 12:00:00"))

# calculate photo period
daily_w_photo_period = calc_photo_period(shade_shape, daily_file)

# Run the function and combine all the shade layers by month
result = future_map(times_list, ~make_shade_shape(shade_shape, .x)) %>% 
  future_map(~summarise(.x, shade = sum(shade)/sum(shade), do_union = TRUE)) %>% 
  future_map2(seq(1,12,1), ~rename(.x, !!paste0("shade_", .y) := shade))

# get the location of the center for plotting
# location = shade_shape %>% 
#   summarize(geometry = st_union(geometry)) %>% 
#   st_centroid()

# Print a plot
# result_plot = bind_rows(result) %>% 
#   pivot_longer(cols = starts_with("shade")) %>% 
#   na.omit()
# plot_1=ggplot(result_plot) +
#   theme_classic() +
#   geom_sf() +
#   geom_sf(data = location)+
#   facet_wrap(name~., nrow = 3)
# X11(xpos = 2000, width = 20, height = 10)
# print(plot_1)

# save the files
saveRDS(result, file = here(temp_folder, "R", "shade_file.rds"))

write.csv(x = daily_w_photo_period,
          file = here(temp_folder,"NetLogo","daily_input_file.csv"),
          row.names = FALSE)


