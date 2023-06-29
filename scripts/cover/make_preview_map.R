##### Description #####
# This just loads the spatial input data and makes a map to preview

##### Load the functions #####
source(here("scripts","cover","make_preview_map_functions.R"))

map_plot <- make_map_plot()

display_plot(map_plot, 10, 10)

