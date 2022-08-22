##### Description #####
# This program takes in a ahsde shape file and calculates shade values
# for mid day, half way between sun rise and mid day, and half way
# between mid-day and sun set
# any whare shaded in these times is considered shaded

# the units of tolerance to simplify shape
# units are whatever the crs of the shpe file is
simplfly_tolarence = 5

# Load Libraries and some base parameters
library(here)
source(here("scripts","R","main","load_libraries.R"))

##### Load Files #####
# Load data files
# Read in the main input file file
input_data <- read.csv(file = here(input_folder, input_file),
                       sep = "=",
                       row.names = 1,
                       header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

grid_file_name = here(temp_folder, "R", paste0("river_grid_",
                        input_data["resolution",], "_",
                        input_data["buffer",], ".rds"))

# Load the canopy cover zone file 
# simplify it to speed up
shade_shape = st_read(here(input_folder, 
                              input_data["folder",],
                              "cover",
                              input_data["canopy cover",]), quiet = TRUE) %>% 
  st_simplify(dTolerance = simplfly_tolarence) %>% 
  # Filter out empty ones
  filter(!st_is_empty(.)) %>% 
  group_by(height) %>%
  summarize() %>% 
  ungroup()


# Check to see if the files exists
if (0 < sum(!file.exists(grid_file_name)))
  stop('An input file does not exist.')


# Make a list on months but in time format
# also add in an arbitary year and time
times_list = as.list(paste0("2010-", seq(1,12,1), "-15 12:00:00"))

# Make a function to make the shade file
make_shade_shape = function(shape_file_in,
                            time_in){

  # Get the location for the center of the shape and put it in EPSG 4326
  # to allow the solar calculator to work
  location = shape_file_in %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_centroid() %>% 
    st_transform(st_crs("EPSG:4326"))

  # Get the time and time zone based on location
  time = as.POSIXct(
    x = time_in,
    tz = tz_lookup(location)
  )

  # get the sunset time
  set = sunriset(
    crds = st_coordinates(location),
    dateTime = time,
    proj4string=CRS(st_crs(location)$proj4string),
    direction="sunset",
    POSIXct.out=TRUE
  )
  
 # get the sunrise time
  rise = sunriset(
    crds = st_coordinates(location),
    dateTime = time,
    proj4string=CRS(st_crs(location)$proj4string),
    direction="sunrise",
    POSIXct.out=TRUE
  )

  # Get a set number of times in between rise and set and drop the 
  # rise and set times
  times_seq = seq(rise$time, set$time, length.out = 5)
  adjusted_times = times_seq[-c(1,length(times_seq))]
  
  make_1_day_shade = function(time_in, location_in, shape_in){
    
    # calculate the solar angel
    solar_pos = solarpos(
      crds = st_coordinates(location_in),
      dateTime = time_in,
      proj4string=CRS(st_crs(location_in)$proj4string)
    )
    
    # calculate the foot print of the shade
    # the height attribute needs to be in the same units as the crs
    footprint = shadowFootprint(
      obstacles = as(shape_in, "Spatial"),
      obstacles_height_field = "height",
      solar_pos = solar_pos) %>% 
      st_as_sf() %>% 
      mutate(id = 1:n()) %>% 
      select(geometry, id) %>% 
      mutate(shade = 1,
             month = month(time_in),
             hour = hour(time_in))
    
    return(footprint)
  }
  
  output = map_dfr(adjusted_times,
                   ~make_1_day_shade(.x, location, shape_file_in))
  
  return(output)
}

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

# save the file
saveRDS(result, file = here(temp_folder, "R", "shade_file.rds"))


