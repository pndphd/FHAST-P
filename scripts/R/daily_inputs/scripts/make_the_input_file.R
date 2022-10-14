##### Description #####
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.

library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load the functions
source(here("scripts","R","daily_inputs","scripts","functions_make_the_input_file.R"))

# Check to see if the file exists
if (!file.exists(daily_file_name))
  stop('The input file does not exist.')

# Read in the file
input_file <- read.csv(file = daily_file_name,
                      sep = "=",
                      row.names = 1,
                      header = FALSE) %>% 
  # Trim off white spaces form values
  rename(value = 1) %>% 
  mutate(value = str_trim(value, side = c("both")))

##### Make the Files #####
# Get what type of file it is
file_type = input_file["type",]

# get dates in correct R format
start_date = mdy(input_file["start date",])
end_date = mdy(input_file["end date",])

# Make a sequence of dates and put in dumb excel format
dates = seq(start_date, end_date, 1) %>% 
  format("%m/%d/%Y")
days = seq(1, length(dates))

# Get parameters
params = read_parameters(input_file, file_type)
  
if (file_type == "distribution"){
  
  # get the starting values
  start_flow = get_starting_values(params$flow_type, params$flow_mean)
  start_temp = get_starting_values(params$temp_type, params$temp_mean)
  start_turb = get_starting_values(params$turb_type, params$turb_mean)

  # Make placeholder data set
  output_data = data.frame(date = dates,
                           day = days,
                           flow_cms_con = accumulate(rep(start_flow, length(dates)),
                                                 ~calc_next_value(.x,
                                                                  params$flow_mean,
                                                                  params$flow_SD,
                                                                  params$flow_autocor,
                                                                  params$flow_type)),
                           turb_ntu_con = accumulate(rep(start_turb, length(dates)),
                                                     ~calc_next_value(.x,
                                                                      params$turb_mean,
                                                                      params$turb_SD,
                                                                      params$turb_autocor,
                                                                      params$turb_type)),
                           temp_c_con = accumulate(rep(start_temp, length(dates)),
                                               ~calc_next_value(.x,
                                                                params$temp_mean,
                                                                params$temp_SD,
                                                                params$temp_autocor,
                                                                params$temp_type))) %>% 
    # Change the value depending on the increase
    # Also check for the min values and 0 for temperature and turbidity
    mutate(flow_cms = pmax(flow_cms_con + params$flow_change * (day - 1), params$flow_min),
           temp_c = pmax(temp_c_con + params$temp_change * (day - 1), 0),
           turb_ntu = pmax(turb_ntu_con + params$turb_change * (day - 1), 0)) %>% 
    select(-flow_cms_con, -temp_c_con, -turb_ntu_con)
  
} else if (file_type == "hydrograph"){
  
  # Get parameters
  hydro_file_name = input_file["file",]
  
  # Read in the file and convert to date
  hydro_file = read.csv(file = here(hydrology_folder, hydro_file_name)) %>% 
    mutate(date = mdy(date))
  
  # use left join to just get dates wanted and convert to dumb excel date format
  output_data = data.frame(date = dates,
                           day = days) %>% 
    mutate(date = mdy(date)) %>% 
    left_join(hydro_file, by = "date") %>% 
    mutate(date = format(date, "%m/%d/%Y"))

} else if (file_type == "link"){

  # get the starting values
  start_flow = get_starting_values(params$flow_type, params$flow_mean)

  # Make placeholder data set
  output_data = data.frame(date = dates,
                           day = days,
                           flow_cms_con = accumulate(rep(start_flow, length(dates)),
                                                     ~calc_next_value(.x,
                                                                      params$flow_mean,
                                                                      params$flow_SD,
                                                                      params$flow_autocor,
                                                                      params$flow_type))) %>% 
    # Change the value depending on the increase
    # Also check for the min values and 0 for temperature and Turbidity
    rowwise() %>% 
    mutate(flow_cms = pmax(flow_cms_con + params$flow_change * (day - 1), params$flow_min),
           temp_c_temp = (params$temp_a + params$temp_b * flow_cms),
           temp_c = temp_c_temp* params$temp_cor + 
             (1 - params$temp_cor) * rnorm(1, mean = temp_c_temp, sd = params$temp_SD),
           turb_ntu_temp = (params$turb_a + params$turb_b * flow_cms),
           turb_ntu = turb_ntu_temp* params$turb_cor + 
             (1 - params$turb_cor) * rnorm(1, mean = turb_ntu_temp, sd = params$turb_SD)) %>% 
    select(-flow_cms_con, -temp_c_temp, -turb_ntu_temp)
}

##### Make the Files #####
daily_input_file = output_data %>% 
  mutate(month = month(as_date(date, format = "%m/%d/%Y")))

write.csv(x = daily_input_file,
          file = here(temp_folder,"R","daily_input_file.csv"),
          row.names = FALSE)

##### Plots #####
# Get y-limits for a first and second graph
ylim_first = c(min(output_data$flow_cms), max(output_data$flow_cms))  
ylim_second = c(min(output_data$temp_c), max(output_data$temp_c))

# Get parameters to trasform data so it displays on second axis
b = diff(ylim_first)/diff(ylim_second)
a = ylim_first[1] - b*ylim_second[1]

# Plot both time series
time_series_plot = ggplot(data = output_data, aes(x = mdy(date), y = flow_cms)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_path(aes(color = "Flow_cms"), size=1,
            linetype = "solid") +
  geom_path(aes(y = a + temp_c*b, color = "Temp_C"), size = 1) +
  geom_path(aes(y = a + turb_ntu*b, color = "Turb_NTU"), size = 1) +
   scale_color_manual(name = NULL, values = c(Temp_C = cbPalette[3],
                                             Flow_cms = cbPalette[1],
                                             Turb_NTU = cbPalette[4]))+
  scale_y_continuous("Flow (cms)",
                     sec.axis = sec_axis(~ (. - a)/b,
                                         name = expression("Temp and Turb ("*~degree*"C or NTU)"))) +
   labs(x = "Date") 

# Print in outside window
X11(width = 10, height = 5)
print(time_series_plot)

# Make the plots
temp_hist_plot = make_hist_plot(output_data,
                                "temp_c",
                                expression("Temperature ("*~degree*"C)"))

flow_hist_plot = make_hist_plot(output_data,
                                "flow_cms",
                                "Flow (cms)")

# Print in outside window using patchwork
X11(width = 10, height = 10)
print(flow_hist_plot / temp_hist_plot)




  
  
