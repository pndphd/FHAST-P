##### Description #####
# This script takes an input file and reads form it what type of flow and temperature
# input file the program should make. It then makes it.
library(here)

# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

# Load the functions
source(here("scripts","R","daily_inputs","scripts","functions_make_the_input_file.R"))

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

#get the name of the input file
input_file_name = here(input_folder, input_data["folder",], "daily", input_data["daily file",])

# Check to see if the file exists
if (!file.exists(input_file_name))
  stop('The input file does not exist.')

# Read in the file
input_file <- read.csv(file = input_file_name,
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
  
if (file_type == "distribution"){
  # Get parameters
  temp_type = input_file["temperature distribution",]
  temp_mean = as.numeric(input_file["temperature mean",])  
  temp_SD = as.numeric(input_file["temperature SD",])
  temp_autocor = as.numeric(input_file["temperature autocorrelation",])
  temp_change = as.numeric(input_file["temperature change",])
  flow_type = input_file["flow distribution",]
  flow_mean = as.numeric(input_file["flow mean",])
  flow_SD = as.numeric(input_file["flow SD",])
  flow_autocor = as.numeric(input_file["flow autocorrelation",])
  flow_change = as.numeric(input_file["flow change",])
  flow_min = as.numeric(input_file["flow min",])
  
  # Get starting values. Just the means
  if (flow_type == "normal") {
    start_flow = flow_mean
  } else if (flow_type == "lognormal") {
    start_flow = exp(flow_mean)
  } else {
    stop('\nYou did not select a valid flow distribution type.\nPlease select "normal" or "log"')
  }
  
  if (temp_type == "normal") {
    start_temp = temp_mean
  } else if (temp_type == "lognormal") {
    start_temp = exp(temp_mean)
  } else {
    stop('\nYou did not select a valid temperature distribution type.\nPlease select "normal" or "log"')
  }

  # Make placeholder data set
  output_data = data.frame(date = dates,
                           day = days,
                           flow_cms_con = accumulate(rep(start_flow, length(dates)),
                                                 ~calc_next_value(.x,
                                                                  flow_mean,
                                                                  flow_SD,
                                                                  flow_autocor,
                                                                  flow_type)),
                           temp_c_con = accumulate(rep(start_temp, length(dates)),
                                               ~calc_next_value(.x,
                                                                temp_mean,
                                                                temp_SD,
                                                                temp_autocor,
                                                                temp_type))) %>% 
    # Change the value depending on the increase
    # Also check for the min values and 0 for temperature
    mutate(flow_cms = pmax(flow_cms_con + flow_change * (day - 1), flow_min),
           temp_c = pmax(temp_c_con + temp_change * (day - 1), 0)) %>% 
    select(-flow_cms_con, -temp_c_con)
  
} else if (file_type == "hydrograph"){
  
  # Get parameters
  hydro_file_name = input_file["file",]
  
  # Read in the file and convert to date
  hydro_file = read.csv(file = here("inputs", hydro_file_name)) %>% 
    mutate(date = mdy(date))
  
  # use left join to just get dates wanted and convert to dumb excel date format
  output_data = data.frame(date = dates,
                           day = days) %>% 
    mutate(date = mdy(date)) %>% 
    left_join(hydro_file, by = "date") %>% 
    mutate(date = format(date, "%m/%d/%Y"))

} else if (file_type == "link"){
  # Get parameters
  temp_SD = as.numeric(input_file["temperature SD",])
  temp_cor = as.numeric(input_file["temperature correlation",])
  temp_a = as.numeric(input_file["temperature intercept",])
  temp_b = as.numeric(input_file["temperature slope",])
  flow_type = input_file["flow distribution",]
  flow_mean = as.numeric(input_file["flow mean",])
  flow_SD = as.numeric(input_file["flow SD",])
  flow_autocor = as.numeric(input_file["flow autocorrelation",])
  flow_change = as.numeric(input_file["flow change",])
  flow_min = as.numeric(input_file["flow min",])
  
  # Get starting values. Just the means
  if (flow_type == "normal") {
    start_flow = flow_mean
  } else if (flow_type == "lognormal") {
    start_flow = exp(flow_mean)
  } else {
    stop('\nYou did not select a valid flow distribution type.\nPlease select "normal" or "log"')
  }

  # Make placeholder data set
  output_data = data.frame(date = dates,
                           day = days,
                           flow_cms_con = accumulate(rep(start_flow, length(dates)),
                                                     ~calc_next_value(.x,
                                                                      flow_mean,
                                                                      flow_SD,
                                                                      flow_autocor,
                                                                      flow_type))) %>% 
    # Change the value depending on the increase
    # Also check for the min values and 0 for temperature
    rowwise() %>% 
    mutate(flow_cms = pmax(flow_cms_con + flow_change * (day - 1), flow_min),
           temp_c_temp = (temp_a + temp_b * flow_cms),
           temp_c = temp_c_temp* temp_cor + 
             (1 - temp_cor) * rnorm(1, mean = temp_c_temp, sd = temp_SD)) %>% 
    select(-flow_cms_con, -temp_c_temp)

  
} else {
  stop('\nYou did not select a valid type.\nPlease select "link", "distribution", or "hydrograph"')
}
  
##### Make the Files #####
daily_input_file = output_data %>% 
  mutate(month = month(as_date(date, format = "%m/%d/%Y")))

write.csv(x = daily_input_file,
          file = here(temp_folder,"NetLogo","daily_input_file.csv"),
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
   scale_color_manual(name = NULL, values = c(Temp_C = cbPalette[3],
                                             Flow_cms = cbPalette[1]))+
  scale_y_continuous("Flow (cms)",
                     sec.axis = sec_axis(~ (. - a)/b,
                                         name = expression("Temperature ("*~degree*"C)"))) +
   labs(x = "Date") 

# Print in outside window
# X11(width = 10, height = 5)
# print(time_series_plot)

# Make the function for histogram plots
make_hist_plot = function(data_in, variabel, legend){
  out_hist_plot = ggplot(data = data_in, aes_string(variabel)) +
    theme_classic(base_size = 20) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank()) +
    geom_histogram(aes(y=..density..),
                   bins = 30,
                   color = cbPalette[1],
                   fill = cbPalette[1],
                   alpha = 0.25,
                   size = 1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = legend)
  
  return(out_hist_plot)
}

# Make the plots
temp_hist_plot = make_hist_plot(output_data,
                                "temp_c",
                                expression("Temperature ("*~degree*"C)"))

flow_hist_plot = make_hist_plot(output_data,
                                "flow_cms",
                                "Flow (cms)")

# Print in outside window using patchwork
# X11(width = 10, height = 10)
# print(flow_hist_plot / temp_hist_plot)




  
  
