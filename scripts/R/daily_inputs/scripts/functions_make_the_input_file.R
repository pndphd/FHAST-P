##### Description #####
# These are the functions for the make_the_input_file

# Make the function to calculate the next value in the distribution
calc_next_value = function(current_value, mean, sd, autocor, dist_type_in){
  # Select next vlue depending on desired distribution
  if (dist_type_in == "normal") {
    new_value = current_value * autocor +
      (1-autocor) * rnorm(1, mean = mean, sd = sd)
  } else if (dist_type_in == "lognormal") {
    new_value = current_value * autocor +
      (1-autocor) * rlnorm(1, mean = mean, sd = sd)
  } else {
    stop('\nInvalid distribution type.')
  }
  
  return(new_value)
}

# This function just reads in the parameters based on the fiel type
read_parameters <- function(input_file_in, file_type_in){
  output_list = list()
  if (file_type_in == "distribution"){
    output_list$temp_type = input_file_in["temperature distribution",]
    output_list$temp_mean = as.numeric(input_file_in["temperature mean",])  
    output_list$temp_SD = as.numeric(input_file_in["temperature SD",])
    output_list$temp_autocor = as.numeric(input_file_in["temperature autocorrelation",])
    output_list$temp_change = as.numeric(input_file_in["temperature change",])
    output_list$flow_type = input_file_in["flow distribution",]
    output_list$flow_mean = as.numeric(input_file_in["flow mean",])
    output_list$flow_SD = as.numeric(input_file_in["flow SD",])
    output_list$flow_autocor = as.numeric(input_file_in["flow autocorrelation",])
    output_list$flow_change = as.numeric(input_file_in["flow change",])
    output_list$flow_min = as.numeric(input_file_in["flow min",])
    output_list$turb_type = input_file_in["turbidity distribution",]
    output_list$turb_mean = as.numeric(input_file_in["turbidity mean",])
    output_list$turb_SD = as.numeric(input_file_in["turbidity SD",])
    output_list$turb_autocor = as.numeric(input_file_in["turbidity autocorrelation",])
    output_list$turb_change = as.numeric(input_file_in["turbidity change",])
    
  } else if (file_type_in == "hydrograph"){
    hydro_file_name = input_file_in["file",]
    
  } else if (file_type_in == "link"){
    output_list$temp_SD = as.numeric(input_file_in["temperature SD",])
    output_list$temp_cor = as.numeric(input_file_in["temperature correlation",])
    output_list$temp_a = as.numeric(input_file_in["temperature intercept",])
    output_list$temp_b = as.numeric(input_file_in["temperature slope",])
    output_list$turb_SD = as.numeric(input_file_in["turbidity SD",])
    output_list$turb_cor = as.numeric(input_file_in["turbidity correlation",])
    output_list$turb_a = as.numeric(input_file_in["turbidity intercept",])
    output_list$turb_b = as.numeric(input_file_in["turbidity slope",])
    output_list$flow_type = input_file_in["flow distribution",]
    output_list$flow_mean = as.numeric(input_file_in["flow mean",])
    output_list$flow_SD = as.numeric(input_file_in["flow SD",])
    output_list$flow_autocor = as.numeric(input_file_in["flow autocorrelation",])
    output_list$flow_change = as.numeric(input_file_in["flow change",])
    output_list$flow_min = as.numeric(input_file_in["flow min",])
    
  } else {
    stop('\nYou did not select a valid type.
         \nPlease select "link", "distribution", or "hydrograph"')
  }

    return(output_list)
}

# Get starting values. Just the means
get_starting_values <- function(dist_type, mean_value){
    if (dist_type == "normal") {
      start_value = mean_value
    } else if (dist_type == "lognormal") {
      start_value = exp(mean_value)
    } else {
      stop('\nYou did not select a valid flow distribution type.\nPlease select "normal" or "log"')
    }
  return(start_value)
}

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