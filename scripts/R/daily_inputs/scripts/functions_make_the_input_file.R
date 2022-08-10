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