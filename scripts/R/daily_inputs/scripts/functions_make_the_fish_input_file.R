##### Description #####
# These are the functions for the make_the_input_file

# Make the function to calculate fish per day
spread_out_fish <- function(...){
  current <- tibble(...)
  # Get the standard deviation
  sd = (current$interquartile/2)/qnorm(0.75)
  df = data.frame(date = seq(from = current$date - 100 * current$interquartile,
                             to = current$date + 100 * current$interquartile,
                             by = 1)) %>% 
    mutate(difference = as.numeric(date - current$date),
           value = 1/(sd*sqrt(2*pi))*exp(-0.5*(difference/sd)^2),
           raw_number = value * current$number,
           wrong_number = round(raw_number,0)) %>% 
    filter(wrong_number > 0) %>% 
    # Correct for the tails that were taken off
    mutate(fraction = value/sum(value),
           number = round(fraction * current$number,0),
           species = current$species,
           lifestage = current$lifestage,
           length = current$length,
           length_sd = current$sd) %>% 
    select(date, number, species, lifestage, length, length_sd)
  return(df)
}