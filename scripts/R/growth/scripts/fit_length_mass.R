##### Description #####
# This script fits growth data to get 2 parameters

library(here)
# Load Libraries and some base parameters
source(here("scripts","R","main","load_libraries.R"))

gs_growth_data <- read.csv(here("scripts","R","growth", "inputs",
                                "green_sturgeon_lenght_weight_data.csv"),
                           header = TRUE) %>% 
  mutate(mass_g = mass_kg * 1000,
         ln_mass = log(mass_g),
         ln_length = log(length_cm))

model = lm(gs_growth_data$ln_mass ~ gs_growth_data$ln_length)

slope = model$coefficients[[2]]
intercept = exp(model$coefficients[[1]])
