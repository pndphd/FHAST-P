########################################
# This script loads all the files and formats parameters fo rthe rest of the run
########################################

##### Load Functions #####
source(here("scripts", "R", "parameters", "load_convert_parameters_functions.R"))

##### Initial file checks ######
# make a list of all the inputs that must exist
input_paths = list(grid_center_line_path,
                   grid_top_marker_path,
                   canopy_path,
                   cover_path,
                   daily_path,
                   fish_population_path,
                   fish_parameters_path,
                   predator_path,
                   hab_path,
                   interaction_path)

# Check if they exist
walk(input_paths, ~check_file_exists(.x))

##### Read in the files #####
grid_center_line <- st_zm(st_read(grid_center_line_path, quiet = TRUE))
grid_top_marker <- st_zm(st_read(grid_top_marker_path, quiet = TRUE))
canopy_shape <- st_read(canopy_path, quiet = TRUE) 
cover_shape <- st_read(cover_path, quiet = TRUE) 
daily_inputs <- load_text_file(daily_path)
fish_daily_inputs <- read.csv(file = fish_population_path, sep = ",", header = TRUE) %>%
  mutate(date = mdy(date))
fish_parm_temp <- read_csv(file = fish_parameters_path,
                           col_types = cols(.default = "d", species = "c"))
pred_parm_temp <- read_csv(file = predator_path,
                           col_types = cols(.default = "d", species = "c")) %>%
  rename(term = species)
hab_parm_temp <- load_text_file(hab_path)
int_parm_temp <- load_text_file(interaction_path)

##### Check the CRS off shape files #####
# Check that these are the same crs
if (!compareCRS(grid_center_line, grid_top_marker) |
    !compareCRS(canopy_shape, grid_top_marker) |
    !compareCRS(canopy_shape, cover_shape)){
  stop('The CRSs of the two shape files and the aoi are not the same.')
}

# Check the the center line is one object
if (NROW(grid_center_line) != 1)
  stop('The river center line shape file is a multipart object.\n
       It must be a single part object.')

##### Convert into usable formats ##### 
# fish parameters to named list with species as index
fish_parm <- fish_parm_temp %>% 
  rename(species_temp = species) %>% 
  pivot_longer(cols=c(-species_temp), names_to="specie")%>%
  pivot_wider(names_from=c(species_temp)) %>% 
  as.list()

# habitat and interaction parameters
habitat_parm <- tibble(
  # from the habitat file
  hab_bentic_ene = hab_parm_temp["benthic food energy density", ],
  hab_drift_ene = hab_parm_temp["drift food energy density", ],
  hab_benthic_hab = hab_parm_temp["benthic food habitat", ],
  hab_drift_con = hab_parm_temp["drift food density", ],
  hab_bentic_con = hab_parm_temp["benthic food density", ],
  vel_cutoff = hab_parm_temp["velocity cutoff", ],
  dep_cutoff = hab_parm_temp["depth cutoff", ],
  resolution = hab_parm_temp["resolution", ],
  buffer = hab_parm_temp["buffer", ],
  pred_per_area = hab_parm_temp["predators per area", ],
  superind_ratio = hab_parm_temp["superindividual ratio", ],
  # from the interaction file
  shelter_frac = int_parm_temp["cover velocity fraction", ],
  reaction_distance = int_parm_temp["temperature predator area baseline", ],
  pred_success = int_parm_temp["predator success baseline", ],
  int_pct_cover = int_parm_temp["percent cover intercept", ],
  sqrt_pct_cover = int_parm_temp["percent cover root", ],
  pct_cover = int_parm_temp["percent cover slope", ],
  sqrt_pct_cover_x_pct_cover = int_parm_temp["percent cover 3-2 root", ],
  dis_to_cover_m = int_parm_temp["distance to cover slope", ],
  dis_to_cover_int = int_parm_temp["distance to cover intercept", ],
  turbidity_int = int_parm_temp["turbidity intercept", ],
  turbidity_slope = int_parm_temp["turbidity slope", ],
  d84_size = int_parm_temp["d84 size", ],
  ben_vel_height = int_parm_temp["benthic velocity height", ]) %>% 
  # make everything which is a number a doubble
  mutate(across(!c(hab_benthic_hab), ~as.numeric(.))) 

# Predator parameters
# make lists of parameters per file, so the dataframe can be separated appropriately
log_model_par_names <- c("intercept_glm", "shade", "veg",
                         "wood", "depth", "velocity", "substrate")
temp_model_par_names <- c("intercept_C", "temperature_C")
gape_par_names <- c("a", "B")
length_dist_par_names <- c("meanlog", "sdlog")

# combined all parameter lists into a list of lists to use with map
par_lol <- list(log_model_par_names,
                temp_model_par_names,
                gape_par_names,
                length_dist_par_names)

# make a list of separate dataframes
models_separated <- map(par_lol, ~ pred_parm_temp %>%
                          filter(term %in% .x))

# reshape data depending on needs
longer <- map(list(models_separated[[1]], models_separated[[4]]), params_pivot_longer)
wider <- map(list(models_separated[[2]], models_separated[[3]]), params_pivot_wider)

# make a list of lists into a single list
df_list <- flatten(list(longer, wider))

# final reshape to the length distribution model params
df_list[[2]] <- df_list[[2]] %>% 
  pivot_wider(names_from = "term", values_from = "estimate")

# variabel names
pred_output_names <- list("pred_model_params",
                          "pred_length_data",
                          "pred_temp_params",
                          "gape_params")

# assign the variables
walk2(df_list, pred_output_names, make_variables)

##### Make predator models for habitat summary #####
# for models related to cover, an lm() object is rebuilt
# using fake data and the model params 

# make a dataframe with fake x values and predicted values
synth_cover_data <- tibble(
  pct_cover = seq(0.01, 0.99, 0.01),
  dis_to_cover_m = habitat_parm$int_pct_cover + 
    habitat_parm$sqrt_pct_cover * sqrt(pct_cover) + 
    habitat_parm$dis_to_cover_m * pct_cover + 
    habitat_parm$sqrt_pct_cover_x_pct_cover * pct_cover ^ 1.5)

# build the model object
pct_cover_model <- lm(dis_to_cover_m ~ pct_cover * sqrt(pct_cover),
                      data = synth_cover_data)

# model params for the model converting distance to cover to safety from predation
synth_cover_benefit_data <- tibble(
  dis_to_cover_m = seq(0, 5, 0.1),
  unitless_y = 1 / (1 + exp(-1 * (dis_to_cover_m * habitat_parm$dis_to_cover_m +
                                    habitat_parm$dis_to_cover_int))))

cover_ben_model <- glm(unitless_y ~ dis_to_cover_m,
                       data = synth_cover_benefit_data,
                       family = stats::quasibinomial(logit))

##### Write the files for NetLogo ##### 
# Write the fish parameters
write_csv(fish_parm_temp,
          file = here(temp_folder, "NetLogo", "fish_params_input.csv"))

# Write the habitat parameters
write_csv(habitat_parm %>%
            mutate(across(where(is.numeric), as.character)) %>% 
            pivot_longer(cols = everything()),
          file = here(temp_folder, "NetLogo", "habitat.csv"))

# Copy over the base predator input file into the temp folder
file.copy(
  from = predator_path,
  to = here(temp_folder, "NetLogo", basename(predator_path)),
  overwrite = TRUE,
  copy.date = TRUE) %>%
  invisible()

# Save the predation models
cover_models <- list(pct_cover_model, cover_ben_model)
cover_model_filenames <- list("pct_cov_convers_model.rds",
                              "dis_to_cov_model.rds")
walk2(cover_models, cover_model_filenames, write_rds_temp_folder)
