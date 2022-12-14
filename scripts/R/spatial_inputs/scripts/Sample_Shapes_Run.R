########################################
# This runs the scripts and functions to sample shape files with the grid
########################################

##### Load the functions #####
# Load the function to sample shapes on the grid
source(here("scripts","R","spatial_inputs","scripts","Sample_Shapes_Functions.R"))

# inputs
# temp_river_grid_path, temp_shape_file_path, cover_path, hab_path, aoi_path
# outputs
# temp_netlogo_shape_data_path

temp_river_grid_path <- here(temp_folder, "R", paste0("river_grid_",
                                                      habitat_parm$resolution,
                                                      "_",
                                                      habitat_parm$buffer,
                                                      ".rds"))
temp_shape_file_path <- here(temp_folder, "R", "shade_file.rds")

temp_netlogo_shape_data_path <- here(temp_folder,"NetLogo",
                                     paste0("Shape_Data_Input_",
                                            habitat_parm$resolution,"_",
                                            habitat_parm$buffer, ".csv"))

input_output_file_paths <- c(temp_river_grid_path, temp_shape_file_path,
                             cover_path, hab_path, aoi_path,
                             temp_netlogo_shape_data_path)

hash_storage <-here(temp_folder, "sample_shapes_run_hashes.txt")

if (!compare_last_run_hashes(hash_storage, input_output_file_paths)) {
    
  
  ##### Load Files #####
  # Load the grid file
  river_grid = readRDS(temp_river_grid_path)
  # Load the shade file
  shade_file = readRDS(temp_shape_file_path) 
  
  ##### Main Work #####
  # get what is classified as benthic food habitat
  benthic_food_habitat = habitat_parm$hab_benthic_hab %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    str_trim(side = "both")
  # make a list of files and variable names
  cover_names = list("veg", "wood", "fine", "gravel", "cobble", "rock")
  
  # Get a list of data frames with just one for each df that 
  shape_dfs = map(cover_names, ~select(cover_shape, matches(.x))) 
  
  the_variables = c(cover_names, as.list(paste0("shade_", seq(1,12,1))))
  shape_files = c(shape_dfs, shade_file)
                    
  # Sample all the shapes over the grid
  sampeled_shapes = sample_all_shapes(river_grid,
                                      shape_files,
                                      the_variables,
                                      the_variables) %>%
    # Add in column fo benthic food
    mutate(ben_food_fra = rowSums(across(benthic_food_habitat)))
  
  
  # Add in the AOI
  if(is.na(aoi_path)){
    sampeled_w_aoi = sampeled_shapes %>%
      mutate(aoi = 1) %>% 
      st_as_sf(sf_column_name = "geometry") 
  } else {
    # Load the aoi shape
    aoi_shape = st_read(aoi_path, quiet = TRUE) %>% 
    mutate(aoi = 1) %>% 
    select(aoi)
    
    sampeled_w_aoi = sample_shape_with_grid (river_grid, aoi_shape, "aoi", "aoi") %>%
      mutate(aoi = ifelse(aoi>0,1,0)) %>% 
      select(aoi, ID) %>% 
      right_join(sampeled_shapes, by = c("ID")) %>% 
      st_as_sf(sf_column_name = "geometry")
  }
  
  shapes_csv = sampled_to_csv(sampeled_w_aoi)
  
  ##### Save Outputs #####
  write.csv(shapes_csv,
            temp_netlogo_shape_data_path,
            na = "-9999",
            row.names = FALSE)
  
  store_last_run_hashes(hash_storage, input_output_file_paths)
}
