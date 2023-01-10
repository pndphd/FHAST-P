;; The FHAST program

;; Load Extensions
extensions [
  csv      ; The extenshion the allows for csv IO
  palette  ; Allows the use of more palets like ColorBrewer
  time     ; Use the LogoTime extenshion to tract dates/time
  table    ; Allows for use of tables instead of lists for speed reasons
  rnd      ; For random draws
  profiler ; includes primitives that measure how many times the procedures are called during a run and how long each call takes
]

;##### Define Variables #######################################################

;; Define global variables
globals [
  ;; General run setup variabels
  resolution          ; The grid resolution in meters
  resolution_file     ; the file that contaions the resolution and buffer information
  buffer              ; Set the buffer (only used to read in the files)
  resolution_factor   ; A factor used to control how large the display can get
  area_column         ; The location index for area of a cell read in from the flow CSV
  reach_start         ; The upper most reach distance
  reach_end           ; The bottom reach distance
  top_patches         ; The patches that are closest to the top and are wet
  stop_flag           ; A flag to note when the code is done running

  ;; Internal parameter variables
  species_list         ; A list of species names
  fish_logistics_table ; A table of logistic functions for survival, by species

  ;; Time variables
  first_day           ; Pointless time variabel to initalize
  last_day            ; Time varible for the last day in the simulation
  tick_date           ; Variable to track date

  ;; Daily input variabels
  daily_input_csv     ; The daily flow and temperature data
  day                 ; The day of the simulation
  date                ; A date value
  month               ; The month (used for shade)
  flow_column         ; The location index for the flow from the daily CSV
  month_column        ; The location index for the months from the daily CSV
  daily_flow_values   ; The complete list of daily flow values
  month_values        ; A list of the months
  flow                ; The daily flow value
  x_flow              ; The location index for the x position of a cell read in from the flow CSV
  y_flow              ; The location index for the y position of a cell read in from the flow CSV
  temp_column         ; The location index for the temperature from the daily CSV
  daily_temp_values   ; The complete list of daily temp values
  temperature         ; The daily temperature value
  turbidity_column     ; The column that gives the turbidity values
  daily_turbidity_values  ; The complete list of daily turbidity values
  turbidity           ; The daily turbidity
  photoperiod         ; daily photoperiod (day length) in hours
  daily_photoperiod_values ; The complete list of daily photoperiod values

  ;; Habitat params
  habitat_params_csv ; Csv file with a variety of habitat params, including foods, predator density, resolution, buffer, etc.
  hab_drift_con      ; Availability of drift food in reach, g/cm3 (CHANGED TO G/M3)
  hab_ben_con
  hab_ben_ene
  hab_drift_ene
  superind_ratio
  ;resolution
  ;buffer
  pred_per_area
  shelter_frac
  reaction_distance
  pred_success
  d84_size          ; The grain size of the bed for the law of the wall calculation
  ben_vel_height    ; The height off the bed where the velocity is experienced by the benthic fish

  ; pct cover to distance to cover conversion
  int_pct_cover
  sqrt_pct_cover
  pct_cover
  sqrt_pct_cover_x_pct_cover

  ; dis to cover to prey safety conversion
  dis_to_cover_par
  dis_to_cover_int

  ; turbidity model params
  turb_int
  turb_slope
  turb_survival_bonus   ; bonus to fish survival against preds due to turbidity; updates dailys

  ;; Daily Fish input variables
  species_id
  daily_fish_csv        ; The column that gives the number of fish per day
  fish_wo_dates         ; The info for the fish file with not date info
  paired_fish_list      ; The dates and lists paired
  paired_fish_table     ; The same paired list but in tabel form
  fish_date_column      ; The column that gives the dates of fish entry
  number_column         ; The column that gives the number of fish
  species_column        ; The column the lists the species of fish
  lifestage_column      ; The column that lists the life stage
  length_column         ; The column the gives the fish lenght
  length_sd_column      ; The column that gives the fish lenght sd
  daily_number_values   ; The daily values for the fish counts
  daily_species_values  ; The daily values for the species
  daily_ls_values       ; The daily values for the lifestage
  daily_length_values   ; The daily values for the length
  daily_sd_values       ; The daily values for the sd of length
  fish_date_values      ; The values in the fish date columns
  fish_formated_dates   ; The values in the fish date columns formated as LogoTime
  ;fish_spp_index

  ;; Flow raster variabels
  flow_input_csv      ; The variabel to hole the flow csv
  flow_values         ; The list of flow values for which we have rasters
  today_index         ; The flow index for each day
  wet_patches         ; A list of patches that have water in them each day
  max_depth           ; The max depth listed in any raster
  max_velocity        ; The max velcoity listed in any raster

  ;; Shape file variables
  shape_input_csv     ; The variabel to hole the shape csv
  x_shape             ; The location index for the x position of a cell read in from the shape CSV
  y_shape             ; The location index for the y position of a cell read in from the shape CSV
  wood_column         ; The column that has the information on percent wood
  veg_column          ; The column that has the information on percent vegetation
  fine_column         ; The column that has the information on percent fine sediment
  gravel_column       ; The column that has the information on percent spawning gravel
  cobble_column       ; The column that has the information on percent large cobble
  rock_column         ; The column that has the information on percent rock or bedrock
  ben_food_column    ; The column that has the information on fraction of area that has benthic food
  canopy_column       ; The column that has the information on percent canopy cover
  photoperiod_column  ; The column that has the information on photoperiod

  max_cell_available_vel_shelter ; Max available area of velocity shelter from all cells (just used to color patches according to the available vel shelter)

  ;; Fish variables
  fish_params_csv     ; A csv with the individual species parameters
  ;paired_param_table ; A table with pairs of parameters, used for the logistic equations

  ;; The flags for the different types of fish
  fish_benthic         ; Is the fish a benthic fish

  fish_move_dist_A ; Multiplier for maximum movement distance (unitless)
  fish_move_dist_B ; Exponent for maximum movement distance (unitless)
  fish_weight_A        ; Site-specific length-weight relationship for fish in good condition
  fish_weight_B        ; Site-specific length-weight relationship for fish in good condition
  fish_terr_A          ; The intercept of the fish territory size equation
  fish_terr_B          ; The slope of the fish territory size equation
  met_int              ; intercept for metabolic rate equation
  met_lm               ; log mass term for metabolic rate equation
  met_lt               ; log temperature term for metabolic rate equation
  met_v                ; log speed term for metabolic rate equation
  met_lm_lt            ; log mass * log temperature term for metabolic rate equation
  met_lm_v             ; log mass * velocity term for metabolic rate equation
  met_sqv              ; sqrt velocity term for metabolic rate equation
  met_lm_sqv           ; log mass * sqrt velocity term for metabolic rate equation
  met_t                ; temperature term for metabolic rate equation
  met_lm_t             ; log mass * temperature term for metabolic rate equation
  total_metab_rate     ; total metabolic rate
  fish_cmax_A          ; Allometric constant in cMax equation (unitless)
  fish_cmax_B          ; Allometric exponent in cMax equation (unitless)
  fish_cmax_C          ; First parameter for the cmax temperature relation (C)
  fish_cmax_D          ; Second parameter for the cmax temperature relation (C)
  fish_fr_A            ; First parameter for the functional response curve (m^2)
  fish_fr_B            ; Second parameter for the functional response curve (m^2)
  fish_detect_dist     ; Distance over which fish can see or attack (cm)
  fish_react_dist_A    ;Intercept in equation for detection distance (cm) (CHANGED TO M)
  fish_react_dist_B    ; Multiplier in equation for detection distance (unitless)
  fish_turbid_threshold ; Highest turbidity that causes no reduction in detection distance (NTU)
  fish_turbid_min       ; Minimum value of the turbidity function (unitless)
  fish_turbid_exp       ; Multiplier in exponential term for the turbidity function (unitless)
  fish_turbid_function
  fish_capture_1        ; Ratio of cell velocity to fish’s maximum swim speed at which capture success is 0.1 (unitless)
  fish_capture_9        ; Ratio of cell velocity to fish’s maximum swim speed at which capture success is 0.9 (unitless)
  shelter_speed_frac    ; Fraction by which cell velocity is reduced due to velocity shelter
  fish_energy_density   ; The ratio (J/g) between net energy intake and change in weight

  mort_condition_S_at_K1 ; Part of relationship between survival and condition (K); Fish condition factor K at which survival is 10 pct (unitless)
  mort_condition_S_at_K9 ; Part of relationship between survival and condition (K); Fish condition factor K at which survival is 90 pct (unitless)
  mort_high_temp_T1      ; Parameters for the logistic equation for high temp
  mort_high_temp_T9      ; Parameters for the logistic equation for high temp
  fish_max_swim_param_A  ; Parameters for the logistic equation for high velocity survival-Length coefficient in maximum swim speed equation (1/s) (CHANGED)
  fish_max_swim_param_B  ; Parameters for the logistic equation for high velocity survival-Constant in maximum swim speed length term (cm/s) (CHANGED)
  fish_max_swim_param_C  ; Parameters for the logistic equation for high velocity survival-Temperature squared coefficient in maximum swim speed equation
  fish_max_swim_param_D  ; Parameters for the logistic equation for high velocity survival-Temperature coefficient in maximum swim speed equation
  fish_max_swim_param_E  ; Parameters for the logistic equation for high velocity survival-Constant in maximum swim speed temperature term (unitless)

  smolt_param_P1 ; Parameters for the logistic equation for the probability of smolting depending on photoperiod- Photoperiod where probability of being in the "smolting window" is 10 pct
  smolt_param_P9 ; Parameters for the logistic equation for the probability of smolting depending on photoperiod- Photoperiod where probability of being in the "smolting window" is 90 pct

  outmigrate_param_P1 ; Parameters for the logistic equation for the probability of outmigrating depending on the increase in mean velocity from running average velocity -
  ; Percent increase where probability of outmigration is 10 pct
  outmigrate_param_P9 ; Parameters for the logistic equation for the probability of outmigrating depending on the increase in mean velocity from running average velocity -
  ; Percent increase where probability of outmigration is 90 pct

  reach_drift_regen_distance  ; distance over which drift consumed by fish is replenished from the benthos (CHANGED to M)

  ; End of simulation outputs
  death_temp_list            ; A list of 1s representing deaths caused by temperature
  death_velocity_list        ; A list of 1s representing deaths caused by velocity
  death_stranding_list       ; A list of 1s representing deaths caused by stranding
  death_pred_list            ; A list of 1s representing deaths caused by predation
  death_condition_list       ; A list of 1s representing deaths caused by poor condition

  migrant_count_list         ; A list of 1s representing migrants
  migrant_length_list        ; A list of the migrants' length when it exits the river
  migrant_mass_list          ; A list of the migrants' mass when it exits the river
  migrant_condition_list     ; A list of the migrants' condition when it exits the river

  drifter_count_list         ; A list of 1s representing drifters
  drifter_length_list        ; A list of the drifters' length when it exits the river
  drifter_mass_list          ; A list of the drifters' mass when it exits the river
  drifter_condition_list     ; A list of the drifters' condition when it exits the river

  ;; Daily outputs
  daily_juvenile_count       ; A list of 1s representing all of the fish alive (smolt and nonsmolt)  at any given timestep
  daily_dead_fish_count      ; A list of 1s representing all of the dead fish at any given timestep
  daily_migrant_count        ; A list of 1s representing all of the fish migrating at any given timestep
  daily_smolt_count          ; A list of 1s representing all of the fish smolting at any given timestep
  daily_drifter_count        ; A list of 1s representing all of the fish drifting at any given timestep
  total_daily_juveniles      ; Total number of juveniles (smolts and nonsmolts) at any given timestep
  total_daily_dead_fish      ; Total number of dead fish at any given timestep
  total_daily_migrants       ; Total number of migrants at any given timestep
  total_daily_smolts         ; Total number of fish smolting at any given timestep
  total_daily_drifters       ; Total number of fish drifting at any given timestep

  ;; Other global variables/inernal data structures
  outfile_name_base           ; Text string for creating unique output file names for each model run
  brief_pop_outfile_name      ; String with name of brief population output file; set in build_output_files
  mortality_events_outfile_name    ; String with name of mortality events output file; set in build_output_files
  fish_events_outfile_name    ; String with name of fish events output file; set in build_output_files
  cell_info_outfile_name      ; String with name of cell info output file; set in build_output_files
  detailed_population_outfile_name  ; String with name of detailed population output file; set in build_output_files
  cell_info_list              ; List of all destination cell information (number of total fish alive in each destination, number of dead fish in each destination) per time step that can be written the the cell info outputs file
  fish_events_list            ; List of all individual fish-related events (mortality, smolting, migrating) that can be written the the fish events outputs file
  detailed_population_list    ; List of all summary fish-related info per day (total migrating, total smolting) that can be written to the detailed population output file
  next_output_time            ; A LogoTime for when the next file output is scheduled.
  shade_variable              ; Controls patch shading. Equal to "velocity", "depth", "light" or "off"
  destination_cells           ; A list of all of the destination cells during a time step
  cmax_temp_func              ; A list of cmax temperature functions for each species

  ;; Predator variables
  pred_input_file_csv          ; Input csv file with some default params for pred counts
  total_preds                  ; The total number of predators in the system

  ;; Predator model variables
  pred_species
  num_pred_species
  ; glm for habitat rating
  int_glm
  shade_glm
  veg_glm
  wood_glm
  depth_glm
  velocity_glm
  substrate_glm

  ; params for log-normal distribution
  meanlog
  sdlog

  ; gape limitation params
  a_gape
  B_gape

  ; temp vs pred activity params

  pred_temperature_activity
  int_temp
]

;; Define patch variables
patches-own [

  x_pos  ; equal to "lat_dist" in the flow and shapefile input files
  y_pos  ; equal to "distance" in the flow and shapefile input files

  ;; Flow releated variables
  area                ; The true area of the patch (not the area displayed here) (CHANGED TO M)
  wetted_fraction     ; the fraction of the area that is wetted
  wetted_fractions    ; the list of fraction of the area that is wetted
  wetted_area         ; the total area wetted (CHANGED TO M)
  depths              ; A list of all the depths at the set flows in the cell
  today_depth         ; The depth of the patch on this day (CHANGED TO M)
  yesterday_depth     ; The depth of the patch on the previous day. (CHANGED TO M)
  velocities          ; A list of all the velocities at the set flows in the cell
  today_velocity      ; The velocity of the patch on this day
  yesterday_velocity  ; The velocity of the patch on the previous day.

  ;; Shape file releated variables
  veg                 ; the fraction of the area that is covered with vegetation
  wood                ; the fraction of the area that is covered with wood
  fine                ; the fraction of the area that is covered with fine sediment
  gravel              ; the fraction of the area that is covered with gravel
  cobble              ; the fraction of the area that is cobble
  rock                ; the fraction of the area that is rock
  shades              ; All the shade values a call can have
  shade               ; The shade fraction of a cell
  ben_food_fra        ; The fraction of the cell area that has benthic food

  ;; Parameters for valid patch logic for fish
  has_visited?        ; Used to identify if the pathfinding has visited the patch
  path_to_here_cost   ; Used to store the cost to move to this patch for pathfinding
  path_survival       ; Used to store the predation risk along this path
  fish_survival       ; Survival for current fish in this path
  previous_patch      ; The previous patch in the path from pathfinding

  frac_velocity_shelter     ; The fraction of a cell with velocity shelters (the wood + veg sum)

  cell_available_ben         ; Availability ofdenthic food in, g
  cell_available_vel_shelter ; Available area of velocity shelter in cell, (m^2)
  cell_available_wet_area    ; Available wetted area in cell, (m^2)

  ratio_vel_max_swim         ; Ratio of the cell velocity and the maximum swim speed sustained by a fish
  capture_area               ; The area over which fish can capture food
  swim_speed                 ; (m/s)
  capture_success            ; The fraction of food items passing through the capture area that are actually caught
  feed_time                  ; Number of house spend feeding per day (set at 12)

  daily_intake               ; Drift intake (g/d), limited by cmax, drift availability etc
  daily_energy_intake        ; Drift intake in J energy
  daily_net_energy           ; Drift intake - total respiration (J/d)
  total_food_in_cell         ; Total amount of food in the cell (g) (basically equal to the daily net energy, can be negative)
  total_mort_risk_for_cell   ; Total probability of surviving all of the mortality risks in the cell (total_mort_risk_for_cell/total_food_in_cell)
  ratio_food_risk            ; Ratio of total mortality risk to the total amount of food in the cell
  consider_path_risk         ; Path survival multiplied by the ratio food to risk. Used by fish when selecting desination cells while taking into account path risk

  fish_death_hightemp_prob          ; Probability of a fish surviving high temperature
  ratio_swim_speed_max_swim_speed   ; Ratio of swim speed to the max swim speed that can be sustained by fish
  fish_death_aq_pred_prob           ; Probability of a fish surviving predation by other fish

  count_fish_destination                 ; Total number of fish alive in a cell at the end of a timestep

  avg_weight_fish_in_destination         ; Average weight of live fish in a cell at the end of a timestep
  avg_length_fish_in_destination         ; Average length of live fish in a cell at the end of a timestep
  avg_condition_fish_in_destination      ; Average condition of live fish in a cell at the end of a timestp

  running_average_velocity              ; The mean velocity that the fish has experienced over the last 5 time steps
  migrant_patch                         ; Identifies the patch as having a migrant juvenile in it at any given timestep

  ;; Predator-related parameters
  substrate                             ; A binary value used by the predator model
  hab_rating                            ; Rating of habitat quality for predators
  partial_hab_rating                    ; Part of the hab rating calc is done at set up to save time
  adj_hab_rating                        ; hab_rating weighted by the amount of wetted area in a patch
  num_preds                             ; Number of predators in a patch
  pred_length                           ; Mean length of predators in a cell
  max_prey_length                       ; Largest prey size that a given predator can consume based on its length
  encounter_prob                        ; Probability of encountering a predator in a cell
  cover                                 ; Total cover available to prey for predator avoidance
  distance_to_cover                     ; Average distance a fish needs to travel to reach cover in a patch
  cover_bonus                           ; The "bonus" to survival in a predator-prey encounter provided by cover
  survival_prob                         ; The probability a prey fish survives an encounter based on the cover bonus and pred success rate

]

;; Define agent variables
turtles-own [

  ;; Species
  number               ; The assigned number for this species
  species              ; The species name

  ;; Physological parameters
  mass                 ; Mass
  f_length             ; Fork length
  healthy_mass         ; Mass for a healthy condition factor
  percent_daily_growth ; Percent that the daily growth is of its mass
  fraction_growth_cmax
  fish_sex             ; Sex of fish
  fish_condition       ; Condition factor K (values 0 - 1, fish condition)
  daily_growth         ; Growth per day (g)
  max_swim_speed       ; Maximum sustainable swim speed for a fish
  new_length           ; Used in "grow" procedure - equal to previous length + change in length
  new_mass             ; Used in "grow" procedure - equal to previous mass + growth
  new_condition        ; Used in "grow" procedure
  desired_length       ; Length for a healthy individual of weight
  cmax                 ; Maximum daily consumption rate as limited by its physiology (g/d)
  energy_intake        ; J/d

  exit_status          ; If the fish is exiting this turn
  strand_status        ; If the fish is getting stranded this turn

  patch_radius              ; Radius (in cells) representing where the fish can move in one time step
  territory_size            ; The fish territory size
  fish_in_radius            ; Count of number of fish in the radius before the fish decides its destination cell
  dispersal_distance        ; If the fish is a disperser, this is the max area it can dirft to
  destination               ; The cell that the fish moves to by the end of the time step
  wet_cells_in_radius       ; The cells that are in a fish's radius
  open_cells_for_dispersal  ; All of the wet cells within the new area that the fish (a disperser) can disperse to
  downstream_random_cell    ; When the fish disperses downstream, it selects a random "open_cells_for_dispersal"
  mean_velocity_in_radius   ; The mean velocity that the fish experiences in its radius

  fish_death_starv_survival_prob  ; Probability of suriviving starvation from poor condition
  fish_smolting_prob              ; Probability of being in the "smolting window" and undergoing smolting
  fish_outmigration_prob          ; Probability of outmigrating

  velocity_experience_list        ; List of mean radius cell velocity fish experience each day

  is_in_shelter                   ; If the fish is using shelter
  is_smolt                        ; If the fish has undergone smoltification (depending on photoperiod and size)
  is_migrant                      ; If the fish has decided to migrate (depending on change in flow)
  is_alive                        ; If the fish is alive
  is_drifter                      ; A boolean for whether fish is drifting downstream due to crappy habitat

; Intermediate variables to reduce calcs.,
  fish_resp_std_wt_term           ; resp-A * wt ^ resp-B
  fish_max_speed_len_term         ; max-swim-A * L + max-swim-B
  fish_cmax_wt_term               ; cmax-A * wt ^ (1+cmax-B)

  fish_logistic_table             ; Table holding the logistic functions
]

;; Setup breeds
breed [adults adult]
breed [spawners spawner]
breed [eggs egg]
breed [juveniles juvenile]
breed [migrants migrant]

;##### Setup Actions #######################################################

;; Run the setup actions whe setup button is presed
to setup

 ; Basic reset procedure
  clear-all                           ; Clear the entire space
  reset-ticks                         ; Reset time ticks
  set stop_flag FALSE                 ; Set the stop flag to F
  set-default-shape turtles "dot"     ; Set the default turtel shape to dot

  ; Read in files and set resolution
  set habitat_params_csv csv:from-file "../../../temporary/NetLogo/habitat.csv"                           ; Read in general habitat params
  set_habitat_params                                                                                      ; Sets numerous values including resolution and buffer
  set resolution_factor 600                                                                               ; Set the resolution factor
  set daily_input_csv csv:from-file
    "../../../temporary/NetLogo/daily_input_file.csv"                                                     ; Read in the daily flow and temperature
  set flow_input_csv csv:from-file
    (word "../../../temporary/NetLogo/Depth_Velocity_Data_Input_" resolution "_" buffer ".csv")           ; Read in the depth CSV
  set shape_input_csv csv:from-file
    (word "../../../temporary/NetLogo/Shape_Data_Input_" resolution "_" buffer ".csv")                    ; Read in the shape file
  set daily_fish_csv csv:from-file "../../../temporary/NetLogo/daily_fish_input.csv"                      ; Read in the daily fish csv
  set fish_params_csv csv:from-file "../../../temporary/NetLogo/fish_params_input.csv"                    ; Read in the daily fish csv
  set pred_input_file_csv csv:from-file "../../../temporary/NetLogo/predator_params_input.csv"            ; Read in the predator input csv

  ;; Get the indexes for the columns in each file
  set day (position "day" (item 0 daily_input_csv))
  set date (position "date" (item 0 daily_input_csv))
  set flow_column (position "flow_cms" (item 0 daily_input_csv))
  set month_column (position "month" (item 0 daily_input_csv))
  set temp_column (position "temp_c" (item 0 daily_input_csv))
  set turbidity_column (position "turb_ntu" (item 0 daily_input_csv))
  set photoperiod_column (position "photoperiod" (item 0 daily_input_csv))

  ; Set the initial and end time
  set first_day time:create-with-format item date (item 1 daily_input_csv) "MM/dd/yyyy"
  set last_day time:create-with-format item date (item (length daily_input_csv - 1) daily_input_csv) "MM/dd/yyyy"
  set tick_date time:anchor-to-ticks first_day 1 "days"

  ;; For the fish parameters
  set species_list but-first (item 0 fish_params_csv)

  ;; From the fish daily input file
  set number_column (position "number" (item 0 daily_fish_csv))
  set species_column (position "species" (item 0 daily_fish_csv))
  set fish_date_column (position "date" (item 0 daily_fish_csv))
  set length_column (position "length" (item 0 daily_fish_csv))
  set length_sd_column (position "length_sd" (item 0 daily_fish_csv))
  set lifestage_column (position "lifestage" (item 0 daily_fish_csv))

  ;; From the flow inputs
  set x_flow (position "lat_dist" (item 0 flow_input_csv)) ;shows which column lat_dist is in csv, and then which element is first in the column
  set y_flow (position "distance" (item 0 flow_input_csv))
  set area_column (position "area" (item 0 flow_input_csv))

  ;; From the shape input file
  set x_shape (position "lat_dist" (item 0 shape_input_csv))
  set y_shape (position "distance" (item 0 shape_input_csv))
  set canopy_column (position "height" (item 0 shape_input_csv))
  set wood_column (position "wood" (item 0 shape_input_csv))
  set veg_column (position "veg" (item 0 shape_input_csv))
  set fine_column (position "fine" (item 0 shape_input_csv))
  set rock_column (position "rock" (item 0 shape_input_csv))
  set cobble_column (position "cobble" (item 0 shape_input_csv))
  set gravel_column (position "gravel" (item 0 shape_input_csv))
  set ben_food_column (position "ben_food_fra" (item 0 shape_input_csv))

  set death_temp_list (list)
  set death_velocity_list (list)
  set death_stranding_list (list)
  set death_pred_list (list)
  set death_condition_list (list)

  set migrant_count_list (list)
  set migrant_length_list (list)
  set migrant_mass_list (list)
  set migrant_condition_list (list)

  set drifter_count_list (list)
  set drifter_length_list (list)
  set drifter_mass_list (list)
  set drifter_condition_list (list)

  ; Initialize the cmax_tempfunction with an empty list
  set cmax_temp_func n-values length species_list [1]

  ;; Initialize patch data for pathfinding
  ask patches
  [
    set has_visited? false
    set previous_patch nobody
    set path_to_here_cost -1
    set path_survival -1
    set fish_survival -1
  ]

  ;; Set globals
  set_globals

  ;; Set the size of the world
  set_world_size

  ;; Get all the flow values that we have input rasters for
  set_temp_turbidity_flow_photoperiod

  ;; Get the daily values from the fishh input
  set_daily_fish_counts

  ;; Set the fish parameters
  set_fish_parameters

  ;; Set each patch flow values
  set_patch_flow_values

  ;; Set each patch shade values
  set_patch_shade_values

  ;; Set each patches values from the shape file
  set_patch_shape_values

  ;; Set substrate values to patches for predators
  set_substrate_values

  ;; Assign cover values to patches
  set_cover_values

  ;; Build the logistic functions for the fish
  build_logistic_functions

  ;; Load in model parameters for predators
  set_pred_params

  ;; Habitat rating pre-calculations
  set_partial_hab_rating



 ; update_output

 ;reset-ticks

end

;; Set the golbals
to set_globals
  ; Observer procedure that initializes global variables.
  ; DO NOT CHANGE any of these statements!

  set outfile_name_base (word date-and-time ".csv")
  while [(position ":" outfile_name_base) != false]    ; Replace colons, which are not allowed in file names
  [
    let the_position position ":" outfile_name_base
    set outfile_name_base replace-item the_position outfile_name_base "-"
  ]
  while [(position " " outfile_name_base) != false]    ; Replace blanks, which are not cool in file names
  [
    let the_position position " " outfile_name_base
    set outfile_name_base replace-item the_position outfile_name_base "_"
  ]

  set brief_pop_outfile_name      "b-p-o-n" ; String with temporary name of brief population output file; reset in build_output_file
  set detailed_population_outfile_name "d-p-o-n" ; String with temporary name of detailed population output file; reset in build_output_file
  set fish_events_outfile_name    "f-e-o-n" ; String with temporary name of fish events output file; reset in build_output_file
  set cell_info_outfile_name  "c-i-o-n"   ; String with temporary name of destination cell info output file; reset in build_output_file

  set fish_events_list (list)    ; List keeping track of individual fish-related events including whether the fish died, smolted, migrated etc
  set cell_info_list (list)    ; List keeping track of destination cell-related info including the number of fish in a destination cell at every time step, number of dead fish at a destination cell at every time step etc
  set detailed_population_list (list) ; List keeping track of summary fish-related info per day including the total number of fish that died per day, total alive, total migrating, etc
end

;; Set the size of the world
to set_world_size

  ; Get necessary information to set up world size
  let x_values (map [n -> item x_flow n ] flow_input_csv)  ; Read in all the x positions
  let y_values (map [n -> item y_flow n ] flow_input_csv)  ; Read in all the y positions
  let x_max max x_values                                   ; Get the max x value
  let x_min min x_values                                   ; Get the min x value
  let y_max max y_values                                   ; Get the max y value

  ; Resize the world and patchs
  resize-world (x_min / (resolution)) (x_max / (resolution)) 0 (y_max / (resolution))
  ; Set the patch size to be 1 or larger (if reasonable)
  set-patch-size max (list 1 ((resolution / y_max) * resolution_factor))

end

;; Set the world values that are the same for every cell
to set_temp_turbidity_flow_photoperiod

  ; Get the temperature, turbidity, and photoperiod values from the daily input
  set daily_temp_values but-first (map [n -> item temp_column n ] daily_input_csv)
  set daily_turbidity_values  but-first (map [n -> item turbidity_column n ] daily_input_csv)
  set daily_photoperiod_values but-first (map [n -> item photoperiod_column n ] daily_input_csv)

  ; Get the flow values from the daily input
  set daily_flow_values but-first (map [n -> item flow_column n ] daily_input_csv)  ; Read in all the x positions
  let flow_header item 0 flow_input_csv

  ; Get the first row of the CSV
  let flow_columns filter [ s -> member? "mean.D" s ] flow_header    ; Get only column headders with mean in the name
  let flow_values_str map [ s -> remove "mean.D" s ] flow_columns    ; Take out the prefix
  set flow_values map [ s -> read-from-string s ] flow_values_str    ; Covnert the strings to number values
  set flow_values insert-item 0 flow_values 0.0  ; Add zero to the start

end

;; Set up the number of fish that get added to the model every day
to set_daily_fish_counts

  set fish_date_values but-first (map [n -> item fish_date_column n ] daily_fish_csv)
  ; Get all things in the fish input file except date
  ; This is to make a table later on
  set fish_wo_dates but-first (map [n -> but-first n] daily_fish_csv)
  ; Combine the last to to make a paired list then a table
  set paired_fish_list (map [ [ a b ] -> ( list a b ) ] fish_date_values fish_wo_dates)
  set paired_fish_table table:from-list paired_fish_list
  set daily_number_values but-first (map [n -> item number_column n ] daily_fish_csv)
  set daily_species_values but-first (map [n -> item species_column n ] daily_fish_csv)
  set daily_ls_values but-first (map [n -> item lifestage_column n ] daily_fish_csv)
  set daily_length_values but-first (map [n -> item length_column n ] daily_fish_csv)
  set daily_sd_values but-first (map [n -> item length_sd_column n ] daily_fish_csv)
  set fish_formated_dates (map [n -> (time:create-with-format n "MM/dd/yyy")] fish_date_values)

end

;; Set up the parameter values for the fish
to set_fish_parameters

  let paired_param_table make_table_from_csv fish_params_csv

  set fish_benthic (table:get paired_param_table "fish_benthic")

  set fish_cmax_A (table:get paired_param_table "fish_cmax_A")
  set fish_cmax_B (table:get paired_param_table "fish_cmax_B")
  set fish_cmax_C (table:get paired_param_table "fish_cmax_C")
  set fish_cmax_D (table:get paired_param_table "fish_cmax_D")
  set fish_fr_A (table:get paired_param_table "fish_fr_A")
  set fish_fr_B (table:get paired_param_table "fish_fr_B")
  set fish_react_dist_A (table:get paired_param_table "fish_react_dist_A")
  set fish_react_dist_B (table:get paired_param_table "fish_react_dist_B")
  set fish_turbid_threshold (table:get paired_param_table "fish_turbid_threshold")
  set fish_turbid_min (table:get paired_param_table "fish_turbid_min")
  set fish_turbid_exp (table:get paired_param_table "fish_turbid_exp")
  set fish_energy_density (table:get paired_param_table "fish_energy_density")
  set fish_terr_A (table:get paired_param_table "fish_terr_A")
  set fish_terr_B (table:get paired_param_table "fish_terr_B")
  set fish_weight_A (table:get paired_param_table "fish_weight_A")
  set fish_weight_B (table:get paired_param_table "fish_weight_B")
  set fish_capture_1 (table:get paired_param_table "fish_capture_1")
  set fish_capture_9 (table:get paired_param_table "fish_capture_9")
  set met_int (table:get paired_param_table "met_int")
  set met_lm (table:get paired_param_table "met_lm")
  set met_lt (table:get paired_param_table "met_lt")
  set met_v (table:get paired_param_table "met_v")
  set met_lm_lt (table:get paired_param_table "met_lm_lt")
  set met_lm_v (table:get paired_param_table "met_lm_v")
  set met_sqv (table:get paired_param_table "met_sqv")
  set met_lm_sqv (table:get paired_param_table "met_lm_sqv")
  set met_t (table:get paired_param_table "met_t")
  set met_lm_t (table:get paired_param_table "met_lm_t")
  set fish_cmax_A (table:get paired_param_table "fish_cmax_A")
  set fish_cmax_B (table:get paired_param_table "fish_cmax_B")
  set fish_turbid_threshold (table:get paired_param_table "fish_turbid_threshold")
  set fish_turbid_min (table:get paired_param_table "fish_turbid_min")
  set fish_turbid_exp (table:get paired_param_table "fish_turbid_exp")
  set fish_max_swim_param_A (table:get paired_param_table "fish_max_swim_param_A")
  set fish_max_swim_param_B (table:get paired_param_table "fish_max_swim_param_B")
  set fish_max_swim_param_C (table:get paired_param_table "fish_max_swim_param_C")
  set fish_max_swim_param_D (table:get paired_param_table "fish_max_swim_param_D")
  set fish_max_swim_param_E (table:get paired_param_table "fish_max_swim_param_E")
  set fish_move_dist_A (table:get paired_param_table "fish_move_dist_parameter_A")
  set fish_move_dist_B (table:get paired_param_table "fish_move_dist_parameter_B")
  set mort_high_temp_T1 (table:get paired_param_table "mort_high_temp_T1")
  set mort_high_temp_T9 (table:get paired_param_table "mort_high_temp_T9")
  set mort_condition_S_at_K1 (table:get paired_param_table "mort_condition_S_at_K1")
  set mort_condition_S_at_K9 (table:get paired_param_table "mort_condition_S_at_K9")
  set reach_drift_regen_distance (table:get paired_param_table "reach_drift_regen_distance")
  set shelter_speed_frac (table:get paired_param_table "shelter_speed_frac")
  set smolt_param_P1 (table:get paired_param_table "smolt_param_P1")
  set smolt_param_P9 (table:get paired_param_table "smolt_param_P9")
  set outmigrate_param_P1 (table:get paired_param_table "outmigrate_param_P1")
  set outmigrate_param_P9 (table:get paired_param_table "outmigrate_param_P9")

end

;; set the predation parameters
to set_pred_params
  let paired_param_table make_table_from_csv pred_input_file_csv

  set pred_species (table:get paired_param_table "species")
  set num_pred_species length pred_species

  ; glm for habitat rating
  set int_glm (table:get paired_param_table "intercept_glm")
  set shade_glm (table:get paired_param_table "shade")
  set veg_glm (table:get paired_param_table "veg")
  set wood_glm (table:get paired_param_table "wood")
  set depth_glm (table:get paired_param_table "depth")
  set velocity_glm (table:get paired_param_table "velocity")
  set substrate_glm (table:get paired_param_table "substrate")


  ; params for log-normal distribution
  set meanlog (table:get paired_param_table "meanlog")
  set sdlog (table:get paired_param_table "sdlog")

  ; gape limitation params
  set a_gape (table:get paired_param_table "a")
  set B_gape (table:get paired_param_table "B")

  ; temp vs pred activity params

  set pred_temperature_activity (table:get paired_param_table "temperature_C")
  set int_temp (table:get paired_param_table "intercept_C")

end

;; Setup habitat parameters
to set_habitat_params
  let paired_param_table make_table_from_csv habitat_params_csv

  ; feeding params
  set hab_drift_con item 0 (table:get paired_param_table "hab_drift_con")
  set hab_ben_con item 0 (table:get paired_param_table "hab_bentic_con")
  set hab_ben_ene item 0 (table:get paired_param_table "hab_bentic_ene")
  set hab_drift_ene item 0 (table:get paired_param_table "hab_drift_ene")
  set superind_ratio item 0 (table:get paired_param_table "superind_ratio")

  ; reach params
  set resolution item 0 (table:get paired_param_table "resolution")
  set buffer item 0 (table:get paired_param_table "buffer")
  set pred_per_area item 0 (table:get paired_param_table "pred_per_area")
  set shelter_frac item 0 (table:get paired_param_table "shelter_frac")
  set reaction_distance item 0 (table:get paired_param_table "reaction_distance")
  set pred_success item 0 (table:get paired_param_table "pred_success")

  ; pct cover to distance to cover conversion
  set int_pct_cover item 0 (table:get paired_param_table "int_pct_cover")
  set sqrt_pct_cover item 0 (table:get paired_param_table "sqrt_pct_cover")
  set pct_cover item 0 (table:get paired_param_table "pct_cover")
  set sqrt_pct_cover_x_pct_cover item 0 (table:get paired_param_table "sqrt_pct_cover_x_pct_cover")

  ; dis to cover to prey safety conversion
  set dis_to_cover_par item 0 (table:get paired_param_table "dis_to_cover_m")
  set dis_to_cover_int item 0 (table:get paired_param_table "dis_to_cover_int")

  ; turbidity survival bonus params
  set turb_int item 0 (table:get paired_param_table "turbidity_int")
  set turb_slope item 0 (table:get paired_param_table "turbidity_slope")

  ; benthic velocity reduction parameters
  set ben_vel_height item 0 (table:get paired_param_table "ben_vel_height")
  set d84_size item 0 (table:get paired_param_table "d84_size")

end

;; Make a tabel for the CSV fish params inputs
to-report make_table_from_csv [#csv_file]
  let parameter_names (map [n -> item 0 n ] #csv_file)
  let parameter_values  (map [n -> but-first n] #csv_file)
  ; Combine the last to to make a paired list then a table
  let paired_param_list (map [ [ a b ] -> ( list a b ) ] parameter_names parameter_values)
  report table:from-list paired_param_list
end

;; Assign each of the cells all its flow values
to set_patch_flow_values

  ;; Assign all the patches list of depths to have -9999 to rep no data
  ask patches [
    set depths (map [n -> 0] flow_values)
    set velocities (map [n -> 0] flow_values)
    set wetted_fractions (map [n -> 0] flow_values)
  ]
  ;; Populate each patch with depth data
  ;; Take each line in the csv
  foreach (but-first flow_input_csv) [n ->
    ; Move over each patch using its location and value
    ask patch ((item x_flow n) / (resolution)) ((item y_flow n) / (resolution)) [
      ; Set its x and y position, so that a map can later be made in R
      set x_pos (item x_flow n)
      set y_pos (item y_flow n)
      ; Set the area value
      set area (item area_column n)  ; area is in m2
      let counter 0
      ; Assign all the flow values and an index for them
      foreach but-first flow_values [m ->
        ; Use the flow values combined with the data type to get the depth values
        let m_column (position (word "mean.D" m) (item 0 flow_input_csv)) ; Gives the row (position) of meanD1,2,3... of the first item of flow_input
        let depth_input (item m_column n)
        set depths (replace-item counter depths max list depth_input 0) ; Replace the depth place holder with the actual or depth zero if - depth
        set max_depth max (list depth_input max_depth)       ; Check to see if this is a new max depth
        ; Use the flow values combined with the data type to get the velocity values
        let p_column (position (word "mean.V" m) (item 0 flow_input_csv))
        let velocity_input (item p_column n)
        set velocities (replace-item counter velocities max list velocity_input 0) ; Replace the depth place holder with the actual or depth zero if - depth
        set max_velocity max (list velocity_input max_velocity)       ; Check to see if this is a new max depth
        ; Use the flow values combined with the data type to get the wetted fraction
        let q_column (position (word "wetd.D" m) (item 0 flow_input_csv))
        let wetted_input (item q_column n)
        set wetted_fractions (replace-item counter wetted_fractions max list wetted_input 0) ; replace the depth place holder with the actual or depth zero if - depth
        set counter counter + 1]
        set depths insert-item 0 depths 0.0  ; Add zero to the start
        set velocities insert-item 0 velocities 0.0  ; Add zero to the start
    ]
  ]

  set today_index floor (length flow_values / 2)     ; "floor" finds largest number less than or equal to the length of "flow_values/2"; Set the index to a starting value

end

;; Assign each of the cells all its shade values
to set_patch_shade_values

  set month_values but-first (map [n -> item month_column n ] daily_input_csv)  ; Read in all the x positions
  ;; Assign all the patches list of depths to have -9999 to rep no data
  ask patches [
    set shades (map [n -> 0] (range 1 13))]
  ;; Populate each patch with depth data
  ;; Take each line in the csv
  foreach (but-first shape_input_csv) [n ->
    ; Move over each patch using its location and value
    ask patch ((item x_shape n) / (resolution)) ((item y_shape n) / (resolution)) [
      ; Set the area value
      let counter 0
      ; Assign all the flow values and an index for them
      foreach (range 1 13) [m ->
        ; Use the flow values combined with the data type to get the depth values
        let m_column (position (word "shade_" m) (item 0 shape_input_csv))
        let shade_input (item m_column n)
        set shades (replace-item counter shades max list shade_input 0) ; Replace the depth place holder with the actual or depth zero if - depth
        set counter counter + 1]
    ]
  ]

end

;; Assign each of the cells all of the values associated with shapefiles (also in this section, the lists for mortality count and visit counts are initialized), and set whether they have a migrant in them to 'false'
to set_patch_shape_values

  ;; Now do the same but for the shapes
  foreach (but-first shape_input_csv) [n ->
    ask patch ((item x_shape n) / (resolution)) ((item y_shape n) / (resolution)) [
      set wood (item wood_column n) ; Used for relief for predation
      set gravel (item gravel_column n)
      set veg (item veg_column n) ; Used for cover relief for predation
      set fine (item fine_column n)
      set rock (item rock_column n)
      set cobble (item cobble_column n)
      set ben_food_fra (item ben_food_column n)
      ]
  ]

end

;; To set substrate values to patches for predators
to set_substrate_values

    ask patches [
      set substrate (gravel + cobble + rock)
      ifelse (substrate >= fine and substrate > 0)
        [set substrate 1]
        [set substrate 0]
  ]
end

;; Assign cover values to patches
to set_cover_values

  ask patches [
    set cover veg + wood
  ]

end

;; Does part of the habitat rating calculation for variables that do not change during the time steps
to set_partial_hab_rating

  ask patches [
    set partial_hab_rating []
    foreach range length pred_species [n ->
      set partial_hab_rating lput (item n int_glm + item n wood_glm * wood + item n veg_glm * veg + item n substrate_glm * substrate) partial_hab_rating
    ]
  ]

end

;; Bulid the logistic function
to build_logistic_functions
  ; An observer procedure to create logistic functions used in survival and other fish functions.
  ; Logistic functions are stored in a table; they can differ among species.
  ; Create the table.
  set fish_logistics_table table:make

  ; Build a table of functions for each species.
  foreach species_list [ next_species ->
   let the_species_table table:make
   let the_spp_index position next_species species_list

   ; Poor condition
   create_logistic_with_table_and_params the_species_table
     "poor_condition" (item the_spp_index mort_condition_S_at_K1) 0.1 (item the_spp_index mort_condition_S_at_K9) 0.9

   ; Capture success for drift feeding
   create_logistic_with_table_and_params the_species_table
     "capture_success" (item the_spp_index fish_capture_1) 0.1 (item the_spp_index fish_capture_9) 0.9

   ; High temperature
   create_logistic_with_table_and_params the_species_table
     "high_temperature" (item the_spp_index mort_high_temp_T1) 0.1 (item the_spp_index mort_high_temp_T9) 0.9

   ; Probability of being in the "smolting window" depending on photoperiod
   create_logistic_with_table_and_params the_species_table
     "smolt_photoperiod" (item the_spp_index smolt_param_P1) 0.1 (item the_spp_index smolt_param_P9) 0.9

   ; Probability of outmigrating given the percent increase in mean velocity of the search radius from the mean running average velocity of search radius throughout the last 5 days
   create_logistic_with_table_and_params the_species_table
     "outmigrate_velocity" (item the_spp_index outmigrate_param_P1) 0.1 (item the_spp_index outmigrate_param_P9) 0.9

   table:put fish_logistics_table next_species the_species_table

  ]

end

;; Make the logistic table
to create_logistic_with_table_and_params [a_table a_name x1 p1 x2 p2]
  ; An observer reporter to initialize a logistic function.
  ; p1 and p2 are the probabilities associated with inputs x1 and x2

  ; For convenience in test output, make sure X1 is less than X2
  if x1 > x2
  [
    let a_num x2
    set x2 x1
    set x1 a_num
    set a_num p2
    set p2 p1
    set p1 a_num
  ]

  ; Calculate the internal parameters using method on p. 216 of Railsback & Grimm 2012
  let C ln (p1 / (1 - p1))
  let D ln (p2 / (1 - p2))
  let logistic_b (C - D) / (x1 - x2)
  let logistic_a C - (logistic_b * x1)

  let a_subtable table:make
  table:put a_subtable "logistic_b" logistic_b
  table:put a_subtable "logistic_a" logistic_a

  table:put a_table a_name a_subtable

end

;; Calculate the turbidity bonous of each cell
to-report set_turb_bonus [#turb_int #turb_slope #turbidity]
  let exponent -1 * (#turb_int + #turb_slope * #turbidity)
  report 1 / (1 + exp exponent )
end

;##### Run Actions #######################################################
to go

  ;profiler:start

  tick
  if ticks = 1 [ reset-timer ]

  if time:is-after? tick_date last_day = TRUE
  [
    show (word "Simulation finished in " timer " seconds.")

    update_output

    stop
  ]

  print tick_date
  clear-drawing

	update_habitat
  set_shade
  set_dist_to_cover_values
  set_cover_bonus
  set_survival_prob
  place_predators
  set_boundaries
  color_patches
  hatch_fish

  foreach sort-on [-1 * f_length] turtles
  [next_fish -> ask next_fish
    [

  ; Each fish completes the following procedures in order of longest to shortest
  update_fish
  find_potential_destination_cells
  calculate_smolting_probability

 ; If a juvenile has smolted, the outmigration probability is calculated
  if is_smolt = true [
        calculate_outmigration_probability
     ; If the smolt decides to migrate, it does so
     if is_migrant = true [migrate]
      ]

  if (is_smolt = true and is_migrant = false) or (is_smolt = false) [
 ; If a smolt or non-smolt decides not to migrate, it performs the following procedures in order
  calculate_starvation_prob
  calculate_food_availability
  calculate_mortality_risk
  select_destination_cell

        if is_drifter = true ; the fish checks whether it will have any positive net energy in any of the search radius cells. If not, it can move to another farther area
        [drift_downstream
        calculate_food_availability
        calculate_mortality_risk
        select_destination_cell
        ]
  grow
  deplete_destination_resources
  survive
      ]
    ]
  ]

  save_destination_cell_info
  save_detailed_population_info

  stop_program

end

;; Updates variables that change daily (flow, depth, velocity, temperature, turbidity, food predator-related variables  in each cell)
to update_habitat

  ; Set today's reach temperature
  set temperature (item ticks daily_temp_values)

  ; Set today's reach turbidity
  set turbidity (item ticks daily_turbidity_values)

  ; Set today's photoperiod
  set photoperiod ((item ticks daily_photoperiod_values)) * 24  ; photoperiod in hours

  ; Caclulate and set today's flow values for the reach (depth and velocity)
  set flow (item ticks daily_flow_values)
  if flow > max flow_values [
  ; Check if it is outside the range of flows
    user-message "The daily flow value is greater than the raster input files.\nThe program will halt."]
  if flow <= 0 [
  ; Check if it is outside the range of flows
    user-message "There is no flow in the system.\nAll your fish are dead.\nThe program will halt."]
  let high_flow min filter [i -> i >= flow] flow_values     ; Find the closest flow in flow_values which is greater than or equal to the current daily value
  let low_flow  max filter [i -> i < flow]  flow_values     ; Find the closest flow in flow_values which is less than the current daily value
  let flow_fraction (flow - low_flow) / (high_flow - low_flow)  ; Calculate the fraction of distance the flow is form the low flow
  set today_index position low_flow flow_values

  ; Color the patches based on user selection
  ask patches [
    set pcolor black
    set yesterday_depth today_depth
    let low_depth  (item today_index depths)
    let high_depth (item (today_index + 1) depths)
    set today_depth (low_depth * (1 - flow_fraction) + high_depth * (flow_fraction))    ; Depth is in meters
    if today_depth <= 0 [set today_depth 0]
    ; Do the same for velotity
    set yesterday_velocity today_velocity
    let low_velocity  (item today_index velocities)
    let high_velocity (item (today_index + 1) velocities)
    set today_velocity (low_velocity * (1 - flow_fraction) + high_velocity * (flow_fraction))       ; Velocity is in m/s
    ; Do the same for wetted fraction
    let low_wetted  (item today_index wetted_fractions)
    let high_wetted (item (today_index + 1) wetted_fractions)
    set wetted_fraction (low_wetted * (1 - flow_fraction) + high_wetted * (flow_fraction))
    set wetted_area area * wetted_fraction
    ; if today_velocity <= 0 [set today_velocity 0]
  ]
    ; Ask patches to update their depleted variables (velocity shelter availability, available hiding places, and drift)
  ask patches
    [
      set frac_velocity_shelter veg + wood  ; the fraction of the cell that has velocity shelter is the sum of the fraction covered by vegetation and wood
      set cell_available_vel_shelter (wetted_area * frac_velocity_shelter)   ; available velocity shelter per cell (m^2)
      set cell_available_wet_area wetted_area   ; available velocity shelter per cell (m^2)
      set cell_available_ben  wetted_area * hab_ben_con * ben_food_fra ; available drift food in g/m3

     ]

  ; Reset global variables written to detailed population output files every time step
  set daily_juvenile_count (list)
  set daily_dead_fish_count (list)
  set daily_migrant_count (list)
  set daily_smolt_count (list)
  set daily_drifter_count (list)

  set total_daily_juveniles 0.0
  set total_daily_migrants 0.0
  set total_daily_smolts 0.0
  set total_daily_dead_fish 0.0
  set total_daily_drifters 0.0

  ; Update intermediate variables that depend on temperature
  set turb_survival_bonus set_turb_bonus turb_int turb_slope turbidity

  ; Update the temperature cmax fuinction for each fish species
  foreach species_list [ next_species ->
   let index (position next_species species_list)
   let c_par item index (fish_cmax_C)
   let d_par item index (fish_cmax_d)
   ; Use the beta sigmoid function for cmax temp
   set cmax_temp_func replace-item index cmax_temp_func ((1 + (c_par - temperature) / (c_par - d_par)) * (temperature / c_par) ^ (c_par / (c_par - d_par)))
  ]
  ;print cmax_temp_func

end

;; Set the shade value of each cell
to set_shade

  set month (item ticks month_values)                   ; get todays month value
  ask patches [
    set pcolor black
    set shade item (month - 1) shades
  ]

end

;; Calculate distance to cover based on percent cover
to set_dist_to_cover_values

  ask patches [
    let patch_width sqrt wetted_area
    set distance_to_cover (int_pct_cover + sqrt_pct_cover * (sqrt cover) + pct_cover * cover + sqrt_pct_cover_x_pct_cover * (cover ^ 1.5)) * patch_width
  ]

end

;; Calculate the bonus to prey fish from cover in a patch
to set_cover_bonus

  ask patches [
    ifelse cover > 0
    [
      let exponent -1 * (dis_to_cover_int + dis_to_cover_par * distance_to_cover)
      set cover_bonus 1 / (1 + exp exponent)
    ]
    [
      set cover_bonus 0
    ]
  ]

end

;; Calculate the probability of prey survivng a predator encounter based on amount of cover, turbidity, and pred success rate
to set_survival_prob

  ask patches [
    set survival_prob 1 - pred_success + (pred_success * (1 - ((1 - turb_survival_bonus) * (1 - cover_bonus))))
  ]

end

;; Determine the number of predators in each patch
to place_predators
  ; calculate the total number of predators in the reach on that time tick

  let total_wetted_area (sum [wetted_area] of patches)
  set total_preds round (total_wetted_area * pred_per_area )

  ;; Calculate habitat ratings for each patch and predator species

  ask patches [

    ; only do calculations if there is water in a patch
    ifelse (wetted_area > 0)
    [
      ; set an empty list to save habitat ratings for each species
      set hab_rating []

      set adj_hab_rating []

      ; a binary version of shade used by the model
      let shade_binary shade
      ifelse (shade_binary >= 0.5) [set shade_binary 1] [set shade_binary 0]

      ; calculate the habitat rating for each predator species
      foreach (range num_pred_species)
      [ n ->
        let exponent -1 * (item n partial_hab_rating + item n shade_glm * shade_binary + item n depth_glm * today_depth + item n velocity_glm * today_velocity)
        let model_prediction 1 / (1 + exp exponent)
        ; values < 0.5 indicate predictions of predator absence, so vals are set to 0
        if (model_prediction < 0.5) [set model_prediction 0]
        set hab_rating lput model_prediction hab_rating
        set adj_hab_rating lput (model_prediction * wetted_area) adj_hab_rating

      ]
    ]
    [
      ; set hab rating to 0 for each predator if there is no water in the patch
      set hab_rating n-values num_pred_species [0]
      set adj_hab_rating n-values num_pred_species [0]

    ]
  ]

  ;; End hab rating calcs

  ;; Calculate number of predators of each species and predator length

  ; Calculate the total hab rating across all patches for each predator
  ; used for calculating the number of predators in each cell
  let total_hab_rating []
  let all_wetted_vals ([wetted_area] of patches)
  ; calculate the sum of wetted_area * hab_rating for each species across all patches
  ; used for weighting hab_rating values to predict the number of predators
  foreach (range num_pred_species) [n ->
    ; restructures the habitat ratings into a list of lists in which each sublist contains all values for a particular species
    ; initially, the sublists contain values for all species in each patch rather than all values for each species across all patches
    let hab_list (map [x -> item n x] ([adj_hab_rating] of patches))
    set total_hab_rating lput (sum hab_list) total_hab_rating

  ]


  ask patches [

    ifelse (wetted_area > 0)
    [
      set num_preds []
      set pred_length []
      foreach (range num_pred_species) [n ->
        ; use the total preds in the system, patch habitat rating, and wetted area to calculate actual predator numbers
        let temp_pred_num round (item n adj_hab_rating / item n total_hab_rating * total_preds)

        ; draw predator lengths from a log-normal distribution
        ;let mu item 0 butfirst item n butfirst pred_length_dist_params
       ; let sigma item 1 butfirst item n butfirst pred_length_dist_params
        ; filter predators that are above 150 mm
        let pred_length_list filter [i -> i >= 150] (n-values temp_pred_num [x -> log-normal item n meanlog item n sdlog ])

        ifelse (length pred_length_list > 0)
        [
          set pred_length lput pred_length_list pred_length
          set num_preds lput length pred_length_list num_preds
        ]
        [
          set pred_length lput 0 pred_length
          set num_preds lput 0 num_preds
        ]
      ]
    ]
    ; set pred_length and num_preds to 0's for each predator species if the patch has no water
    [
      set pred_length n-values num_pred_species [0]
      set num_preds n-values num_pred_species [0]
    ]

    ; combine the list-of-lists for pred_length into a single list
    set pred_length filter [i -> i > 0] (flatten pred_length)
    ; calculate pred_length as the average of all predator lengths across all species
    if length pred_length > 0
    [
      ; set the patch's pred_length value to a random length from the list
      set pred_length item (random length pred_length) pred_length
      ; Need to change to allow different values of a_gape and B_gape if we want different values for each pred species
      set max_prey_length get-max-prey-length item 0 a_gape item 0 B_gape pred_length
    ]

    ;; End of pred counts and length calculations

    ;; Calculate proportional patch area occupied by predators

    ifelse wetted_area > 0
    [
      set encounter_prob []
      foreach (range num_pred_species) [n ->

        ; calculate the effect of temperature on predator activity
        let exponent -1 * (item n int_temp + temperature * item n pred_temperature_activity)
        let pred_temperature_effect 1 / (1 + exp exponent)
        ; encounter_prob is simply the area occupied by predators divided by the wetted area of the patch
        set encounter_prob lput (((item n num_preds) * (reaction_distance ^ 2) * pi) * pred_temperature_effect / wetted_area) encounter_prob
      ]
      ; probability of encounter across all predator species
      set encounter_prob sum encounter_prob
    ]
    [set encounter_prob 0]
  ]

end

;; Allows summing of specific elements from each list in a list of lists
to-report sum_item [#pos #listoflists ]
  let items map [ x -> item #pos x ] #listoflists
  ;report reduce [ [a b] -> a + b] items
  report items
end

;; Draw from log-normal distribution
to-report  log-normal [#mu #sigma]
  report exp random-normal #mu #sigma
end

;; Flatten a list of lists
to-report flatten [ #list ]
  set #list fput 0 #list ;; and a sacrificial scalar to the front of the list
  set #list ( reduce [ [ a b ] -> ( sentence a b) ] #list )
  report but-first #list ;; remove the sacrificial scalar before reporting
end

;; Pairs up outcomes with their probabilities
to-report make-prob-list [#survival_prob]
  let prob_eaten 1 - #survival_prob
  let outcomes [1 0]
  let probs list prob_eaten #survival_prob
  report (map list outcomes probs)
end

;; Draws an outcome weighted by probability
to-report check-outcome [#prob_pairs]

  report first rnd:weighted-one-of-list #prob_pairs [ [p] -> last p ]
end

;; Calculate the maximum prey lenght of a predator
to-report get-max-prey-length [#a #B #pred_length]
  report exp (#a + #B * (#pred_length ^ 2))
end

;; Set the color of the cells/patches based on user input (right now colored by depth)
to color_patches

  set max_cell_available_vel_shelter [cell_available_vel_shelter] of max-one-of patches [cell_available_vel_shelter] ; this is very ugly code (only used to color patches by max cell available shelter)

   ask patches [
    if today_velocity <= 0 [set today_velocity 0]
    (ifelse
      background_display = "depth" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "Blues" 9 today_depth 0 (max_depth)]
      ]
      background_display = "velocity" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Divergent" "Spectral" 9 today_velocity 0 (max_velocity)]
      ]
      background_display = "wood" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "YlGnBu" 9 wood 0 1]
      ]
      background_display = "veg" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "PuRd" 9 veg 0 1]
      ]
      background_display = "shade" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "BuGn" 9 shade 0 1]
      ]
      background_display = "wetted fraction" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "Blues" 9 wetted_fraction 0 1]
      ]
      background_display = "predator habitat rating" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "YlGnBu" 9 sum hab_rating 0 1]
      ]
      background_display = "available velocity shelter" [
        if (today_depth > 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "YlGnBu" 9 cell_available_vel_shelter 0 (max_cell_available_vel_shelter) ]

      ]
    )
  ]

end

;; This will set some boundaries of the reach based on flow
to set_boundaries

  set wet_patches patches with [today_depth > 0]         ; Select patches with water
  set reach_start max [pycor] of wet_patches             ; Get the highest patch distance value
  set reach_end min [pycor] of wet_patches               ; Get the lowest patch distance value
  set top_patches wet_patches with [pycor = reach_start] ; Get the highest patchs that are wet

end

;; Add new fish to the reach (currently, only chinook juveniles are added, for testing)
to hatch_fish

loop
  [
  let todays_date time:show tick_date "MM/dd/yyyy"
  let todays_fish table:get-or-default paired_fish_table todays_date -9999
  table:remove paired_fish_table todays_date
  if todays_fish = -9999 [stop]

  ; Add in fish based on life stage
  if item (lifestage_column - 1) todays_fish = "adult" [
    create-adults (item (number_column - 1) todays_fish )[
      set species (item (species_column - 1) todays_fish )
      set species_id (position species species_list)

      set f_length (item (length_column - 1) todays_fish ) ; fish length is in cm
      set mass (item species_id (fish_weight_A)) * (f_length ^ (item species_id (fish_weight_B))) ; mass is in g
      set fish_condition 1.0

      set territory_size (item species_id fish_terr_A) * f_length ^ (item species_id fish_terr_B)

      set size 2
      set shape "fish"
      set destination one-of wet_patches with [today_velocity < 100]  ; Place the turtle in a wet patch
      move-to destination
      set color red
      set exit_status 0

     ; Memory lists
      set velocity_experience_list (list) ; A list of destination cell velocity fish experience each day

      set is_in_shelter false        ; A boolean for whether trout is drift-feeding in velocity shelter
      set is_alive true

      ;save_event "initialized"
    ]
  ]

  if item (lifestage_column - 1) todays_fish = "juvenile" [
    create-juveniles (item (number_column - 1) todays_fish )[
      set species (item (species_column - 1) todays_fish )
      set species_id (position species species_list)

      set f_length (item (length_column - 1) todays_fish ) ; fish length is in cm
      set mass (item species_id (fish_weight_A)) * (f_length ^ (item species_id (fish_weight_B)))
      ;print "mass"
      ;show mass
      set fish_condition 1.0

      set territory_size (item species_id fish_terr_A) * f_length ^ (item species_id fish_terr_B)

      set size 2
      set shape "fish"
      set destination one-of wet_patches with [today_velocity < .5]  ; Place the turtle in a wet patch with velocity less than 5 m2 (this can be changed)
      move-to destination
      set color green
      set exit_status 0

    ; Memory lists
      set velocity_experience_list (list) ; A list of destination cell velocity fish experience each day

      set is_in_shelter false          ; A boolean for whether trout is drift-feeding in velocity shelter.
      set is_smolt false               ; A boolean for whether fish has smolted.
      set is_migrant false             ; A boolean for whether fish has migrated
      set is_alive true                ; A boolean for whether fish is alive
      set is_drifter false             ; A boolean for whether fish is drifting downstream due to crappy habitat

      ;save_event "initialized"
    ]
  ]
  ]

end

;; Update the fish variables that change every time step (turbidity functions, etc)
to update_fish

  ; Set the drifter fish color back to red from blue
  ; set color red

  ; Re-set the fish in shelter variable to false
  set is_in_shelter false

  if mass <= 0 [set mass .001] ; If mass is negative, set it to a very small number to calculate log mass in metabolic rate equation

  ; Cmax weight term
  set fish_cmax_wt_term (item species_id fish_cmax_A) * (mass ^ (1 + item species_id fish_cmax_B))
  ; Calculate cmax (g/d)
  set cmax fish_cmax_wt_term * item species_id cmax_temp_func

  ; Update the turbidity function
  ifelse turbidity <= item species_id fish_turbid_threshold [
   set fish_turbid_function 1.0
 ][
   set fish_turbid_function (item species_id fish_turbid_min) + (1.0 - (item species_id fish_turbid_min)) *
    exp ((item species_id fish_turbid_exp) * (turbidity - (item species_id fish_turbid_threshold)))
    ;show fish_turbid_function
 ]

  ; set the distance that fish can detect/react to prey
   set fish_detect_dist ((item species_id fish_react_dist_A) + (item species_id fish_react_dist_B * f_length)) * fish_turbid_function ; meters
   ;show fish_detect_dist

   ; Calculate max swim speed for a fish in m/s
   set max_swim_speed ((item species_id fish_max_swim_param_A * f_length) + item species_id fish_max_swim_param_B) * ((item species_id fish_max_swim_param_C * temperature ^ 2) + item species_id fish_max_swim_param_D * temperature) + item species_id fish_max_swim_param_E

end

;; Calculate the probability of a juvenile being in the smolt "window" (depending on photoperiod) and thus becoming a smolt. The juveniles must also meet or surpass a size threshold of 7 cm.
to calculate_smolting_probability

  ; Only evaluate probability of smolting if the individual hasn't smolted
  if is_smolt = false [

  set fish_smolting_prob (evaluate_logistic "smolt_photoperiod" species photoperiod)

  if ((random-float 1.0) < fish_smolting_prob) and f_length >= 7 [
    set is_smolt true
    save_event "smolted" ; only save the smolted event if its the fish's first time step smolting
    set daily_smolt_count lput 1 daily_smolt_count
  ]
]

end

;; Calculate the probability of a smolt migrating, dependent on changes in velocity
to calculate_outmigration_probability

 ; If the velocity experience list has less than 5 items, we take the average of those values
  ifelse length velocity_experience_list <= 5 [
    set running_average_velocity  mean velocity_experience_list
  ][
  ; Calculate the running average of the mean velocity that the fish has experienced in its radius the last 5 time steps
    let running_average_velocity_sublist  sublist velocity_experience_list (length velocity_experience_list - 6) (length velocity_experience_list - 1)
    set running_average_velocity mean running_average_velocity_sublist
  ]

  ; Calculate the percent change from the running average to the current radius velocity
  let percent_change_velocity ( mean_velocity_in_radius - running_average_velocity) / running_average_velocity

  ; If the change is negative (velocity decreased) then we set it to zero so the outmigration probability is 0
  if percent_change_velocity < 0 [set percent_change_velocity 0]

  ; The probability of outmigrating increases as the difference in velocity between the current radius and the running average increases.
  set fish_outmigration_prob (evaluate_logistic "outmigrate_velocity" species percent_change_velocity)

  if ((random-float 1.0) < fish_outmigration_prob) [ set is_migrant true
  set daily_migrant_count lput 1 daily_migrant_count
  ]

  ; If the fish reaches a max size for outmigration, it also outmigrates regardless of changes in velocity. This is so that fish dont stay in the river forever.


end

;; A reporter to evalulate the logistic function
to-report evaluate_logistic [ a_logistic_name a_species an_input ]
  ; An observer reporter to report the value of a survival logistic

  let the_table (table:get (table:get fish_logistics_table a_species) a_logistic_name)
  let Z (table:get the_table "logistic_a") + ((table:get the_table "logistic_b") * an_input)
  ; Defensive programming to avoid over/underflow runtime errors
  if Z < -200 [ report 0.0 ]
  if Z >   35 [ report 1.0 ]

  report (exp Z) / (1.0 + exp Z)   ; Calculating exp(Z) twice does not slow execution

end

;; Migrate to new cell
to migrate

 let bottom_patches wet_patches with [pycor = reach_end] ; Get the lowest patches in the reach that are wet

 set destination one-of bottom_patches ; Set the migrant's destination patch to one of these bottom patches

 ask destination [set migrant_patch true] ; Set the destination's to identify itself as a migrant patch (this is so we don't include these cells as destination cells for the output files)

 move-to destination

 save_event "migrated"

 set migrant_count_list lput 1 migrant_count_list ; add the migrant to the migrant count list

 set migrant_length_list lput f_length migrant_length_list ; add the migrant length to the migrant length list

 set migrant_mass_list lput mass migrant_mass_list ; add the migrant mass to the migrant mass list

 set migrant_condition_list lput fish_condition migrant_condition_list ; add the migrant condition to the migrant condition list

 die ; remove migrant from system

end

;; Find all of the potential destination cells that the fish can choose to move to
to find_potential_destination_cells

  ; If a fish is smaller than 7 cm, it's radius is not even one cell, so we set it to at least one cell by giving them a fake length of 7
  ifelse f_length < 7 [
     let temp_fish_length 7
     set patch_radius (((item species_id fish_move_dist_A) * (temp_fish_length ^ (item species_id fish_move_dist_B))) / 100) / resolution
   ][
     set patch_radius (((item species_id fish_move_dist_A) * (f_length ^ (item species_id fish_move_dist_B))) / 100) / resolution   ; Number of patches within the maximum distance that a fish can travel. Essentially the "radius" in cm converted to meters and divided by the resolution
   ]

  ; Find all of the reachable cells within the radius
  set wet_cells_in_radius find_possible_destinations self patch_radius

  ;print "wet cells in radius"
  ;show wet_cells_in_radius

  ; Calculate the mean water velocity in the radius
  set mean_velocity_in_radius mean [today_velocity] of wet_cells_in_radius

   ; If it is the first time step, the first value in the velocity experience list is the mean velocity of the radius
  set velocity_experience_list lput mean_velocity_in_radius velocity_experience_list


end

;; Calculate the strvation probablity
to calculate_starvation_prob
; Calculate the probability of surviving starvation based on K. This survival does not depend on the cell. It implements the linear condition mortality model.
; The equation is algebraically equivalent to a line with survival = mort-condition-S-at-K5 at K = 0.5 and survival = 1.0 at K = 1.0

  set fish_death_starv_survival_prob (evaluate_logistic "poor_condition" species fish_condition)

  end

;; Calculates the energy benefits of each cell in the search radius
to calculate_food_availability

  ask wet_cells_in_radius  [

  let fish_species [species] of myself

  ; Calculate the swim speed (m/s) of the fish in each cell if it were to drift feed (dependent on whether there are velocity shelters)


  ; Choose between benthic and and drift swimming
  ifelse (item species_id fish_benthic) = 0 [
      ifelse (cell_available_vel_shelter) > ([territory_size] of myself) [ ; If there are velocity shelters in its cell, the speed is reduced
        set swim_speed today_velocity * item species_id shelter_speed_frac  ; Swim speed is in m/s
      ][
        ; Otherwise, the speed is the velocity of the cell
        set swim_speed today_velocity ; Swim speed is in m/s
      ]
    ][
      ; The fish is benthic so experiences a reduced velocity
      ; 0.07 is a constant representing between 5 and 10%
      ; 0.41 is Von Karman constant
      ; 30 and 3.5 are also fitted constants
      set swim_speed (0.07 * today_velocity / 0.41 * ln (ben_vel_height / d84_size * 30 / 3.5))
    ]

   ; Choose between benthic and and drift feeding
   ifelse (item species_id fish_benthic) = 0 [
      ; Calculate the capture area of the fish
      set capture_area (2 * fish_detect_dist) * min (list fish_detect_dist today_depth) ; m^2

      ; Set up logistic equation to determine capture success
      set ratio_vel_max_swim today_velocity / [max_swim_speed] of myself
      set capture_success (evaluate_logistic "capture_success" fish_species ratio_vel_max_swim)

      ; Calculate daily intake as a daily rate, g/d
      set daily_intake capture_area * hab_drift_con * today_velocity * capture_success * 86400
    ]
    [
      set daily_intake (([mass] of myself) * item species_id fish_fr_A * hab_ben_con) / (1 + item species_id fish_fr_B * hab_ben_con)
    ]

  ; Convert swim speed from m/s to bodylength/s in order to use it in the metabolic rate equation below
  let converted_swim_speed ( swim_speed * 100 ) / [f_length] of myself

  ; Calculate the log metabolic rate (J/day) - equation is for chinook
  let log_total_metab_rate item species_id met_int + item species_id met_lm * log [mass] of myself 10 + item species_id met_t
  * temperature - item species_id met_sqv * sqrt (converted_swim_speed) + item species_id met_lm_sqv * log [mass] of myself 10 * sqrt (converted_swim_speed) + item species_id met_lm_t * log [mass] of myself 10 * temperature

  set total_metab_rate exp log_total_metab_rate

  ; Use cmax to limit intake
  if daily_intake > [cmax] of myself [ set daily_intake [cmax] of myself ]

  ifelse (item species_id fish_benthic) = 0 [
      ; Calculate energy obtained each cell (J/d)
      set daily_energy_intake daily_intake * hab_drift_ene
    ]
    [
      ; limit based on avaiable food
      if daily_intake > cell_available_ben [ set daily_intake cell_available_ben ]
      ; Calculate energy obtained each cell (J/d)
      set daily_energy_intake daily_intake * hab_ben_ene
    ]

  ; Calculate net energy intake by subtracting the metabolic rate (J/d)
  set daily_net_energy daily_energy_intake - total_metab_rate

  set total_food_in_cell precision (daily_net_energy) 2 ; "Total food in cell" can be negative since daily net energy can be negative

  ]

end

;; Calculates the mortality risk of each cell in the search radius (previously, was set to all non-starvation risks multiplied)
to calculate_mortality_risk

  ask wet_cells_in_radius [

    let fish_species [species] of myself

    ; Calculate the probability of death from high temp (the logistic calculates probability of surviving the risk, so we subtract from 1)
    set fish_death_hightemp_prob (evaluate_logistic "high_temperature" fish_species temperature)
    set fish_death_hightemp_prob 1 - fish_death_hightemp_prob

    ; Calculate the probability of dying from predation
    set fish_death_aq_pred_prob calc-pred-mortality survival_prob encounter_prob max_prey_length ([f_length] of myself) num_preds

  ]

end

to move_fish [target]
  if draw_fish_movements? [
    ; Draw the path taken to get to the destination.
    let curpatch target
    ; The patches know the path the fish took to this location in reverse, so we
    ; actually start at the destination and draw backwards before jumping back to
    ; the end.
    move-to target
    pen-down
    while [curpatch != nobody]
    [
      move-to curpatch
      set curpatch [previous_patch] of curpatch
    ]
    pen-up
  ]

  ; Move to the destination.
  move-to target

end

;; Cell selection strategy for movement
to select_destination_cell

  ; Each fish performs this in order of longest length to shortest length

  ifelse (random-float 1.0) > fish_death_starv_survival_prob [ ; If the probability of surviving starvation is less than the randomly generated number, the fish selects cells with higher food regardless of risk

    print "select highest food cell because of starvation"
    ifelse all? wet_cells_in_radius [total_food_in_cell < 0] and is_drifter = false    ; if all of the cells in the radius have negative food values, the fish moves elsewhere in the reach to get out of crappy area
    [set is_drifter true
     set daily_drifter_count lput 1 daily_drifter_count
    ][

     set destination max-one-of wet_cells_in_radius [total_food_in_cell] ; If they do not become drifters, they select cell with higher food regardless of risk
     move_fish destination
     ;show destination
     set is_drifter false]

    ][ ; If the probability of surviving starvation is greater than the randomly generated number, the fish selects cells that maximize food to nonstarvation risks ratio

    print "good condition, take into account predation"
    ;ifelse all? wet_cells_in_radius [total_food_in_cell < 0]
;    [set is_drifter true
;     set daily_drifter_count lput 1 daily_drifter_count
;    ][

    ask wet_cells_in_radius [

     ;Fish takes into account the food-risk ratio AND actual path risk (prob of surviving along the path) in selecting a destination:
      set consider_path_risk  total_food_in_cell * path_survival]

      set destination max-one-of wet_cells_in_radius [consider_path_risk]

      move_fish destination
      ;show destination
      set is_drifter false
    ]

  ask destination [set migrant_patch false]  ; The destination cell's migrant_patch status is set to false

  print "drifter?"
  show is_drifter

end

;; Procedure for drifters drifting downstream
to drift_downstream

  ;print "drifting downstream"

  ; Temporarily set the drifter's color to blue
  set color blue

  ; Set the distance a fish can disperse to be equal to 10 times the current radius (arbitrary number to change)
  set dispersal_distance patch_radius * 10

  ; Find all of the wet cells within the new area that the fish can disperse to
  set open_cells_for_dispersal find_possible_destinations self dispersal_distance

  ; The fish will disperse downstream, so select a random downstream cell within the area it can dsperse
  set downstream_random_cell one-of open_cells_for_dispersal with [pycor < [ ycor ] of myself ]

  ifelse downstream_random_cell = nobody [ ; If there is no downstream cell to move to, the drifter exits out of the river

   ;set drifter_count_list lput 1 drifter_count_list ; add the drifter to the drifter count list

   ;set drifter_length_list lput f_length drifter_length_list

   ;set drifter_mass_list lput mass drifter_mass_list

   ;set drifter_condition_list lput fish_condition drifter_condition_list

  die ][

  ; The fish drifts to one of these random cells

  move_fish downstream_random_cell

  ;The "select destination cell" logic repeats, except with "downstream random cell" as the fish's position

  ; Find all of the reachable cells within the radius
  set wet_cells_in_radius find_possible_destinations self patch_radius

  ; Calculate the mean water velocity in the radius
  set mean_velocity_in_radius mean [today_velocity] of wet_cells_in_radius

  ; If it is the first time step, the first value in the velocity experience list is the mean velocity of the radius
  set velocity_experience_list lput mean_velocity_in_radius velocity_experience_list

  ]

end

;; Update the fish size
to grow

  set daily_growth ([daily_net_energy] of destination) / item species_id fish_energy_density

  ifelse daily_growth = 0.0 [ ; if daily growth is 0, set the percent daily growth to a very small number to avoid div by 0 error

  ; This is the daily growth in percent body weight (column in output file)
  set percent_daily_growth 0

  ; This is the fraction that the daily growth is of cmax (column in output file)
  set fraction_growth_cmax 0

  ][

  ; This is the daily growth in percent body weight (column in output file)
  set percent_daily_growth (daily_growth / mass) * 100

  ; This is the fraction that the daily growth is of cmax (column in output file)
  set fraction_growth_cmax daily_growth / cmax
  ;print "frac growth cmax"
  ;show fraction_growth_cmax

  ]

  ; Calculate the fish's new weight
  set new_mass mass + daily_growth
  ;print "new mass"
  ;show new_mass

  ; If the fish's mass is 0, the condition is 0
  if new_mass <= 0.0 [set new_condition 0
  set new_mass 0.0 ]

  set healthy_mass (item species_id fish_weight_A) * (f_length ^ (item species_id fish_weight_B)) ; g
  ;print "healthy mass"
  ;show healthy_mass

  set desired_length (new_mass / (item species_id fish_weight_A)) ^ (1 / (item species_id fish_weight_B)) ; cm
  ;print "desired length"
  ;show desired_length

  ; If the new mass is greater than the healthy mass for its length, we set the new length to the desired length and new condition to 1
  ifelse new_mass > healthy_mass [
   set new_length desired_length
   set new_condition 1.0
  ][ ; Otherwise, we divide the new mass by the healthy mass to get the new fish condition
   set new_length f_length ; The fish keeps its current length (cm)
   set new_condition new_mass / healthy_mass
  ]

  set mass new_mass
  set f_length new_length
  set territory_size (item species_id fish_terr_A) * f_length ^ (item species_id fish_terr_B)
  set fish_condition new_condition
end

;; Fish pathfinding functions

; Clear all of the values used in the pathfinding algorithm
to clear_patch_path_data
  set has_visited? false
  set previous_patch nobody
  set path_to_here_cost -1
  set path_survival -1
  set fish_survival -1
end

;; Calculate the cost to move to a new patch.
to-report calculate_move_cost [from_patch]
  ; assume the cost == the distance
  ; calculate distance instead of using 1 to account for diagonals
  ; todo - is this how cost should be calculated? maybe energy expenditure estimate?
  report distance from_patch
end

;; Calcuate the survival chances of this patch. Used to determine what patches are options
to-report calculate_patch_survival [fish]
  if (fish_survival = -1)
  [
    set fish_survival 1 - (calc-pred-mortality survival_prob encounter_prob max_prey_length [f_length] of fish num_preds)
  ]
  report fish_survival
end

;; Store values relevant for the pathfinding. Use the from_patch and the provided cost
; to calculate values for myself.
to store_pathfinding_patch_values [from_patch cost survival]
  ; Set the path data for this patch (costs, previous patch, visited)
  set previous_patch from_patch
  set path_to_here_cost cost
  set has_visited? true
  set path_survival survival
end

;; Find possible destinations the fish can swim to within the given move_dist.
; This minimizes the predation risk along a path and limits total distance.
; travelled by the provided move distance.
; This also handles stranding by having fish stuck on a dry patch use yesterday's
; water levels for valid pathing decisions. If the results are just the current
; patch, than the fish has stranded.
to-report find_possible_destinations [fish move_dist]
  let destinations (list)
  ; We start looking for valid destinations with the fish's current location.
  let to_visit (list [patch-here] of fish)
  ask [patch-here] of fish
  [
    set has_visited? true
    set path_to_here_cost 0
    set path_survival 1
  ]
  let dirty (list[patch-here] of fish)
  ; Check if the fish is trying to avoid stranding or not
  let avoiding_stranding [today_depth] of [patch-here] of fish > 0
  while [length to_visit > 0]
  [
    ; Pull the patch we're looking at off the to_visit list.
    let cur_patch first to_visit
    set to_visit but-first to_visit
    ask cur_patch [
      ask neighbors [
        ; validate neighbor is wet
        if (today_depth > 0 or (avoiding_stranding and yesterday_depth > 0))
        [
          let survival [path_survival] of cur_patch * calculate_patch_survival fish
          ; If this is the first time being visited or this path was less risky than an alternative
          if (has_visited? = false) or (path_survival < survival)
          [
            let move_cost [path_to_here_cost] of cur_patch + calculate_move_cost cur_patch
            if (move_cost < move_dist)
            [
              if (has_visited? = false)
              [
                set dirty lput self dirty
              ]
              store_pathfinding_patch_values cur_patch move_cost survival
              ; If we've moved too far, don't bother adding to the to_visit list, but
              ; if we still have distance the fish can travel than keep going.
              set to_visit lput self to_visit
              ; If we're in stranding logic we might be evaluating patches that aren't currently wet.
              ; Make sure a patch is wet before considering it a valid destination.
              ; Also chack to make sure there is area fo the fish
              if (today_depth > 0) and (not member? self destinations) and (cell_available_wet_area >= [territory_size] of fish) ; and (pycor < [ ycor ] of fish) may add in later?
              [
                ; If all checks pass (including depth and having available area), mark as a potential destination.
                set destinations lput self destinations
              ]
            ]
          ]
        ]
      ]
    ]
  ]

  ifelse (empty? destinations)
  [
    ; This fish has nowhere to go and is stuck on dry land, strand them.
    ask fish [set strand_status 1]
    ; We're expected to return something, so even if we're stranding add the current patch
    set destinations lput ([patch-here] of fish) destinations
  ]
  [
    ; Make sure the current patch is an option to avoid fish shifting back and forth between adjacent patches.
    let cur_patch [patch-here] of fish
    if ([today_depth] of cur_patch > 0) and (not member? cur_patch destinations) and ([cell_available_wet_area] of cur_patch >= [territory_size] of fish) ; and (pycor < [ ycor ] of fish) may add in later?
    [
      ask cur_patch [
        set path_survival calculate_patch_survival fish
      ]
      set destinations lput cur_patch destinations
    ]
  ]
  ask patch-set dirty [
    clear_patch_path_data
  ]
  report patch-set destinations
end

;; Remove resources form cells
to deplete_destination_resources
; Deplete the food resources and the shelter resources in the desination cell

  ask destination [

    ; If there are velocity shelters for the fish to use, and it's not benthic we remove the shelter from available shelter

    ifelse (item species_id fish_benthic) = 0 [
      if (cell_available_vel_shelter) >= ([territory_size] of myself) [
        set cell_available_vel_shelter cell_available_vel_shelter - ([territory_size] of myself)
      ]
      if (cell_available_wet_area) >= ([territory_size] of myself) [
        set cell_available_wet_area cell_available_wet_area - ([territory_size] of myself)
      ]
    ][
     if (cell_available_ben) >= (daily_intake) [
      set cell_available_ben max list (cell_available_ben - daily_intake) 0
      ]
    ]
  ]

  if (item species_id fish_benthic) = 0 [  ; for some reason, I can't set is_in_shelter to true within the code block above, so I have it here for now
      if (cell_available_vel_shelter) >= (territory_size) [
      set is_in_shelter true ]
  ]

;  if (item species_id fish_benthic) = 0 ;###### this might get used for benthic fish later
;    [if ([cell_available_vel_shelter] of destination) >= (territory_size)
;      [set is_in_shelter true] ; If there are velocity shelters for the fish to use, we set is_in_shelter to true
;    ]

end

to-report calc-pred-mortality [#survival_prob #encounter_prob #max_prey_length #prey_f_length #num_preds]

  ; mortality is 0 if fork length is too big for predators
  if #max_prey_length < (#prey_f_length * 10)  ; f_length is in cm but the model that calcs max_prey_length uses mm
  [
    report 0
  ];

  ifelse #encounter_prob <= 1
  [
    ; this will be the output in most cases with relatively low predator density
    report #encounter_prob * (1 - #survival_prob)
  ]
  [
    ; allow for multiple encounters in patches with encounter_prob > 1
    ; find the most appropriate value to act as an encounter rate
    ; prevents absurdly high numbers from being used
    let new_enc_num (min (list (#encounter_prob) (sum #num_preds)))
    ; integer value of the new enc number determines how many encounters a prey fish is guaranteed to have in high enc prob patches
    let guaranteed_num_encounters floor new_enc_num
    ; any leftover decimal-point value can be used to factor in an additional potential encounter with prob of occurring equal to the decimal value
    let potential_extra_encounter new_enc_num - guaranteed_num_encounters

    report 1 - (#survival_prob ^ (guaranteed_num_encounters + 1) * potential_extra_encounter + (1 - potential_extra_encounter) * #survival_prob ^ guaranteed_num_encounters)
  ]
end

;; Determines whether fish die and of what cause
to survive

  if strand_status = 1
  [
    print "died of stranding"
    set death_stranding_list lput 1 death_stranding_list
    save_event "died of stranding"
    set is_alive false
    die
  ]

  if (random-float 1.0) > [path_survival] of destination ; If the fish is eaten in its destination cell or if the survival probability of the path is less than a random number
  [ ; Fish died of fish predation
    print "died of fish predation"
    set death_pred_list lput 1 death_pred_list
    save_event "died of predation"
    set is_alive false
    die
  ]


  if (random-float 1.0) < ([fish_death_hightemp_prob] of destination)
  [ ; Fish died of high temperature
    print "died of high temperature"
    set death_temp_list lput 1 death_temp_list
    save_event "died of high temp"
    set is_alive false
    die
  ]


  if (random-float 1.0) > (fish_death_starv_survival_prob)
  [ ; Fish died of poor condition
    print "died of poor condition"
    set death_condition_list lput 1 death_condition_list
    save_event "died of poor condition"
    set is_alive false
    die
  ]

  ifelse is_alive = true [
    save_event "alive"
    set daily_juvenile_count lput 1 daily_juvenile_count
    ask destination [
      set count_fish_destination count turtles-here
      set avg_weight_fish_in_destination mean [mass] of turtles-here
      set avg_length_fish_in_destination mean [f_length] of turtles-here
      set avg_condition_fish_in_destination mean [fish_condition] of turtles-here
    ]
  ]
  [
    set daily_dead_fish_count lput 1 daily_dead_fish_count
  ]

  ;print "number of fish in cell"
  ;show count_fish_destination

  ;print "number of dead fish in cell"
  ;show count_dead_fish_destination

end

;; Saves the information of each destination cell every timestep (for cell_info_list csv)
to save_destination_cell_info

  set destination_cells [self] of patches with [ any? turtles-here and migrant_patch = false ]

 ; print "destination cells"
 ; show destination_cells


  foreach destination_cells [[next_cell] ->

  set cell_info_list lput ( csv:to-row (list
      tick_date
      next_cell
      [x_pos] of next_cell
      [y_pos] of next_cell
      [count_fish_destination] of next_cell
      [avg_weight_fish_in_destination] of next_cell
      [avg_length_fish_in_destination] of next_cell
      [avg_condition_fish_in_destination] of next_cell
      [today_velocity] of next_cell
      [today_depth] of next_cell
      [total_food_in_cell] of next_cell
      [total_mort_risk_for_cell] of next_cell
      [fish_death_aq_pred_prob] of next_cell
    )) cell_info_list

  ]

;profiler:stop
;print profiler:report  ;; view the results
;profiler:reset
end

;; A turtle procedure to save events for the fish output files (events include mortality, initialization, smolting etc)
to save_event [an_event_type]

  ; an-event-type is a character string that says what event happened.
  if breed = juveniles
  [
    set fish_events_list lput ( csv:to-row (list
      tick_date
      species_id
      who
      destination
      f_length
      mass
      fish_condition
      daily_growth
      percent_daily_growth
      fraction_growth_cmax
      is_in_shelter
      is_drifter
      an_event_type
      [today_velocity] of destination
      [today_depth] of destination
      [distance_to_cover] of destination
      [cell_available_vel_shelter] of destination
      [total_food_in_cell] of destination
      [total_mort_risk_for_cell] of destination
      [fish_death_aq_pred_prob] of destination
    )) fish_events_list
  ]

end

;; Saves detailed summary info of the population every timestep ( total number fish migrating, total number dead fish etc)
to save_detailed_population_info

  set total_daily_juveniles sum daily_juvenile_count ; the number of non-smolt and smolt individuals in the river that day
  set total_daily_migrants sum daily_migrant_count ; the number of individuals that turned into migrants that day
  set total_daily_smolts sum daily_smolt_count    ; the number of individuals that turned into smolts that day
  set total_daily_dead_fish sum daily_dead_fish_count ; the number of individuals that died that day
  set total_daily_drifters sum daily_drifter_count  ; the number of individuals that turned into drifters that day

  set detailed_population_list lput ( csv:to-row (list
      tick_date
      total_daily_juveniles   ;this includes smolts
      total_daily_migrants
      total_daily_smolts
      total_daily_dead_fish
      total_daily_drifters
      temperature
      flow
    )) detailed_population_list

end

;; An observer procedure to initialize an output file.
to build_output_file_named [a_file_name]

  ; The parameter a_file_name is the global variable for the file name.

  ; Create the brief population output file.
  if a_file_name = "b-p-o-n" ; This is the value of the uninitialized file name
  [
    set brief_pop_outfile_name (word "BriefPopOut-" outfile_name_base)
    if file-exists? brief_pop_outfile_name [ file-delete brief_pop_outfile_name ]
    file-open brief_pop_outfile_name
    file-print (word "FHAST brief population output file, Created " date-and-time)
    file-print "Juvenile count, smolt count, migrant count, juvenile length, smolt length, migrant length, juvenile mass, smolt mass, migrant mass, juvenile condition, smolt condition, migrant condition, predation, high temperature, stranding, poor condition, velocity"
    file-close
  ]

;; Create the detailed population output file.
   if a_file_name = "d-p-o-n" ; This is the value of the uninitialized file name
  [
    set detailed_population_outfile_name (word "DetailedPopInfoOut-" outfile_name_base)
    if file-exists? detailed_population_outfile_name [ file-delete detailed_population_outfile_name ]
    ; These header lines must be put at the *start* of the list. Use fput with header
    ; lines in reverse order.
    set detailed_population_list fput "End of time step, Number of juveniles, Number of migrants, Number of smolts, Number of dead fish, Number of drifters, Temperature, Flow" detailed_population_list
    set detailed_population_list fput (word "FHAST detailed population output file, Created " date-and-time) detailed_population_list
  ]
;
   ; Create the destination cell info output file.
   if a_file_name = "c-i-o-n" ; This is the value of the uninitialized file name
  [
    set cell_info_outfile_name (word "CellInfoOut-" outfile_name_base)
    if file-exists? cell_info_outfile_name [ file-delete cell_info_outfile_name ]
    ; These header lines must be put at the *start* of the list. Use fput with header
    ; lines in reverse order.
    set cell_info_list fput "End of time step, Cell, Xpos, Ypos, Number of fish alive, Avg weight of live fish, Avg length of live fish, Avg condition of live fish, Velocity at cell, Depth at cell, Food at cell, Mortality risk survival prob, Prob of dying from predation" cell_info_list
    set cell_info_list fput (word "FHAST destination cell info output file, Created " date-and-time) cell_info_list
  ]

  ; Create the fish events output file.
  if a_file_name = "f-e-o-n" ; This is the value of the uninitialized file name
  [
    set fish_events_outfile_name (word "FishEventsOut-" outfile_name_base)
    if file-exists? fish_events_outfile_name [ file-delete fish_events_outfile_name ]
    ; There can be events on the fish-events-list when file is created (from fish initialization)
    ; so these header lines must be put at the *start* of the list. Use fput with header
    ; lines in reverse order.
    set fish_events_list fput "End of time step,Species,ID,Cell,Length,Weight,Condition, Growth, Percent daily growth, Fraction growth of cmax, In shelter?, Is a drifter?, Event, Velocity at cell, Depth at cell, Distance to cover, Available velocity shelter, Food at cell, Mortality risk ratio, Prob of dying from predation" fish_events_list
    set fish_events_list fput (word "FHAST fish events output file, Created " date-and-time) fish_events_list
  ]


end

;; Updates all output files
to update_output

  ; Output files are created when first used, instead of in setup,
  ; to keep new output files from being created if setup is executed but go is not.

  if brief_pop_output?
  [
  if brief_pop_outfile_name = "b-p-o-n" [ build_output_file_named brief_pop_outfile_name ]
      file-open brief_pop_outfile_name

           ; These statements are to make output work when there are no fish in the category
           ; (because "mean" raises an error if there are no values).
           let the_count_juveniles 0.0
           let the_count_smolts 0.0
           let the_count_migrants 0.0
           ;let the_count_drifters 0.0
           let the_length_juveniles 0.0
           let the_length_smolts 0.0
           let the_length_migrants 0.0
           ;let the_length_drifters 0.0
           let the_weight_juveniles 0.0
           let the_weight_smolts 0.0
           let the_weight_migrants 0.0
           ;let the_weight_drifters 0.0
           let the_condition_juveniles 0.0
           let the_condition_smolts 0.0
           let the_condition_migrants 0.0
           ;let the_condition_drifters 0.0
           let count_death_predation 0.0
           let count_death_hightemp 0.0
           let count_death_stranding 0.0
           let count_death_poorcond 0.0
           let count_death_velocity 0.0

           set the_count_juveniles (count juveniles)
           set the_count_smolts (count juveniles with [is_smolt = TRUE])
           set the_count_migrants (sum migrant_count_list)
           ;set the_count_drifters (sum drifter_count_list)
           set the_length_juveniles (mean [f_length] of juveniles)
           set the_length_smolts (mean [f_length] of juveniles with [is_smolt = TRUE])
           set the_length_migrants (mean migrant_length_list)
           ;set the_length_drifters (mean drifter_length_list)
           set the_weight_juveniles (mean [mass] of juveniles)
           set the_weight_smolts (mean [mass] of juveniles with [is_smolt = TRUE])
           set the_weight_migrants ((mean migrant_mass_list))
           ;set the_weight_drifters (mean drifter_mass_list)
           set the_condition_juveniles (mean [fish_condition] of juveniles)
           set the_condition_smolts (mean [fish_condition] of juveniles with [is_smolt = TRUE])
           set the_condition_migrants (mean migrant_condition_list)
           ;set the_condition_drifters (mean drifter_condition_list)
           set count_death_predation sum death_pred_list
           set count_death_hightemp sum death_temp_list
           set count_death_stranding sum death_stranding_list
           set count_death_poorcond sum death_condition_list
           set count_death_velocity sum death_velocity_list

              file-print csv:to-row (list
                  the_count_juveniles
                  the_count_smolts
                  the_count_migrants
                  ;the_count_drifters
                  the_length_juveniles
                  the_length_smolts
                  the_length_migrants
                  ;the_length_drifters
                  the_weight_juveniles
                  the_weight_smolts
                  the_weight_migrants
                  ;the_weight_drifters
                  the_condition_juveniles
                  the_condition_smolts
                  the_condition_migrants
                  ;the_condition_drifters
                  count_death_predation
                  count_death_hightemp
                  count_death_stranding
                  count_death_poorcond
                  count_death_velocity
                ) ; End of output list

  file-close
  ]

 ; The following outputs are produced each tick

  ; Update detailed population output file
  ifelse detailed_population_output?
   [
    if not empty? detailed_population_list
    [
      if detailed_population_outfile_name = "d-p-o-n" [ build_output_file_named detailed_population_outfile_name ]
      file-open detailed_population_outfile_name
      foreach detailed_population_list [ next -> file-print next ]  ; Write each detail to the file
      file-close
      set detailed_population_list (list)                ; Clear the list
    ]
  ][
    set detailed_population_list (list)                ; Clear the list
  ]

  ; Update detailed fish events output file
  ifelse fish_events_output?
   [
    if not empty? fish_events_list
    [
      if fish_events_outfile_name = "f-e-o-n" [ build_output_file_named fish_events_outfile_name ]
      file-open fish_events_outfile_name
      foreach fish_events_list [ next_cell -> file-print next_cell ]  ; Write each cell event to the file
      file-close
      set fish_events_list (list)                ; Clear the event list
    ]
  ][
    set fish_events_list (list)                ; Clear the event list
  ]

  ; Update detailed cell events output file
  ifelse cell_info_output?
   [
    if not empty? cell_info_list
    [
      if cell_info_outfile_name = "c-i-o-n" [ build_output_file_named cell_info_outfile_name ]
      file-open cell_info_outfile_name
      foreach cell_info_list [ next_cell -> file-print next_cell ]  ; Write each cell event to the file
      file-close
      set cell_info_list (list)                ; Clear the event list
    ]
  ][
    set cell_info_list (list)                ; Clear the event list
  ]
;
  ; Just to be safe & tidy
  file-close-all


end

;; End the program
to stop_program
  if ticks = (length daily_flow_values) [
    user-message "All days run.\nProgram finished."
  set stop_flag TRUE]
 end
@#$#@#$#@
GRAPHICS-WINDOW
213
10
302
620
-1
-1
1.3363028953229399
1
10
1
1
1
0
1
1
1
-30
30
0
449
1
1
1
ticks
30.0

BUTTON
27
307
91
341
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
23
355
122
426
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
579
14
864
231
Juvenile Sum
Time
# of Juveniles Alive
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count juveniles"

PLOT
599
313
848
512
Fork length over time
Time
Average fork length
0.0
10.0
0.0
3.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [f_length] of juveniles"
"min" 1.0 0 -7500403 true "" "plot min [f_length] of juveniles"
"max " 1.0 0 -2674135 true "" "plot max [f_length] of juveniles"

PLOT
876
42
1179
278
Condition factor over time
Time
Condition factor
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [fish_condition] of juveniles"
"min" 1.0 0 -9276814 true "" "plot min [fish_condition] of juveniles"

PLOT
853
311
1122
514
Flow vs time
Time
Flow cm3/s
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"daily flow" 1.0 0 -13791810 true "" "plot flow"
"410 " 1.0 0 -7500403 true "" "plot 410"

PLOT
1206
42
1521
296
Smolted individuals vs time
Time
Smolt Sum
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"smolt sum" 1.0 0 -16777216 true "" "plot count juveniles with [is_smolt = true]"

PLOT
1461
316
1788
592
Migrant sum vs time
Time
Migrant Sum
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"migrant sum" 1.0 0 -16777216 true "" "plot sum migrant_count_list"

PLOT
1534
41
1782
291
Photoperiod vs time
Time
Photoperiod
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot photoperiod"

SWITCH
16
124
174
157
brief_pop_output?
brief_pop_output?
0
1
-1000

SWITCH
13
168
203
201
detailed_population_output?
detailed_population_output?
0
1
-1000

SWITCH
15
213
209
246
fish_events_output?
fish_events_output?
0
1
-1000

SWITCH
24
262
191
295
cell_info_output?
cell_info_output?
0
1
-1000

CHOOSER
13
19
202
64
background_display
background_display
"veg" "wood" "depth" "velocity" "shade" "predator habitat rating" "wetted fraction" "available velocity shelter"
3

SWITCH
14
73
200
106
draw_fish_movements?
draw_fish_movements?
0
1
-1000

PLOT
1137
315
1448
536
Causes of death
Time
# of juveniles dead
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"velocity" 1.0 0 -14070903 true "" "plot sum death_velocity_list"
"temperature" 1.0 0 -2674135 true "" "plot sum death_temp_list"
"stranding" 1.0 0 -6459832 true "" "plot sum death_stranding_list"
"predation" 1.0 0 -10899396 true "" "plot sum death_pred_list"
"poor condition " 1.0 0 -10141563 true "" "plot sum death_condition_list"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
