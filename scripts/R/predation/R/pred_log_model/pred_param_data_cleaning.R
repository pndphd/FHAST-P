
# 0. load libraries -------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(here)
options(readr.show_col_types = FALSE)

# 1. load data ------------------------------------------------------------
pred_proj_path <- here::here("scripts", "R", "predation")
data_2013 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2013 Master Data.csv"))
data_2014 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2014 Master Data.csv"))

# 2. functions ------------------------------------------------------------

update_cover_names <- function(var) {
  dplyr::case_when(
    {{ var }} == "VERY LOW" ~ "S",
    {{ var }} == "LOW" ~ "M",
    {{ var }} == "MEDIUM" ~ "M",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "VERY HIGH" ~ "H",
    {{ var }} == "HEAVY" ~ "H",
    {{ var }} == "NOW" ~ "S",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "D" ~ "H",
    {{ var }} == "NONE" ~ "A",
    TRUE ~ {{ var }}
  )
}

convert_cover_to_val <- function(var) {
  dplyr::case_when(
    {{ var }} == "A" ~ 0,
    {{ var }} == "S" ~ 0.055,
    {{ var }} == "M" ~ 0.3,
    TRUE ~ 0.6,
  )
}

# 3. clean data -----------------------------------------------------------

# work columns so data from both years can be combined
data_2013_parsed <-
  data_2013 %>%
  dplyr::select(
    -c(
      Site:CHNJ,
      MYKISS:ISS,
      TerrVeg:TerrWM,
      TempF:DiffF
    )
  )

data_2014_parsed <-
  data_2014 %>%
  dplyr::mutate(
    BASS = rowSums(dplyr::across(LMBS:SMBL)),
    SASQ = rowSums(dplyr::across(SASQS:SASQL))
  ) %>%
  dplyr::select(
    -c(
      Site:C,
      TerrVeg:TerrWM,
      MidChannelTempF:DiffF
    )
  )

combined_data <- bind_rows(data_2013_parsed, data_2014_parsed)

# add sample ID column and fix column names

combined_data <-
  combined_data %>%
  rename_all(~ tolower(.)) %>% # for more consistent naming
  rename_all(~ str_replace_all(., "\\s+", "_"))

# make data in cover columns consistent and convert to proportions
combined_data <-
  combined_data %>%
  dplyr::mutate(
    dplyr::across(
      emergvegdensity:emergwmdensity,
      toupper
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      emergvegdensity:emergwmdensity,
      .fns = ~ update_cover_names(.x)
    )
  ) %>%
  dplyr::filter(!emergwmdensity == "1")

# convert cover to values

combined_data <-
  combined_data %>%
  dplyr::mutate(
    dplyr::across(
      emergvegdensity:emergwmdensity,
      .fns = ~ convert_cover_to_val(.x)
    )
  )

# update shade
combined_data <-
  combined_data %>%
  dplyr::mutate(
    shade = toupper(shade),
    shade = dplyr::case_when(
      shade == "ABSENT" ~ "A",
      shade == "PRESENT" ~ "P",
      shade == "C" ~ "P",
      TRUE ~ shade
    ),
    shade = dplyr::if_else(shade == "P", 1, 0)
  )

# add mean depth and velocity

combined_data <-
  combined_data %>%
  dplyr::mutate(
    mean_depth = rowMeans(dplyr::across(depth5:depth15)),
    mean_vel = rowMeans(dplyr::across(vel5:vel15))
  )

# drop distance-dependant depth and velocity
combined_data <-
  combined_data %>%
  dplyr::select(-c(depth5:depth15, vel5:vel15))

# update substrate

combined_data <-
  combined_data %>%
  dplyr::mutate(
    substrate = substrate10,
    substrate = dplyr::case_when(
      substrate == "G" ~ "R",
      substrate == "R/G" ~ "R",
      substrate == "R/M" ~ "R",
      substrate == "M/R" ~ "R",
      substrate == "M/G" ~ "M",
      substrate == "M/V" ~ "M",
      TRUE ~ substrate
    ),
    substrate = dplyr::if_else(substrate == "R", 1, 0)
  ) %>%
  dplyr::select(-c(substrate5:substrate15))

# set pred counts to presence/absence

combined_data <- combined_data %>%
  dplyr::mutate(
    dplyr::across(
      bass:sasq,
      .fns = ~ dplyr::if_else(.x > 0, "present", "absent")
    )
  )

# create predator specific data sets

combined_data <- combined_data %>%
  tidyr::pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count")

# set character data types as factors
combined_data <- combined_data %>%
  dplyr::mutate_if(is.character, factor)

# drop any NA's

combined_data <- combined_data %>%
  tidyr::drop_na()

# rename cols to be more compatible with our modeling terms

combined_data <- combined_data %>%
  dplyr::rename(
    veg = emergvegdensity, # veg_cover
    wood = emergwmdensity, # wood_cover
    depth_ft = mean_depth,
    velocity_fps = mean_vel
  )

# convert to metric
combined_data <- combined_data %>%
  mutate(across(depth_ft:velocity_fps, fns = ~ .x / 3.28)) %>%
  rename(
    velocity = velocity_fps,
    depth = depth_ft
  )
