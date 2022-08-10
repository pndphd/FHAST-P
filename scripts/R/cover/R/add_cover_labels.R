
# 0. load libraries -------------------------------------------------------

library(tidyverse)
library(here)
library(sf)

# 1. load data ------------------------------------------------------------

data_path <- here::here("scripts", "R", "cover", "data", "ds2890.shp")

data <- read_sf(data_path)

# 2. functions ------------------------------------------------------------

extract_nums <- function(string) {
  str_extract_all(string, "\\(?[0-9,.]+\\)?")
}

calc_height <- function(nums_as_string) {
  nums <- nums_as_string %>%
    as.numeric()
  if (length(nums) > 1) {
    return(mean(nums))
  } else if (nums == 1) {
    return(nums / 2)
  } else {
    return(nums)
  }
}

calc_all_heights <- function(height_list) {
  height_list <- replace_na(height_list, "0")
  nums_as_strings <- extract_nums(height_list)
  map_dbl(nums_as_strings, calc_height)
}

# 3. analysis -------------------------------------------------------------

new_cover_file <- data %>%
  rename_with(tolower) %>%
  mutate(
    across(
      .cols = c(per_total, per_tree),
      .fns = ~ replace_na(.x, 0) / 100,
    ),
    height = calc_all_heights(ht_code),
    per_tree = case_when(
      per_tree > 1 ~ 0,
      TRUE ~ per_tree
    )
  ) %>%
  rename(vegetation = per_total,
         pct_tree_cover = per_tree) %>% 
  select(height, vegetation, pct_tree_cover) 

# TODO: decide which veg category column to keep

# 4. save output ----------------------------------------------------------

output_path <- here::here("scripts", "R", "cover", "output")

st_write(new_cover_file, here(output_path, "cover_file.shp"), delete_layer = TRUE)





#
# tic()
# data %>%
#   mutate(height = case_when(HT_CODE == "< 1m" ~ 0.5,
#                             HT_CODE == "1 - 5m" ~ 3,
#                             HT_CODE == "20 - 50m" ~ 35,
#                             HT_CODE == "5 - 20m" ~ 12.5,
#                             TRUE ~ 0))
# toc()
#


# full_veg <- list("RWF: Riparian Evergreen and Deciduous Woodland" ,
#                  "IMF: Introduced North American Mediterranean Forest",
#                  "VPG: California Vernal Pool and Grassland Matrix" ,
#                  "CAI: California Introduced Annual and Perennial Herbaceous"   ,
#                  "WVO: California Broadleaf Forest and Woodland"    ,
#                  "CFG: California Annual Forbs and Grasses" ,
#                  "RWS: Southwestern North American Riparian Wash/Scrub"   ,
#                  "FEM: Freshwater Emergent Marsh"     ,
#                  "WTM: California Warm Temperate Marsh/Seep" ,
#                  "NTF: Naturalized Temperate Pacific Freshwater Vegetation"  ,
#                  "RIS: Riparian Introduced Scrub"        ,
#                  "TFF: Temperate Freshwater Floating Mat",
#                  "NRW: Naturalized Warm-Temperate Riparian/Wetland" ,
#                  "SSB: Southwestern North American Salt Basin and High Marsh" ,
#                  "AGP: Alkali Grassland - Playa/Pool Matirx"   ,
#                  "VRF: Vancouverian Riparian Deciduous Forest"   ,
#                  "VPB: Californian Mixed Annual/Perennial Freshwater Vernal Pool/Swale Bottomland",
#                  "TBM: Temperate Pacific Tidal Salt and Brackish Meadow" ,
#                  "CSS: Central and South Coastal California Seral Scrub",
#                  "CPG: California Perennial Grassland"  ,
#                  "ECW: California Evergreen Coniferous Forest and Woodland" ,
#                  "CXC: California Xeric Chaparral"  ,
#                  "VCM: Vancouverian Coastal/Tidal Marsh and Meadow"   ,
#                  "LDS: Lower Bajada and Fan Mojavean-Sonoran desert scrub"   ,
#                  "SAM: Southwestern North American Alkali Marsh/Seep Vegetation"    ,
#                  "DUP: Dry Upland Perennial Grassland",
#                  "DAM: Western North American Disturbed Alkaline Marsh and Meadow"   ,
#                  "NMS: Naturalized non-native Mediterranean scrub"   ,
#                  "RMM: Western North American Ruderal Marsh, Wet Meadow & Shrubland Group",
#                  "CCS: Central and South coastal Californian coastal sage scrub"    ,
#                  "BDS: California Coastal evergreen bluff and dune scrub"
# )
# half_veg <- list("AGR: Agriculture",
#                  "SVP: Sparsely Vegetated Playa/Pool"   )
# low_veg <- list("BGS: Bare - Gravel/Sand" ,
#                 "WAT: Water" )
# no_veg <- list("QMG: Stripmines, quarries and gravel pits",
#                "URB: Urban",
#                "CRO: Cliffs and rock outcrop"
# )
#
# data_2 <- data %>%
#   mutate(vegetation = case_when(CV_Group %in% full_veg ~ 1,
#                                 CV_Group %in% half_veg ~ 0.5,
#                                 CV_Group %in% low_veg ~ 0.1,
#                                 TRUE ~ 0),
#   )
