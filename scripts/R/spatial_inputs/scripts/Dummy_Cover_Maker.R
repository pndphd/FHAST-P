##### Description #####
# This is a dummy script just to make a cover test file 

##### Inputs #####

shape_file_path <- "../input_data/Arden_Pond_Post/"
shape_file_name <- "cover_base_file_26941.shp"

##### Libraries #####
library(tidyverse)
library(here)
library(sf)
# library(tidyverse)
# library(lubridate)
# allows combining of plots
# library(patchwork)
# for working with rasters
# library(raster)
# for working with shape files
# library(sf)
# Open and write excel files
# library(openxlsx)
# The color palet library
# library(viridis)

# Overwrite any files which may have been masked. Example:
# Make sure raster::select dosen't mask dplyr::select
# select = dplyr::select

# Color blind pallet
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)
# Look at graffy package for more color blind palets

##### Functions #####
# Make a function 
# functionName = function(input1){
#   return(output)
# }

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
##### Load Files #####
# Load data files
shape_file  <- read_sf(paste0(shape_file_path, shape_file_name)) %>%
  rename_with(tolower)
# rData = readRDS(file = "path") 
# excelData = read.xlsx(xlsxFile = "path", sheet = "SheetName") 
# csvData = read.csv(file = "path")

##### Pre Processing #####
# Do any basic pre processing here

##### Main Work #####
##### Main Thing 1 #####
# Do the work for the first part
cover_output <- shape_file %>% 
  mutate(height = calc_all_heights(ht_code),
         class = ifelse(height>10, "t_wood",
                        ifelse(height>0, "t_veg", "bare")),
         class = ifelse(cv_group == "WAT: Water", "sand", class),
         cover = replace_na(per_total, 0),
         cover = ifelse(cv_group == "WAT: Water", 0, cover),
         cover = cover/max(cover)) %>% 
  select(class, cover)

tree_output <- shape_file %>% 
  mutate(height = calc_all_heights(ht_code)) %>% 
  # Took out the three height
  filter(height >= 5) %>% 
  select(height)


##### Main Thing 2 #####
# Do the work for the second part

##### Plots #####
# Make a window on a second screen
# windows(xpos = 2000)
# 
# plotName = ggplot(data, aes(x = x, y = y))+
#   theme_classic(base_size = 30) +
#   labs(y = "y label", x = "x label") +
#   theme(legend.title = element_blank(),
#     legend.position = c(0.8, 0.2)) +
#   geom_path(aes(x = x1, y = y1, color = "definedColor")) +
#   scale_color_manual(name = NULL, values = c(definedColor = "gray20", definedColor2 = "dodgerblue3")) +
#   scale_color_brewer(palette = "Paired") +
#   scale_color_manual(values = cbPalette) +
#   geom_text(aes(y=interval, label = count), vjust=1.6, 
#             color="Black", size=5, position = position_dodge(width = 0.9)) +
#   geom_hline(yintercept = 0, size = 1, color = "black") 
# print(plotName)

##### Save Outputs #####
st_write(cover_output, here(shape_file_path, "cover_file_no_water.shp"), delete_layer = TRUE)
st_write(tree_output, here(shape_file_path, "tree_file.shp"), delete_layer = TRUE)
# 
# write.xlsx(x,
#            file = "path",
#            sheetName = "Sheet Name")
# 
# write.csv(x,
#           file = "path")
# saveRDS(object,
#         file = "path")