#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)

input_tab = tabPanel(
  "Configuration Directory",
  textInput("config_dir", "Configuration Directory"),
  shinyDirButton('picker_config_dir', 'Select Configuration', 'Please select the configuration folder', FALSE),
  
)

input_file_tab = tabPanel(
  "Input File",
  # Input folder
  textInput("input_dir", "Input Directory"),
  shinyDirButton('picker_input_dir', 'Select Input', 'Please select the input folder', FALSE),
  
  textInput("raster_folder", "Depth and Velocity Raster Directory"),
  shinyDirButton('picker_raster_folder', 'Select Depth and Velocity Raster Directory', 'Please select the Depth and Velocity Raster Folder', FALSE),
  
  textInput("hydrology_folder", "Hydrology Directory"),
  shinyDirButton('picker_hydrology_folder', 'Hydrology Directory', 'Please select the Hydrology folder', FALSE),
  
  # Daily conditions
  textInput("daily_file", "Daily Conditions"),
  shinyFilesButton('picker_daily_file', 'Select Daily Conditions', 'Please select the Daily Conditions', FALSE),
  
  # Fish files
  textInput("fish_pop_file", "Fish Popoulation"),
  shinyFilesButton('picker_fish_pop_file', 'Select Fish Population', 'Please select the Fish Population', FALSE),
  textInput("fish_params_file", "Daily Conditions"),
  shinyFilesButton('picker_fish_params_file', 'Select Fish Parameters', 'Please select the Fish Parameters', FALSE),
  # # Grid files
  textInput("grid_center_file", "Grid Center Line"),
  shinyFilesButton('picker_grid_center_file', 'Select Grid Center Line', 'Please select the Grid Center Line', FALSE),
  textInput("grid_top_file", "Grid Top Point"),
  shinyFilesButton('picker_grid_top_file', 'Select Grid Top Point', 'Please select the Grid Top Point', FALSE),
  textInput("grid_res_file", "Grid Resolution"),
  shinyFilesButton('picker_grid_res_file', 'Select Grid Resolution', 'Please select the Grid Resolution', FALSE),
  # # Habitat input files
  textInput("cover_file", "Cover File"),
  shinyFilesButton('picker_cover_file', 'Select Cover File', 'Please select the Cover File', FALSE),
  textInput("canopy_cover_file", "Canopy Cover File"),
  shinyFilesButton('picker_canopy_cover_file', 'Select Canopy Cover File', 'Please select the Canopy Cover File', FALSE),
  textInput("hab_params_file", "Habitat Parameters"),
  shinyFilesButton('picker_hab_params_file', 'Select Habitat Parameters', 'Please select the Habitat Parameters', FALSE),

  # # Predator files
  # predator density = 30
  # conversion = 1e4
  # reaction distance = 1  
)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeScript(path = "ui.js"),
  # Application title
  titlePanel("FHAST"),

  # Sidebar with inputs for directories and the run button.
  sidebarLayout(
      sidebarPanel(
        # Have a tab per input file
        # Add a save button on each tab that will update file on disk with new values
        tabsetPanel(type = "tabs",
                    input_tab,
                    input_file_tab,
        ),
        hr(),
        actionButton("save", "Save"),
        hr(),
        actionButton("run", "Save and Run"),
      ),

      mainPanel(
        # Add display here or at least progress indication.      
      )
  )
))
