#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)

input_tab <- tabPanel(
  "Configuration Directory",
  textInput("config_file", "Configuration File"),
  shinyFilesButton("picker_config_file", "Select Configuration File",
    "Please select the Configuration File", FALSE),
)

input_file_tab <- tabPanel(
  "Input File",
  # Input folder
  textInput("raster_folder", "Depth and Velocity Raster Directory"),
  shinyDirButton("picker_raster_folder",
    "Select Depth and Velocity Raster Directory",
    "Please select the Depth and Velocity Raster Folder", FALSE),

  # Daily conditions
  textInput("daily_file", "Daily Conditions"),
  shinyFilesButton("picker_daily_file", "Select Daily Conditions",
    "Please select the Daily Conditions", FALSE),

  # Fish files
  textInput("fish_pop_file", "Fish Popoulation"),
  shinyFilesButton("picker_fish_pop_file", "Select Fish Population",
    "Please select the Fish Population", FALSE),
  textInput("fish_params_file", "Fish Parameters"),
  shinyFilesButton("picker_fish_params_file", "Select Fish Parameters",
    "Please select the Fish Parameters", FALSE),
  # # Grid files
  textInput("grid_center_file", "Grid Center Line"),
  shinyFilesButton("picker_grid_center_file", "Select Grid Center Line",
    "Please select the Grid Center Line", FALSE),
  textInput("grid_top_file", "Grid Top Point"),
  shinyFilesButton("picker_grid_top_file", "Select Grid Top Point",
    "Please select the Grid Top Point", FALSE),
  textInput("interaction_file", "Interaction"),
  shinyFilesButton("picker_interaction_file", "Select Interaciton",
    "Please select the Interaction file", FALSE),
  # # Habitat input files
  textInput("cover_file", "Cover File"),
  shinyFilesButton("picker_cover_file", "Select Cover File",
    "Please select the Cover File", FALSE),
  textInput("canopy_cover_file", "Canopy Cover File"),
  shinyFilesButton("picker_canopy_cover_file", "Select Canopy Cover File",
    "Please select the Canopy Cover File", FALSE),
  textInput("hab_params_file", "Habitat Parameters"),
  shinyFilesButton("picker_hab_params_file", "Select Habitat Parameters",
    "Please select the Habitat Parameters", FALSE),
  # # Predator File
  textInput("predator_file", "Predator Parameters"),
  shinyFilesButton("picker_predator_file", "Select Predator Parameters",
    "Please select the Predator Parameters", FALSE),
  # Optional area of interest
  textInput("aoi_file", "Optional Area of Interest"),
  shinyFilesButton("picker_aoi_file", "Select Area of Interest",
    "Please select the Area of Interest", FALSE),
)

interaction_tab <- tabPanel(
  "Interactions",
  uiOutput("interactions_params_ui"),
)

fish_params_tab <- tabPanel(
  "Fish Parameters",
  uiOutput("fish_params_ui"),
)

habitat_params_tab <- tabPanel(
  "Habitat Parmeters",
  uiOutput("habitat_params_ui"),
)

daily_con_hydrograph_tab <- tabPanel(
  "Hydrograph",
  value = "hydrograph",
  uiOutput("daily_hydro_ui"),
)

daily_con_link_tab <- tabPanel(
  "Link",
  value = "link",
  uiOutput("daily_link_ui"),
)

daily_con_distribution_tab <- tabPanel(
  "Distribution",
  value = "distribution",
  uiOutput("daily_dist_ui"),
)

daily_conditions_tab <- tabPanel(
  "Daily Conditions",
  tabsetPanel(
              id = "daily_type",
              type = "tabs",
              daily_con_hydrograph_tab,
              daily_con_link_tab,
              daily_con_distribution_tab,
              ),
)

predator_params_tab <- tabPanel(
  "Predator Parmeters",
  uiOutput("pred_params_ui"),
)

# No tab for fish population, it"s a large spreadsheet and would be better
# edited in a spreadsheet program

output_tab <- tabPanel(
  "Output",
  plotOutput("daily_timeseries", height = "800px"),
  plotOutput("daily_histogram", height = "500px"),
  plotOutput("fish_timeseries"),
  plotOutput("map_preview"),
)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeScript(path = "ui.js"),
  # Application title
  titlePanel("FHAST"),
  mainPanel(
    # Have a tab per input file
    # Add a save button on each tab that will update file on disk
    tabsetPanel(id = "main_tabs",
                type = "tabs",
                input_tab,
                input_file_tab,
                interaction_tab,
                fish_params_tab,
                habitat_params_tab,
                daily_conditions_tab,
                predator_params_tab,
                output_tab,
    ),
    hr(),
    actionButton("save", "Save"),
    hr(),
    actionButton("graphs", "Save and Generate Graphs"),
    hr(),
    textInput("report_name", "Report File Name"),
    actionButton("html", "Save and Generate Report"),
  )
))
