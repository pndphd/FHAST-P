#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(here)
library(data.table)
source(here("scripts","R","main","load_libraries.R"))
source(here("scripts","R","main","default_initialization.R"))
source(here("scripts","R","main","initialize_fhast.R"))


# Define server logic
shinyServer(function(input, output, session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  #input_root = c(config_dir=input_folder)
  volumes = getVolumes()
  handleFolderPicker(input, session, 'picker_config_dir', 'config_dir', volumes(), input_folder)
  handleFolderPicker(input, session, 'picker_input_dir', 'input_dir', volumes(), project_folder)
  handleFolderPicker(input, session, 'picker_raster_folder', 'raster_folder', volumes(), raster_folder)
  handleFolderPicker(input, session, 'picker_hydrology_folder', 'hydrology_folder', volumes(), hydrology_folder)
  
  handleFilePicker(input, session, 'picker_daily_file', "daily_file", volumes(), 'txt', daily_file_name)
  handleFilePicker(input, session, 'picker_fish_pop_file', "fish_pop_file", volumes(), 'csv', fish_population_file)
  handleFilePicker(input, session, 'picker_fish_params_file', "fish_params_file", volumes(), 'csv', fish_parameters_file)
  handleFilePicker(input, session, 'picker_grid_center_file', "grid_center_file", volumes(), 'shp', grid_center_line)
  handleFilePicker(input, session, 'picker_grid_top_file', "grid_top_file", volumes(), 'shp', grid_top_marker)
  handleFilePicker(input, session, 'picker_grid_res_file', "grid_res_file", volumes(), 'txt', grid_res_path)
  handleFilePicker(input, session, 'picker_cover_file', "cover_file", volumes(), 'shp', cover_file)
  handleFilePicker(input, session, 'picker_canopy_cover_file', "canopy_cover_file", volumes(), 'shp', canopy_cover_file)
  handleFilePicker(input, session, 'picker_hab_params_file', "hab_params_file", volumes(), 'txt', hab_path)
  
  # Run button clicked
  observeEvent(input$run, {
    doSave(input)
    
    # Initialize fhast variables to the inputs in the UI.
    initialize_fhast(input$config_dir)
    # Run the model
    source(here("scripts","R","main","run_model.R"))
  })
  
  # Save button clicked
  observeEvent(input$save, {
    doSave(input)
  })
})

handleFilePicker <- function(input, session, picker_name, txt_box_name, picker_roots, file_type, default) {
  updateTextInput(session, txt_box_name, value = default)
  observe({
    shinyFileChoose(input, picker_name, roots=picker_roots, filetypes=c(file_type))
    selected_file = parseFilePaths(picker_roots, input[[picker_name]])
    if (length(selected_file$datapath) > 0) {
      updateTextInput(session, txt_box_name, value = unname(selected_file$datapath))
    }
  })
}

handleFolderPicker <- function(input, session, picker_name, text_box_name, picker_roots, default) {
  updateTextInput(session, text_box_name, value = default)
  observe({
    shinyDirChoose(input, picker_name, roots=picker_roots)
    selected_dir = parseDirPath(picker_roots, input[[picker_name]])
    if (length(selected_dir) > 0) {
      updateTextInput(session, text_box_name, value = selected_dir)
    }
  })
}

doSave <- function(input) {
  obj = data.frame(names=c("folder", "fish population file", "daily file",
                           "fish parameters file", "line", "point",
                           "grid resolution file", "cover file", "canopy cover",
                           "habitat parameters file", "raster folder", "hydrology folder"),
                   paths=c(input$input_dir, input$fish_pop_file, input$daily_file,
                           input$fish_params_file, input$grid_center_file, input$grid_top_file,
                           input$grid_res_file, input$cover_file, input$canopy_cover_file,
                           input$hab_params_file, input$raster_folder, input$hydrology_folder))
  
  fwrite(obj, file = here(input$config_dir, fhast_config_file),
         sep = "=",
         row.names = FALSE,
         col.names = FALSE)
}
