#
# This is the server logic of a Shiny web application. You can run the
# application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(here)
library(data.table)
source(here("scripts", "R", "main", "load_libraries.R"))
source(here("scripts", "R", "main", "default_initialization.R"))
source(here("scripts", "R", "main", "fhast_file_functions.r"))
source(here("scripts", "R", "main", "initialize_fhast.R"))
source(here("scripts", "R", "daily_inputs", "scripts",
            "functions_make_enviro_input_file.R"))
source(here("scripts", "R", "daily_inputs", "scripts",
            "functions_make_fish_input_file.R"))
source(here("scripts","R","spatial_inputs","scripts",
            "make_preview_map_functions.R"))

print_plots <<- FALSE

habitat_params <<- data.frame(
  file_names = c(
    "benthic food density", "benthic food energy density",
    "drift food density", "drift food energy density",
    "velocity cutoff", "depth cutoff", "resolution",
    "buffer", "predators per area", "superindividual ratio"
  ),
  input_names = c(
    "hab_benth_food_dens", "hab_benth_energy_dens",
    "hab_drift_food_dens", "hab_drift_enery_dens",
    "hab_vel_cutoff", "hab_depth_cutoff", "hab_resolution",
    "hab_bufffer", "hab_pred_per_area", "hab_superindividual"
  ),
  default_values = c(1, 1, 3e-3, 1, 0.61, 0.5, 20, 600, 0.003, 1),
  input_type = c(
    "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric"
  )
)

interactions_params <<- data.frame(
  file_names = c(
    "cover velocity fraction", "percent cover intercept",
    "percent cover root", "percent cover slope",
    "percent cover 3-2 root", "distance to cover intercept",
    "distance to cover slope",
    "temperature predator area baseline",
    "predator success baseline", "turbidity intercept", "turbidity slope",
    "d84 size", "benthic velocity height"
  ),
  input_names = c(
    "inter_cover_vel_frac", "inter_perc_cov_inter",
    "inter_perc_cover_root", "inter_percent_cover_slope",
    "inter_perc_cover32_root", "inter_dist_cover",
    "inter_dist_cover_slope", "inter_temp_pred_area_base",
    "inter_pred_success_base", "inter_turb_intercept", "inter_turb_slope",
    "inter_d84_size", "inter_benthic_velocity_height"
  ),
  default_values = c(0.5, 0.477, -0.576, -0.183, 0.282, 2.197, -2.794, 1, 1,
                     -2.49, 0.0586, 0.008, 0.1),
  input_type = c(
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
  )
)

daily_hydrograph_params <<- data.frame(
  file_names = c("type", "file", "start date", "end date"),
  input_names = c(
    "daily_type", "daily_hydro_file", "daily_hydro_start",
    "daily_hydro_end"
  ),
  default_values = c("hydrograph", "daily_data.csv", "04/29/2020",
                     "06/20/2020"),
  input_type = c("tab", "file", "date", "date")
)

daily_link_params <<- data.frame(
  file_names = c(
    "type", "flow distribution", "flow mean", "flow SD",
    "flow autocorrelation", "flow change", "flow min",
    "temperature SD", "temperature correlation",
    "temperature slope", "temperature intercept", "turbidity SD",
    "turbidity correlation", "turbidity slope",
    "turbidity intercept", "start date", "end date"
  ),
  input_names = c(
    "daily_type", "daily_link_flow_dist", "daily_link_flow_mean",
    "daily_link_flow_sd", "daily_link_flow_autocor",
    "daily_link_flow_change", "daily_link_flow_min",
    "daily_link_temp_sd", "daily_link_temp_cor",
    "daily_link_temp_slope", "daily_link_temp_inter",
    "daily_link_turb_sd", "daily_link_turb_cor",
    "daily_link_turb_slope", "daily_link_turb_inter",
    "daily_link_start", "daily_link_end"
  ),
  default_values = c(
    "link", "normal", 400, 200, 0.9, 4, 100, 1, 1, -0.005, 20,
    0.2, 1, 0.001, 5, "10/03/1982", "10/01/1983"
  ),
  input_type = c(
    "tab", "text", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "date", "date"
  )
)

daily_dist_params <<- data.frame(
  file_names = c(
    "type", "flow distribution", "flow mean", "flow SD",
    "flow autocorrelation", "flow change", "flow min",
    "temperature distribution", "temperature mean",
    "temperature SD", "temperature autocorrelation",
    "temperature change", "turbidity distribution",
    "turbidity mean", "turbidity SD",
    "turbidity autocorrelation", "turbidity change", "start date",
    "end date"
  ),
  input_names = c(
    "daily_type", "daily_dist_flow_dist", "daily_dist_flow_mean",
    "daily_dist_flow_sd", "daily_dist_flow_autocor",
    "daily_dist_flow_change", "daily_dist_flow_min",
    "daily_dist_temp_dist", "daily_dist_temp_mean",
    "daily_dist_temp_sd", "daily_dist_temp_autocor",
    "daily_dist_temp_change", "daily_dist_turb_dist",
    "daily_dist_turb_mean", "daily_dist_turb_sd",
    "daily_dist_turb_autocor", "daily_dist_turb_change",
    "daily_dist_start", "daily_dist_end"
  ),
  default_values = c(
    "distribution", "normal", 400, 200, 0.9, 0, 0.1, "normal",
    12, 2, 0.9, 0, "normal", 4, 1, 0.9, 0, "10/03/1982",
    "10/30/1982"
  ),
  input_type = c(
    "tab", "text", "numeric", "numeric", "numeric", "numeric",
    "numeric", "text", "numeric", "numeric", "numeric", "numeric",
    "text", "numeric", "numeric", "numeric", "numeric", "date",
    "date"
  )
)

fish_params_df <<- NULL
pred_params_df <<- NULL

# Define server logic
shinyServer(function(input, output, session) {
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })

  has_rendered_daily_conditions <<- FALSE

  # The config file can be anywhere
  volumes <- getVolumes()
  handle_absolute_file_picker(
    input, session, "picker_config_file", "config_file",
    volumes(), "txt", config_file_path
  )

  # Use a base directory of the config file for everything else.
  picker_roots <- c(fhast_base_folder)
  names(picker_roots) <- c("fhast_base_folder")

  output$habitat_params_ui <- renderUI({
    render_and_load_tab(
      habitat_params, input$hab_params_file, session, input,
      picker_roots
    )
  })

  output$interactions_params_ui <- renderUI({
    render_and_load_tab(
      interactions_params, input$interaction_file, session, input,
      picker_roots
    )
  })

  output$daily_hydro_ui <- renderUI({
    render_daily_tab(
      daily_hydrograph_params, input$daily_file, session, input,
      picker_roots
    )
  })

  output$daily_link_ui <- renderUI({
    render_daily_tab(
      daily_link_params, input$daily_file, session, input,
      picker_roots
    )
  })

  output$daily_dist_ui <- renderUI({
    render_daily_tab(
      daily_dist_params, input$daily_file, session, input,
      picker_roots
    )
  })

  handle_folder_picker(
    input, session, "picker_raster_folder", "raster_folder",
    picker_roots, raster_folder
  )

  handle_file_picker(
    input, session, "picker_daily_file", "daily_file",
    picker_roots, "txt", daily_path
  )
  handle_file_picker(
    input, session, "picker_fish_pop_file", "fish_pop_file",
    picker_roots, "csv", fish_population_path
  )
  handle_file_picker(
    input, session, "picker_fish_params_file",
    "fish_params_file", picker_roots, "csv", fish_parameters_path
  )
  handle_file_picker(
    input, session, "picker_grid_center_file",
    "grid_center_file", picker_roots, "shp", grid_center_line_path
  )
  handle_file_picker(
    input, session, "picker_grid_top_file", "grid_top_file",
    picker_roots, "shp", grid_top_marker_path
  )
  handle_file_picker(
    input, session, "picker_interaction_file", "interaction_file",
    picker_roots, "txt", interaction_path
  )
  handle_file_picker(
    input, session, "picker_cover_file", "cover_file",
    picker_roots, "shp", cover_path
  )
  handle_file_picker(
    input, session, "picker_canopy_cover_file",
    "canopy_cover_file", picker_roots, "shp", canopy_path
  )
  handle_file_picker(
    input, session, "picker_hab_params_file", "hab_params_file",
    picker_roots, "txt", hab_path
  )
  handle_file_picker(
    input, session, "picker_predator_file", "predator_file",
    picker_roots, "txt", predator_path
  )
  handle_file_picker(
    input, session, "picker_aoi_file", "aoi_file", picker_roots,
    "shp", aoi_path
  )

  handle_file_picker(
    input, session, "picker_daily_con_hydro_file",
    "daily_con_hydro_file", picker_roots, "csv", "C:\tempFile.csv"
  )

  fish_params <<- reactiveValues(update = 0)
  pred_params <<- reactiveValues(update = 0)

  # Run button clicked
  observeEvent(input$graphs, {
    do_save(input)
    
    source(here("scripts","R","parameters","load_convert_parameters.R"))

    # Initialize fhast variables to the inputs in the UI.
    initialize_fhast(input$config_file)
    updateTabsetPanel(
      session = session, inputId = "main_tabs",
      selected = "Output"
    )
    output_data <- load_daily_conditions(daily_inputs)
    fish_schedule <- load_fish_timeseries(fish_daily_inputs)
    # Make the plots
    output$daily_timeseries <- renderPlot({
      make_daily_timeseries_plot(output_data)
    })
    output$daily_histogram <- renderPlot({ 
      make_daily_histogram_plot(output_data)
    })
    output$fish_timeseries <- renderPlot({
      plot_fish_timeseries(fish_schedule)
    })
    output$map_preview <- renderPlot({
      make_map_plot()
    })
  })

  observeEvent(input$html, {
    do_save(input)

    # Initialize fhast variables to the inputs in the UI.
    initialize_fhast(input$config_file)

    source(here("scripts","R","parameters","load_convert_parameters.R"))
    # Run the model
    source(here("scripts", "R", "main", "run_model.R"))
    source(here("scripts", "R", "habitat_summary", "scripts",
                "make_habitat_summary.R"))

    report_name <- input$report_name
    if (!nzchar(report_name)) {
      report_name <- "FHAST_report"
    }
    # Generate reports
    rmarkdown::render(
      here(
        "scripts", "R", "habitat_summary", "scripts",
        "make_habitat_summary.Rmd"
      ),
      output_format = "html_document", output_dir = fhast_base_folder,
      output_file = report_name
    )
  })

  # Save button clicked
  observeEvent(input$save, {
    do_save(input)
  })

  observeEvent(input$config_file, {
    load_config(input$config_file, session)
  })

  observeEvent(input$interaction_file, {
    load_tab_file(input$interaction_file, session, interactions_params)
  })

  observeEvent(input$fish_params_file, {
    fish_params_df <<- load_csv_file(input$fish_params_file)
    fish_params$update <<- fish_params$update + 1
  })

  observeEvent(input$hab_params_file, {
    load_tab_file(input$hab_params_file, session, habitat_params)
  })

  observeEvent(input$daily_file, {
    update_daily_conditions_ui(input$daily_file, session)
  })

  observeEvent(input$predator_file, {
    pred_params_df <<- load_csv_file(input$predator_file)
    pred_params$update <<- pred_params$update + 1
  })

  observeEvent(fish_params$update, {
    output$fish_params_ui <- renderUI({
      fish_ui_list <- render_csv(
        input, fish_params_df, "fish_params",
        fish_params_delete_col, fish_params_set_value
      )
      tagList(
        fish_ui_list,
        # Text input and button for adding a new species
        textInput("fishParamsAddSpecies", "Species Name", ""),
        actionButton("fishParamsAddButton", "Add Fish"),
      )
    })
  })

  # Handling add species button click
  observeEvent(input$fishParamsAddButton, {
    if (nzchar(input$fishParamsAddSpecies)) {
      fish_params_df[input$fishParamsAddSpecies] <<-
        numeric(nrow(fish_params_df))
      fish_params$update <<- fish_params$update + 1
    }
  })

  observeEvent(pred_params$update, {
    output$pred_params_ui <- renderUI({
      pred_ui_list <- render_csv(
        input, pred_params_df, "pred_params",
        pred_params_delete_col, pred_params_set_value
      )
      tagList(
        pred_ui_list,
        # Text input and button for adding a new species
        textInput("predParamsAddSpecies", "Species Name", ""),
        actionButton("predParamsAddButton", "Add Predator"),
      )
    })
  })

  # Handling add species button click
  observeEvent(input$predParamsAddButton, {
    if (nzchar(input$predParamsAddSpecies)) {
      pred_params_df[input$predParamsAddSpecies] <<-
        numeric(nrow(pred_params_df))
      pred_params$update <<- pred_params$update + 1
    }
  })
})

render_and_load_tab <- function(tab_data, file_path, session, input,
                                picker_roots) {
  ui_list <- render_tab(tab_data, file_path, session, input, picker_roots)
  # Because the tab is rendered reactively, the file has likely already been
  # loaded. Once we render the tab, reload the file values so the correct values
  # are displayed.
  load_tab_file(file_path, session, tab_data)
  return(ui_list)
}

render_daily_tab <- function(tab_data, file_path, session, input,
                             picker_roots) {
  ui_list <- render_tab(tab_data, file_path, session, input, picker_roots)
  # Because the tab is rendered reactively, the file has likely already been
  # loaded. Once we render the tab, reload the file values so the correct values
  # are displayed.
  update_daily_conditions_ui(file_path, session)
  has_rendered_daily_conditions <<- TRUE
  return(ui_list)
}

render_tab <- function(tab_data, file_path, session, input, picker_roots) {
  ui_list <- list()
  for (i in rownames(tab_data)) {
    if (tab_data[i, "input_type"] == "numeric") {
      ui_list <- list(
        ui_list,
        numericInput(
          tab_data[i, "input_names"],
          str_to_title(tab_data[i, "file_names"]),
          tab_data[i, "default_values"]
        )
      )
    }
    if (tab_data[i, "input_type"] == "text") {
      ui_list <- list(
        ui_list,
        textInput(tab_data[i, "input_names"],
          str_to_title(tab_data[i, "file_names"]),
          value = tab_data[i, "default_values"]
        )
      )
    }
    if (tab_data[i, "input_type"] == "date") {
      ui_list <- list(
        ui_list,
        dateInput(tab_data[i, "input_names"],
          str_to_title(tab_data[i, "file_names"]),
          format = "mm/dd/yyyy"
        )
      )
    }
    if (tab_data[i, "input_type"] == "file") {
      title <- str_to_title(tab_data[i, "file_names"])
      input_name <- tab_data[i, "input_names"]
      picker_name <- paste0("picker_", input_name)
      ui_list <- list(
        ui_list,
        textInput(input_name, title,
          value = tab_data[i, "default_values"]
        ),
        shinyFilesButton(
          picker_name,
          paste0("Select ", title),
          paste0("Please select the ", title), FALSE
        )
      )
      handle_file_picker(
        input, session, picker_name, input_name, picker_roots, "csv",
        tab_data[i, "default_values"]
      )
    }
  }
  return(tagList(ui_list))
}

save_tab_file <- function(input, tab_data, file_path) {
  values_vector <- vector(mode = "character")
  for (i in rownames(tab_data)) {
    name <- tab_data[i, "input_names"]
    if (tab_data[i, "input_type"] == "numeric" ||
      tab_data[i, "input_type"] == "text" ||
      tab_data[i, "input_type"] == "tab" ||
      tab_data[i, "input_type"] == "file") {
      values_vector <- append(values_vector, input[[name]])
    }
    if (tab_data[i, "input_type"] == "date") {
      values_vector <- append(values_vector, format(input[[name]], "%m/%d/%Y"))
    }
  }

  if (length(values_vector) == 0) {
    # If the user hasn't navigated to the tab, then the inputs won't be loaded
    # and this will have no data to save. Skip saving the file. This is fine
    # because if the tab hasn't been loaded in the UI the user will not have
    # made any changes.
    return()
  }

  obj <- data.frame(
    names = tab_data$file_names,
    values = values_vector
  )
  save_text_file(file_path, obj)
}



update_ui_with_file <- function(session, tab_data, file_data) {
  for (i in rownames(tab_data)) {
    if (tab_data[i, "input_type"] == "numeric") {
      updateNumericInput(session, tab_data[i, "input_names"],
        value = as.numeric(
          file_data[tab_data[i, "file_names"], ]
        )
      )
    }
    if (tab_data[i, "input_type"] == "text") {
      updateTextInput(session, tab_data[i, "input_names"],
        value = file_data[tab_data[i, "file_names"], ]
      )
    }
    if (tab_data[i, "input_type"] == "date") {
      updateDateInput(session, tab_data[i, "input_names"],
        value = as.Date(
          file_data[tab_data[i, "file_names"], ], "%m/%d/%Y"
        )
      )
    }
    if (tab_data[i, "input_type"] == "tab") {
      updateTabsetPanel(
        session = session, inputId = tab_data[i, "input_names"],
        selected = file_data[tab_data[i, "file_names"], ]
      )
    }
  }
}

load_tab_file <- function(file_path, session, tab_data) {
  file_data <- load_text_file(file_path)

  if (is.null(file_data)) {
    return()
  }

  update_ui_with_file(session, tab_data, file_data)
}

handle_file_picker <- function(input, session, picker_name, txt_box_name,
                               picker_roots, file_type, default) {
  rel_path <- make_fhast_relative_path(default)
  updateTextInput(session, txt_box_name, value = rel_path)
  observe({
    shinyFileChoose(input, picker_name,
      roots = picker_roots,
      filetypes = c(file_type)
    )
    selected_file <- parseFilePaths(picker_roots, input[[picker_name]])
    if (length(selected_file$datapath) > 0) {
      rel_path <- make_fhast_relative_path(unname(selected_file$datapath))
      updateTextInput(session, txt_box_name, value = rel_path)
    }
  })
}

handle_folder_picker <- function(input, session, picker_name, text_box_name,
                                 picker_roots, default) {
  rel_path <- make_fhast_relative_path(default)
  updateTextInput(session, text_box_name, value = rel_path)
  observe({
    shinyDirChoose(input, picker_name, roots = picker_roots)
    selected_dir <- parseDirPath(picker_roots, input[[picker_name]])
    if (length(selected_dir) > 0) {
      rel_path <- make_fhast_relative_path(selected_dir)
      updateTextInput(session, text_box_name, value = rel_path)
    }
  })
}

do_save <- function(input) {
  save_config(input)
  save_tab_file(input, interactions_params, input$interaction_file)
  save_csv_file(fish_params_df, input$fish_params_file)
  save_tab_file(input, habitat_params, input$hab_params_file)
  save_daily_conditions(input)
  save_csv_file(pred_params_df, input$predator_file)
}

save_config <- function(input) {
  write_config_file(
    file_path, input$fish_pop_file, input$daily_file,
    input$fish_params_file, input$grid_center_file,
    input$grid_top_file, input$cover_file, input$canopy_cover_file,
    input$hab_params_file, input$interaction_file, input$predator_file,
    input$raster_folder, input$aoi_file
  )
}

load_config <- function(config_file, session) {
  if (file.exists(config_file) && !dir.exists(config_file)) {
    # When the directory is updated, run the initialization which loads the
    # config file.
    initialize_fhast(config_file)
    # After we"ve loaded the config file, update all of the inputs to show
    # the proper values.
    updateTextInput(session, "raster_folder",
      value = make_fhast_relative_path(raster_folder)
    )
    updateTextInput(session, "daily_file",
      value = make_fhast_relative_path(daily_path)
    )
    updateTextInput(session, "fish_pop_file",
      value = make_fhast_relative_path(fish_population_path)
    )
    updateTextInput(session, "fish_params_file",
      value = make_fhast_relative_path(fish_parameters_path)
    )
    updateTextInput(session, "grid_center_file",
      value = make_fhast_relative_path(grid_center_line_path)
    )
    updateTextInput(session, "grid_top_file",
      value = make_fhast_relative_path(grid_top_marker_path)
    )
    updateTextInput(session, "interaction_file",
      value = make_fhast_relative_path(interaction_path)
    )
    updateTextInput(session, "cover_file",
      value = make_fhast_relative_path(cover_path)
    )
    updateTextInput(session, "canopy_cover_file",
      value = make_fhast_relative_path(canopy_path)
    )
    updateTextInput(session, "hab_params_file",
      value = make_fhast_relative_path(hab_path)
    )
    updateTextInput(session, "predator_file",
      value = make_fhast_relative_path(predator_path)
    )
  }
}

save_daily_conditions <- function(input) {
  if (!has_rendered_daily_conditions) {
    return()
  }
  switch(input$daily_type,
    Hydrograph = save_tab_file(
      input, daily_hydrograph_params,
      input$daily_file
    ),
    Link = save_tab_file(input, daily_link_params, input$daily_file),
    Distribution = save_tab_file(input, daily_dist_params, input$daily_file),
  )
}

update_daily_conditions_ui <- function(file_path, session) {
  # read file helper (share with load tab)
  file_data <- load_text_file(file_path)

  if (is.null(file_data)) {
    return()
  }

  selected_tab <- "link"

  # Update UI with the tab data and set the selected_tab value.
  switch(file_data["type", ],
    hydrograph = {
      update_ui_with_file(session, daily_hydrograph_params, file_data)
      selected_tab <- "hydrograph"
    },
    link = {
      update_ui_with_file(session, daily_link_params, file_data)
      selected_tab <- "link"
    },
    distribution = {
      update_ui_with_file(session, daily_dist_params, file_data)
      selected_tab <- "distribution"
    },
  )

  # select the active tab
  updateTabsetPanel(
    session = session, inputId = "daily_type",
    selected = selected_tab
  )
}

render_csv <- function(input, data, csv_identifier, df_delete_col,
                       df_update_value) {
  row_names <- row.names(data)
  col_names <- colnames(data)
  ui_list <- list()
  # foreach column in the parameters
  for (col_name in col_names) {
    # Show column name
    ui_list <- list(ui_list, h1(col_name))
    # Delete button for column
    ui_list <- list(
      ui_list,
      csv_delete_col_button(
        input, csv_identifier, col_name,
        df_delete_col
      )
    )
    # foreach row in the parameters
    for (row in row_names) {
      # Numeric input for that species+row
      ui_list <- list(
        ui_list,
        csv_handle_numeric_param(
          input, csv_identifier, col_name, row,
          df_update_value, data[row, col_name]
        )
      )
    }
    # Visual separator between fish species
    ui_list <- list(ui_list, hr())
  }
  return(ui_list)
}

fish_params_delete_col <- function(species) {
  drops <- c(species)
  fish_params_df <<- fish_params_df[, !(names(fish_params_df) %in% drops),
    drop = FALSE
  ]
  fish_params$update <<- fish_params$update + 1
}

pred_params_delete_col <- function(species) {
  drops <- c(species)
  pred_params_df <<- pred_params_df[, !(names(pred_params_df) %in% drops),
    drop = FALSE
  ]
  pred_params$update <<- pred_params$update + 1
}

fish_params_set_value <- function(row, species, value) {
  fish_params_df[row, species] <<- value
}

pred_params_set_value <- function(row, species, value) {
  pred_params_df[row, species] <<- value
}

csv_delete_col_button <- function(input, csv_identifier, col_name,
                                  df_delete_col) {
  delete_button_id <- paste0(csv_identifier, "_delete_", col_name)
  observeEvent(input[[delete_button_id]], {
    df_delete_col(col_name)
  })
  return(actionButton(delete_button_id, paste0("Delete ", col_name)))
}

csv_handle_numeric_param <- function(input, csv_identifier, col_name, row_name,
                                     df_update_value, cur_value) {
  numeric_input_name <- paste0(csv_identifier, "_", col_name, row_name)
  observeEvent(input[[numeric_input_name]], {
    df_update_value(row_name, col_name, input[[numeric_input_name]])
  })
  return(numericInput(numeric_input_name, row_name, cur_value))
}

handle_absolute_file_picker <- function(input, session, picker_name,
                                        txt_box_name, picker_roots, file_type,
                                        default) {
  updateTextInput(session, txt_box_name, value = default)
  observe({
    shinyFileChoose(input, picker_name,
      roots = picker_roots,
      filetypes = c(file_type)
    )
    selected_file <- parseFilePaths(picker_roots, input[[picker_name]])
    if (length(selected_file$datapath) > 0) {
      updateTextInput(session, txt_box_name,
        value = unname(selected_file$datapath)
      )
    }
  })
}
