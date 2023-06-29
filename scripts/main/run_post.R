########################################
# Runs all the scripts for FHAST data post processing
########################################

# Make the habitat summary file
source(here("scripts", "habitat_summary", "make_habitat_summary.R")) 

if (juvenile_run == TRUE){
  # Make the ABM summary file
  source(here("scripts", "abm_summary", "abm_output.R"))
}

# make the html doc
rmarkdown::render(input = here("scripts", "habitat_summary", "make_general_summary.Rmd"),
                  output_format = "html_document",
                  output_file = here(output_folder, "report_general.html") )

if (juvenile_run == TRUE){
  rmarkdown::render(input = here("scripts", "habitat_summary", "make_juvenile_summary.Rmd"),
                   output_format = "html_document",
                   output_file = here(output_folder, "report_juvenile_rearing.html") )
}

if (adult_run == TRUE){
  rmarkdown::render(input = here("scripts", "habitat_summary", "make_adult_summary.Rmd"),
                    output_format = "html_document",
                    output_file = here(output_folder, "report_adult_migration.html") )
}


################
# RUN COMPLETE #
################