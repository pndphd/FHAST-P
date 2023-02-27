########################################
# Runs all the scripts for FHAST data post processing
########################################

# Make the habitat summary file
source(here("scripts", "R", "habitat_summary","scripts", "make_habitat_summary.R")) 

# Make the ABM summary file
source(here("scripts", "R", "abm_summary", "abm_output.R"))

# make the html doc
rmarkdown::render(input = here("scripts", "R", "habitat_summary","scripts", "make_habitat_summary.Rmd"),
                 output_format = "html_document",
                 output_file = here(output_folder, "report.html") )
