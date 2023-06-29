library(nlrx)

#' Gets the results from running the NetLogo model.
#'
#' @return the table of results.
#' @export
run_netlogo_model <- function () {
  modelpath <- here("scripts","NetLogo","FHAST.nlogo")
  
  netLogoParams = load_text_file(here("NetLogoConfig.txt"))
  
  # Initialize NetLogo with the model
  nl <- nlrx::nl(nlversion = netLogoParams["Version", ],
                 nlpath = here(netLogoParams["NetLogoPath", ]),
                 modelpath = modelpath,
                 jvmmem = 1024)
  
  nl@experiment <- experiment(expname="modelrun",
                              outpath="",
                              repetition=1,
                              tickmetrics="false",
                              idsetup="setup",
                              idgo="go",
                              runtime=NA_integer_,
                              idfinal=NA_character_,
                              idrunnum=NA_character_,
                              variables = list("background_display" = "none"),
                              constants = list("draw_fish_movements?" = "true"),
                              metrics = c("count turtles"))
  nl@simdesign <- simdesign_simple(nl=nl,
                                   nseeds=1)
  # Evaluate nl object:
  nlrx::eval_variables_constants(nl)
  # Run all simulations (loop over all siminputrows and simseeds)
  results <- nlrx::run_nl_all(nl)
  return (results)
}