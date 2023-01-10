params_pivot_longer <- function(df) {
  df %>% 
    pivot_longer(cols = -term, names_to = "species", values_to = "estimate") %>% 
    select(species, term, estimate) %>% 
    arrange(species)
}

params_pivot_wider <- function(df) {
  df %>% 
    select(1:2) %>% 
    pivot_wider(names_from = term, values_from = pikeminnow)
}

write_rds_temp_folder <- function(df, filename, subfolder = "NetLogo") {
  write_rds(df, here(temp_folder, subfolder, filename))
}

select_model_param <- function(df, param) {
  df %>% 
    filter(name == param) %>% 
    pull(value) %>% as.numeric()
}

make_variables <- function(df, filename) {
  do.call("<<-",list(filename,df))
}
