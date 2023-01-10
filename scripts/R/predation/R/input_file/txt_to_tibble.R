library(tidyverse)

# reads in a comma separated .txt file without treating the first row as the column names
txt_to_tibble <- function(path){
  read_delim(path, delim = "=", col_names = FALSE) %>% 
   # mutate(X1 = str_replace_all(X1, " ", "_")) %>% 
    pivot_wider(names_from = X1, values_from = X2)
}

# pivots a wide dataframe and copies over the values again to provide param values for each species
pivot_and_clean <- function(path) {
  read_csv(path) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "species", 
                 values_to = "pikeminnow") %>% 
    filter(species != "minimum_cover") %>% 
    mutate(bass = pikeminnow)
}

# creates a long dataframe then widens it on species name
reshape_multi_spp_df <- function(path){
  read_csv(path) %>% 
    #pivot_longer(cols = -species) %>% 
    pivot_wider(names_from = "species", values_from ="estimate")

}
