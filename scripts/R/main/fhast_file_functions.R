########################################
# This is a set of functions communally used in FHAST
########################################

# Make a directory if it's missing for a file path
make_dir_if_missing <- function(file_path) {
  directory <- dirname(file_path)
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
}

# Check if something is an absolute path and get the path
get_path <- function(base_folder, input_path) {
  path <- ifelse(isAbsolutePath(input_path),
                 input_path,
                 here(base_folder, input_path)
  )
  return(path)
}

# Function that not used just returnes ascolute path
make_fhast_relative_path <- function(absolute_path) {
  if (grepl(fhast_base_folder, absolute_path, fixed = TRUE)) {
    relative_path <- str_remove(absolute_path, fhast_base_folder)
    # Removing leading '/'
    if (str_starts(relative_path, "/")) {
      relative_path <- str_remove(relative_path, "/")
    }
    return(relative_path)
  }
  # This wasn't clearly a path contained within the base folder,
  # just return the "absolute path".
  return(absolute_path)
}

# Check to see if the file exists
check_file_exists = function(file_path){
  if (!file.exists(file_path)) {
    stop(paste0("\nThe input file\n", file_path, "\ndoes not exist."))
  }
}

# Load the text files with equals in them 
load_text_file <- function(file_path) {
  file_path <- get_path(fhast_base_folder, file_path)
  if (!file.exists(file_path) || dir.exists(file_path)) {
    return(NULL)
  }
  file_data <- read.csv(
    file = file_path,
    sep = "=",
    row.names = 1,
    header = FALSE
  ) %>%
    # Trim off white spaces form values
    rename(value = 1) %>%
    mutate(value = str_trim(value, side = c("both")))
  
  return(file_data)
}

# Save a text file
save_text_file <- function(file_path, obj) {
  file_path <- get_path(fhast_base_folder, file_path)
  make_dir_if_missing(file_path)
  fwrite(obj,
         file = file_path,
         sep = "=",
         row.names = FALSE,
         col.names = FALSE
  )
}

# Save a CSV file
save_csv_file <- function(data, file_path) {
  if (is.null(data)) {
    return()
  }
  out <- rbind(species = colnames(data), data)
  
  file_path <- get_path(fhast_base_folder, file_path)
  make_dir_if_missing(file_path)
  fwrite(out,
         file = file_path,
         sep = ",",
         row.names = TRUE,
         col.names = FALSE
  )
}

# Load a csv
load_csv_file <- function(file_path) {
  file_path <- get_path(fhast_base_folder, file_path)
  if (!file.exists(file_path) || dir.exists(file_path)) {
    return()
  }
  
  data <- read.csv(
    file = file_path,
    sep = ",",
    header = TRUE,
    row.names = 1
  )
  return(data)
}

# Grab a number for a df that stores the number as a char
# function to get values form list
get_num <- function(df, name) {
  value <- as.numeric(df[name, ])
}

# calculate the logistic function in the style of inSALMO
calc_logistic <- function(parm_10 = NULL,
                          parm_90 = NULL,
                          value = NULL){
  log_d = log(0.9/0.1)
  log_c = log(0.1/0.9)
  log_b = (log_c - log_d)/(parm_10 - parm_90)
  log_a = log_c - (log_b * parm_10)
  z = log_a + (log_b * value)
  s = exp(z)/(1 + exp(z))
  return(s)
}

store_last_run_hashes <- function(file_path, files) {
  obj <- data.frame(
    files = files,
    hashes = sapply(files, md5sum)
  )
  save_text_file(file_path, obj)
}

compare_last_run_hashes <- function(file_path, files) {
  if (!file.exists(file_path)) {
    return(FALSE)
  }
  # Load the hash values from the last run
  obj <- load_text_file(file_path)

  # Verify files match
  if (any(row.names(obj) != files)) {
    return(FALSE)
  }

  # verify md5 matches
  if (any(sapply(files, md5sum)!=obj)) {
    return(FALSE)
  }

  return(TRUE)
}