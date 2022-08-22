pick_num_cores <- function(ratio = 0.75){
  total_cores <- parallel::detectCores()
  cores_to_use <- floor(total_cores * ratio)
  if(cores_to_use <= 1){
    return(total_cores)
  } else {
    return(cores_to_use)
  }
}