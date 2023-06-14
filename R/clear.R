#' Clear all variables in the workspace
#'
#' This function allows you to clear all variables
#' in your works space and to select specific variables
#' that you do not want to be removed.
#'
#' @param exceptions A variable or list of variables that you want to keep
#' @export
clear <- function(exceptions) {

  ## * Build list of variables to be removed
  my_list <- setdiff(ls(envir = .GlobalEnv), lsf.str())

  ## * Remove exceptions from this list
  if (!missing(exceptions)) {
    for (exc in exceptions) {
      my_list <- my_list[my_list != exc]
    }
  }

  # Remove everything in the global workspace
  rm(list = my_list, envir = .GlobalEnv)

}
