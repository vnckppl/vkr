#' Apply FDR correction to batch_lm output
#'
#' This function takes output from batch_lm and grabs all the values in the
#' columns ending in _fdr. batch_lm stores full length (not truncated) raw
#' p-values there. It creates one array of these p-values and then applies FDR
#' correction to it. After that, it stores the corrected p-values back into
#' these columns.
#'
#' @param idf Input data frame
#' @export

batch_lm_fdr <- function(idf) {

  ## ** Collect all p-values
  all_p_cols <- names(idf)[grep("fdr$", names(idf))]

  ## ** Stack returns a single column of all p-values
  ## * Only do this if there is more than 1 column
  if (length(all_p_cols) > 1) {
    all_p_stack <- stack(idf[, all_p_cols])
  } else if (length(all_p_cols) == 1) {
    all_p_stack <- as.data.frame(idf[, all_p_cols])
    names(all_p_stack) <- "values"
  }

  ## ** Apply FDR correction
  all_p_fdr <- formatC(
    round(p.adjust(all_p_stack$values, "fdr"), 4), format = "f", digits = 4)

  ## ** Return the adjusted p-values back into the data frame
  ## *** Combine adjusted p values back into stacked data frame
  all_p_stack$values <- all_p_fdr

  ## *** Reformat original df to before stack()
  ## * Only do this if there is more than 1 column
  if (length(all_p_cols) > 1) {
    all_p_fdr <- unstack(all_p_stack)
  } else if (length(all_p_cols) == 1) {
    all_p_fdr <- all_p_stack
    names(all_p_fdr) <- all_p_cols
  }

  ## *** Loop over the columns and overwrite these in the idf fdr columns
  for (col in colnames(all_p_fdr)) {
    idf[[col]] <- all_p_fdr[[col]]
  }

  ## ** Return data frame
  return(idf)

}
