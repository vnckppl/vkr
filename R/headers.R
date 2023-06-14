#' Print a nice header
#'
#' This function prints nice headers around a string input.
#'
#' @param my_text Text that you want to use for a header
#' Header 1
#' @export
h1 <- function(my_text) {
  cat(crayon::white$bgRed$bold(paste("", "\n")))
  cat(crayon::white$bgRed$bold(paste("", my_text, "\n")))
  cat(crayon::white$bgRed$bold(paste("", "\n")))
}
#' Header 2
#' @export
h2 <- function(my_text) {
  cat(crayon::white$bgCyan$bold(paste("", my_text, "\n")))
}
#' Header 3
#' @export
h3 <- function(my_text) {
  cat(crayon::white$bgBlue$bold("", my_text, ""), "\n")
}
