#' Print a nice header for Emacs org-mode output
#'
#' This function prints nice headers around a string input.
#'
#' @param my_text Text that you want to highlight
#' @param hlstyle Highligh style: YELLOW, ORANGE, red, purple, blue, cyan,
#' aquamarine, green
#' @param numn Number of new lines (default = 0)
#' @export
horg <- function(my_text,
                 hlstyle = c("my", "mo", "fr", "fp", "fb", "mc", "fa", "mg"),
                 numn = 0) {

    ## * Build the number of new lines
    mynewlines <- paste0(rep("\n", numn), collapse = "")

    ## * Build the string and display
    writeLines(paste0("¡", hlstyle, "¡", my_text, "¡/", hlstyle, "¡", mynewlines))

}
