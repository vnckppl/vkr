#' Print a nice header for Emacs org-mode output
#'
#' This function prints nice headers around a string input.
#'
#' @param my_text Text that you want to highlight
#' @param hlstyle Highligh style: YELLOW, ORANGE, red, purple, blue, cyan,
#' aquamarine, green
#' @export
horg <- function(my_text,
                 hlstyle = c("my", "mo", "fr", "fp", "fb", "mc", "fa", "mg")) {

    cat(paste0("¡", hlstyle, "¡", my_text, "¡/", hlstyle, "¡"))

}
