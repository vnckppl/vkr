#' Crop a pdf plot using inkscape
#'
#' This function removes all white space around a pdf figure. The input file
#' will be overwritten.
#'
#' @param figfile The pdf file you want to crop.
#' @export

cropplot <- function(figfile) {
    system2(
        "inkscape",
        args = c(
            "--pdf-poppler", figfile,
            "--pages=1",
            "--actions='select-all;fit-canvas-to-selection;export-type:pdf;export-text-to-path;export-do'",
            "-o",
            figfile
    ))
}
