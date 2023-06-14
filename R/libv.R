#' Display data in Libreoffice from R
#'
#' This function loads a dataframe in Libreoffice
#'
#' @param x Name of a data element in your working environment
#' @param filepath Path to data file
#' @export
libv <- function(x, filepath = NULL) {

    ## * Parse arguments
    dtstamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ## if (length(filepath) == 0) {
    ##     filepath <- paste0(
    ##         Sys.getenv("HOME"), "/Downloads/tmpR_", dtstamp, ".xlsx"
    ##     )
    ## }
    if (length(filepath) == 0) {
        filepath <- tempfile(paste0("tmpR_", dtstamp), fileext = ".xlsx")
    }

    if (length(dim(x)) > 2) {
        stop("Too many dimensions")
    }
    if (is.null(dim(x))) {
        x <- as.data.frame(x)
    }
    if (is.null(rownames(x))) {
        tmp <- seq_len(nrow(x))
    }else {
        tmp <- rownames(x)
    }
    rownames(x) <- NULL
    x <- data.frame(RowLabels = tmp, x)
    writexl::write_xlsx(x, path = filepath)
    system(paste("LD_LIBRARY_PATH= loffice", filepath, "&"))
}
