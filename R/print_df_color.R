#' Print a data frame with striped terminal colors
#'
#' Prints a data frame to the console using ANSI escape codes: the header line
#' is bold yellow, odd data rows are left untouched (default terminal colour),
#' and even data rows are cyan. Output is captured from \code{print()} so
#' alignment and numeric formatting match the standard data frame display.
#'
#' @param df Data frame to print
#' @param gap Number of spaces between columns (default \code{2L}); passed to
#'   \code{print()} as \code{print.gap}
#' @param ... Further arguments passed to \code{print()} (e.g. \code{digits},
#'   \code{row.names})
#'
#' @return \code{df}, invisibly
#'
#' @examples
#' print_striped(head(mtcars, 10))
#' print_striped(head(mtcars, 10), gap = 3)
#' print_striped(iris[1:8, ], digits = 3)
print_df_color <- function(df, gap = 2L, ...) {
    stopifnot(is.data.frame(df))

    bold_yellow <- "\033[1;33m"
    cyan        <- "\033[36m"
    reset       <- "\033[0m"

    # Prevent print() from wrapping wide data frames into multiple blocks,
    # which would repeat the header and break the odd/even logic
    op <- options(width = 10000L)
    on.exit(options(op), add = TRUE)

    lines <- utils::capture.output(print(df, print.gap = gap, ...))

    # Zero-row data frames: print() gives a single non-tabular line
    if (nrow(df) == 0L || length(lines) < 2L) {
        cat(bold_yellow, lines, reset, sep = "")
        cat("\n")
        return(invisible(df))
    }

    header <- lines[1L]
    body   <- lines[-1L]
    row_id <- seq_along(body)

    body_out <- ifelse(
        row_id %% 2L == 0L,
        paste0(cyan, body, reset),  # even rows: cyan
        body                        # odd rows: untouched
    )

    cat(paste0(bold_yellow, header, reset), body_out, sep = "\n")
    cat("\n")

    invisible(df)
}
