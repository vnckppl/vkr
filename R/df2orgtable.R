#' Convert a data frame to an org table and print it
#'
#' @param df Name of a data element in your working environment
#' @export
df2orgtable <- function(df) {

    ## * Header of the data frame
    my_hdr <- paste(names(df), collapse = "|") |>
        gsub("^", "|", x = _) |> gsub("$", "|", x = _)

    ## * Body of the data frame
    my_body <- apply(df[, names(df)], 1, paste, collapse = "|") |>
        gsub("^", "|", x = _) |> gsub("$", "|", x = _)

    ## * Combine data
    my_out <- c(my_hdr, "|-", my_body)

    ## * Print
    cat(my_out, sep = "\n")
    ## return(my_out)

}
