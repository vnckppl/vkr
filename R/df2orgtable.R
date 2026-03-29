#' Convert a data frame to an org table and print it
#'
#' @param df Name of a data element in your working environment
#' @param colorizep When set to TRUE, automatically color columns ending in 'p'
#' and 'fdr' according the the threshold. If set to a list with one or two
#' arrays, the first array will be used as the p-value columns and the second
#' array will be used as fdr p-value columns.
#' @param thold Threshold for coloring
#' @param rownamesout If set to TRUE, print rownames in output table. If set to
#' a string, use this as the header of the rownames column and include it in the
#' output
#' @param verbose Show messages
#' @examples
#' df2orgtable(df,
#'             colorizep = list(c("col1", "col2"), c("col3", "col4")),
#'             thold = 0.05)
#' @export
df2orgtable <- function(
                        df,
                        colorizep = TRUE,
                        thold = 0.05,
                        rownamesout = FALSE,
                        verbose = FALSE
                        ) {

    ## * Colorize p-values
    ## ** Raw p-values
    if (isTRUE(colorizep)) {
        ## *** Find columns ending in 'p'
        pcols <- colnames(df)[grep("p$", colnames(df))]
    } else if (is.list(colorizep)) {
        ## *** Select p columns from user input
        pcols <- colorizep[[1]]
    }
    if (length(pcols) == 0 && verbose) print("No p-value columns found")

    ## *** Loop over p-value columns
    for (npcol in seq_len(length(pcols))) {

        ## **** Put information into variables
        pcol <- pcols[npcol]   # Column name

        ## **** Define org-mode prefix and postfix color syntax
        prefix <- paste0("¡to", npcol, "¡")
        postfix <- paste0("¡/to", npcol, "¡")

        ## **** Apply the coloring to the columns
        # Color when p < threshold and when p is not NA
        df[[pcol]][df[[pcol]] < thold & !is.na(df[[pcol]])] <-
            paste0(prefix, df[[pcol]][df[[pcol]] <
                                      thold & !is.na(df[[pcol]])], postfix)

        ## **** Also colorize the first two columns
        # Colorize the first two columns if the pvalue for that row is
        # surpassing the treshold. Make sure to only do this once (so to
        # not color the same variable twice, because mutiple p-values on
        # the row are surpassing the threshold).
        ## ***** Outcome measure
        df[[1]][!is.na(df[[pcol]]) & df[[pcol]] < thold & !grepl("¡", df[[1]])] <-
            paste0(
                "¡ty1¡",
                df[[1]][!is.na(df[[pcol]]) & df[[pcol]] < thold & !grepl("¡", df[[1]])],
                "¡/ty1¡")

        ## ***** Predictor
        df[[2]][!is.na(df[[pcol]]) & df[[pcol]] < thold & !grepl("¡", df[[2]])] <-
            paste0(
                "¡ty2¡",
                df[[2]][!is.na(df[[pcol]]) & df[[pcol]] < thold & !grepl("¡", df[[2]])],
                "¡/ty2¡")
    }

    ## ** FDR corrected p-values
    if (isTRUE(colorizep)) {
        ## *** Find columns ending in 'fdr'
        fdrcols <- colnames(df)[grep("fdr$", colnames(df))]
    } else if (is.list(colorizep)) {
        ## *** Select p columns from user input
        fdrcols <- colorizep[[2]]
    }
    if (length(fdrcols) == 0 && verbose) print("No FDR columns found")

    ## *** Loop over p-value columns
    for (nfdrcol in seq_len(length(fdrcols))) {

        ## **** Put information into variables
        fdrcol <- fdrcols[nfdrcol] # Column name

        ## **** Define org-mode prefix and postfix color syntax
        prefix <- paste0("¡tr", nfdrcol, "¡")
        postfix <- paste0("¡/tr", nfdrcol, "¡")

        ## **** Apply the coloring to the columns
        # Color when FDR p < threshold and when FDR p is not NA
        df[[fdrcol]][df[[fdrcol]] < thold & !is.na(df[[fdrcol]])] <-
            paste0(prefix, df[[fdrcol]][df[[fdrcol]] <
                                        thold & !is.na(df[[fdrcol]])], postfix)
    }

    ## * Include rownames column if requested
    if (isTRUE(rownamesout)) {
        df <- cbind(rownames(df), df)
        names(df)[1] <- "rowname"
    }

    ## * Header of the data frame
    my_hdr <- paste(names(df), collapse = "|") |>
        gsub("^", "|", x = _) |>
        gsub("$", "|", x = _)

    ## * Body of the data frame
    my_body <- apply(df[, names(df)], 1, paste, collapse = "|") |>
        gsub("^", "|", x = _) |>
        gsub("$", "|", x = _)

    ## * Combine data
    my_out <- c(my_hdr, "|-", my_body)

    ## * Print
    cat(my_out, sep = "\n")

}
