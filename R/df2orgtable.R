#' Convert a data frame to an org table and print it
#'
#' @param df Name of a data element in your working environment
#' @param colorizep Find columns ending in 'p' or 'fdr' and color the values
#' smaller than the threshold
#' @param thold Threshold for coloring
#' @export
df2orgtable <- function(df, colorizep = FALSE, thold = 0.05) {

    ## * Colorize p-values
    ## If set to auto: find p and fdr columsn and color them according to the
    ## threshold. This only if there are 3 or less p-value columns because I
    ## only defined three unique colors in org (this is an org limitation).
    if (colorizep == "auto") {

        ## ** Raw p-values
        ## *** Find columns ending in 'p'
        pcols <- colnames(df)[grep("p$", colnames(df))]
        if (length(pcols) == 0) print("No p-value columns found")

        ## *** Loop over p-value columns
        pcolors <- c("to1", "to2", "to3")
        for (npcol in seq_len(length(pcols))) {
            
            ## **** Put information into variables
            pcol <- pcols[npcol]   # Column name
            tcol <- pcolors[npcol] # Color to be used

            ## **** Define org-mode prefix and postfix color syntax
            prefix <- paste0("¡", tcol, "¡")
            postfix <- paste0("¡/", tcol, "¡")

            ## **** Apply the coloring to the columns 
            df[[pcol]][df[[pcol]] < thold] <-
                paste0(prefix, df[[pcol]][df[[pcol]] < thold], postfix)

            ## **** Also colorize the first two columns
            # Colorize the first two columns if the pvalue for that row is
            # surpassing the treshold.
            df[[1]][df[[pcol]] < thold] <-
                   paste0("¡ty1¡", df[[1]][df[[pcol]] < thold], "¡/ty1¡")
            df[[2]][df[[pcol]] < thold] <-
                   paste0("¡ty2¡", df[[2]][df[[pcol]] < thold], "¡/ty2¡")
        }

        ## ** FDR corrected p-values
        ## *** Find columns ending in 'fdr'
        fdrcols <- colnames(df)[grep("fdr$", colnames(df))]
        if (length(fdrcols) == 0) print("No FDR columns found")

        ## *** Loop over p-value columns
        fdrcolors <- c("tr1", "tr2", "tr3")
        for (nfdrcol in seq_len(length(fdrcols))) {

            ## **** Put information into variables
            fdrcol <- fdrcols[nfdrcol] # Column name
            tcol <- fdrcolors[nfdrcol] # Color to be used

            ## **** Define org-mode prefix and postfix color syntax
            prefix <- paste0("¡", tcol, "¡")
            postfix <- paste0("¡/", tcol, "¡")

            ## **** Apply the coloring to the columns 
            df[[fdrcol]][df[[fdrcol]] < thold] <-
                paste0(prefix, df[[fdrcol]][df[[fdrcol]] < thold], postfix)

        }

    } else if (! FALSE) {

        print("I have left room here to add code to have users add a list
of column name and color pairs to define which columns should be colored how.")
        
    }
    
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
