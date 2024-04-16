#' Add a column inside a dataframe
#'
#' Add a column inside a dataframe
#'
#' @param df Input data frame
#' @param position Column (either a column number or column name AFTER which you want to insert your new column
#' @param colname Name of the new column
#' @param values Values of the new column. Either a single value (that will be repeated for each row) or an array of length nrow(df) are accepted.
#' @export
insert_column <- function(df, position, colname, values) {

    ## * Split up the data frame columns at the position point
    ## ** If position is a number
    if (is.numeric(position)) {
        posloc <- position
    } else if (is.character(position)) {
        ## ** If the position is a column name
        posloc <- grep(position, names(df))
    }
    part1 <- df[1:posloc]
    part2 <- df[(posloc + 1):ncol(df)]

    ## * New column
    ## ** If a single value was entered create an array of length nrow
    if (length(values) == 1) {
        valuesdf <- rep(values, times = nrow(df))
    } else {
        valuesdf <- values
    }
    newcol <- as.data.frame(valuesdf)
    names(newcol) <- colname

    ## * Combine data frames
    odf <- cbind(cbind(part1, newcol), part2)

    ## * Return column
    return(odf)

}
