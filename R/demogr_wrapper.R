#' Run demogr on a set of variables and return a single output data frame
#'
#' This function is a wrapper script for demogr to loop over a number of
#' predcitor and outcome variables.
#'
#' @param idf Input data frame
#' @param vois Outcome measure(s) to loop over
#' @param group Group variable to stratify the variable of interest by
#' @param mediqr If TRUE, then calculate median/IQR instead of mean/stdev
#' @export

demogr_wrapper <- function(idf, vois, group, mediqr = FALSE) {

    ## * Output data frame
    odf <- NULL

    ## * Loop over variables of interest (outcomes)
    for (voi in vois) {

        ## *** Run demogr and append to output
        odf <- rbind(odf, vkr::demogr(idf, voi, group, mediqr))

    }
    ## * Return data frame
    return(odf)

}
