#' Run batch_lmer and subsquently batch_lm_fdr on a number of predictor and
#' outcome variables.
#'
#' This function is a wrapper script for batch_lm to loop over a number of
#' predcitor and outcome variables, and to subsequently apply FDR correction on
#' the output using batch_lm_fdr.
#'
#' @param df Input data frame
#' @param outcomes Outcome measure(s) to loop over
#' @param predictors Predictor(s) to loop over
#' @param reffect Random Effect variable
#' @param covariates Single covariate or array of covariates
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lmer_wrapper <-
    function(
             df,
             outcomes,
             predictors,
             reffect,
             covariates = NULL,
             verbose = FALSE
             ) {

        ## * Output data frame
        odf <- NULL

        ## * Loop over predictor variables
        for (pred in predictors) {

            ## ** Loop over outcome measurs
            for (outc in outcomes) {

                ## *** Run linear models via batch_lm and append to output
                odf <- rbind(odf, vkr::batch_lmer(
                                           df,
                                           outc,
                                           pred,
                                           reffect,
                                           covariates,
                                           verbose
                                       ))
            }
        }

        ## * Apply FDR correction
        odf <- vkr::batch_lm_fdr(odf)

        ## * Return data frame
        return(odf)
}