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
#' @param numdec Number of decimals for the beta and std statistics
#' @param glmmnb Set to TRUE if you want to use a Negative Binomial GLMM
#' @param fdr Apply FDR correction (default = TRUE). Turn it off if you want to merge multiple tables later and apply FDR correction on the combined table.
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lmer_wrapper <-
    function(
             df,
             outcomes,
             predictors,
             reffect,
             covariates = NULL,
             numdec = 2,
             glmmnb = FALSE,
             fdr = TRUE,
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
                                           df = df,
                                           outcome = outc,
                                           predictor = pred,
                                           reffect = reffect,
                                           covariates = covariates,
                                           numdec = numdec,
                                           glmmnb = glmmnb,
                                           verbose = verbose
                                       ))
            }
        }

        ## * Apply FDR correction
        if (fdr == TRUE) odf <- vkr::batch_lm_fdr(odf)

        ## * Return data frame
        return(odf)
}
