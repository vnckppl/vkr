#' Run batch_lm and subsquently batch_lm_fdr on a number of predictor and
#' outcome variables.
#'
#' This function is a wrapper script for batch_lm to loop over a number of
#' predcitor and outcome variables, and to subsequently apply FDR correction on
#' the output using batch_lm_fdr.
#'
#' @param df Input data frame
#' @param outcomes Outcome measure(s) to loop over
#' @param predictors Predictor(s) to loop over
#' @param covariates Single covariate or array of covariates
#' @param partialh2 If TRUE, then use partial-eta^2 instead of eta^2
#' @param ss_type Type of sums of squares for effect sizes: 2 (default) for
#' Type-II SS or 3 for Type-III SS. #' Prefer Type-III SS for models with
#' interactions; center continuous predictors and use appropriate contrasts
#' for categorical predictors.
#' @param numdec Number of decimals for the beta and std statistics
#' @param numdecp Number of decimals for p-values
#' @param numdech Number of decimals for (partial)-eta^2
#' @param np If set with two values (Ca and maxIter), the code will run lmp,
#' from the lmPerm package, i.e., fitting and testing linear models with
#' permutation tests. Ca=Stop iterations when estimated standard error of the
#' estimated p is less than Ca*p. maxIter=The maximum number of iterations.
#' @param plot Generate plots and store in the output folder
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lm_wrapper <-
    function(
             df,
             outcomes,
             predictors,
             covariates = NULL,
             partialh2 = FALSE,
             ss_type = 2,
             numdec = 2,
             numdecp = 4,
             numdech = 2,
             np = FALSE,
             plot = FALSE,
             verbose = FALSE
             ) {

        ## * Output data frame
        odf <- NULL

        ## * Loop over predictor variables
        for (pred in predictors) {

            ## ** Loop over outcome measurs
            for (outc in outcomes) {

                ## *** Run linear models via batch_lm and append to output
                odf <- rbind(odf, vkr::batch_lm(
                                           df = df,
                                           outcome = outc,
                                           predictor = pred,
                                           covariates = covariates,
                                           partialh2 = partialh2,
                                           ss_type = ss_type,
                                           numdec = numdec,
                                           numdecp = numdecp,
                                           numdech = numdech,
                                           np = np,
                                           plot = plot,
                                           verbose= verbose
                                       ))
            }
        }

        ## * Apply FDR correction
        odf <- vkr::batch_lm_fdr(odf)

        ## * Return data frame
        return(odf)
    }
