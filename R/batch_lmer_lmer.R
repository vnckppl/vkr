#' Print summary statistics for a linear mixed-effects model
#'
#' This function prints summary statistics for a linear model such as the
#' estimate, std.err, p-value, and effect size for a predictor. It will
#' automatically detect if the predictor variable is numeric or a factor
#' variable. If it is a factor variable, then estimates etc. will be provided
#' for all levels of the facor variable (except the first level, that will be
#' the reference category)
#'
#' References:
#' 1) https://github.com/bcjaeger/r2glmm
#' 2) https://stats.stackexchange.com/questions/358927/compute-partial-eta2-for-all-fixed-effects-anovas-from-a-lme4-model
#'
#' @import lme4
#' @import lmerTest
#' @import r2glmm
#' @param df Input data frame
#' @param outcome Outcome measure
#' @param predictor Predictor
#' @param reffect Random Effect variable
#' @param covariates Single covariate or array of covariates
#' @param numdec Number of decimals for the beta and std statistics
#' @param glmmnb Set to TRUE if you want to use a Negative Binomial GLMM
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lmer <- function(df,
                       outcome,
                       predictor,
                       reffect,
                       covariates = NULL,
                       numdec = 2,
                       glmmnb = FALSE,
                       verbose = FALSE
                       ) {

    ## ** Disable scientific notations
    options(scipen = 999)

    ## ** Collapse covariates for linear model
    covariates <- paste(covariates, collapse = " + ")

    ## ** Remove rows where there the predictor or outcome measure are NA
    df <- df[!is.na(df[[predictor]]) & !is.na(df[[outcome]]), ]

    ## ** Test if the predictor variable is numeric or categorical
    pred_class <- class(df[[predictor]])

    ## ** If the predictor is a factor variable, then...
    # if (pred_class == "factor") {
    if ("factor" %in% class(df[[predictor]])) {

        ## *** Remove unused levels
        df[[predictor]] <- droplevels(df[[predictor]])

        ## *** Grab all groups
        groups <- levels(df[[predictor]])

        ## *** Create group dummies
        # To get effect size per group, we need to create group dummies
        my_dummies <- as.data.frame(
            model.matrix(~ eval(as.symbol(predictor)) - 1, data = df)
        )
        grvars <- paste0("gr_", groups)
        names(my_dummies) <- grvars
        df <- cbind(df, my_dummies)

        ## *** Build formula for linear model
        grvars_model <- paste(grvars[2:length(grvars)], collapse = " + ")
        my_formula <- paste(
            outcome, "~", covariates, "+", grvars_model, "+ (1 |", reffect, ")"
        )

        ## ** ...otherwise, if the predictor is a numeric variable, then:
    } else if (pred_class == "numeric") {

        ## *** Build formula for linear model
        my_formula <-
            paste(
                outcome, "~", covariates, "+", predictor, "+ (1 |", reffect, ")"
            )
    }

    ## ** Run linear mixed model model
    if (glmmnb == TRUE) {
        my_model <-
            lme4::glmer.nb(
                      formula = my_formula,
                      data = df,
                      na.action = na.exclude,
                      family = "poisson"
                  )
    } else if (glmmnb == FALSE) {
        my_model <-
            lmerTest::lmer(
                          formula = my_formula,
                          data = df,
                          na.action = na.exclude,
                          )
    }
    if (verbose) print(summary(my_model))

    ## ** Calculate effect sizes: semi-partial R-squared
    my_esize <- r2glmm::r2beta(my_model)
    if (verbose) print(my_esize)

    ## ** Extract the number of observations from the model
    my_model_sum <- summary(my_model)
    my_n_to <- my_model_sum[[3]]["dims"][[1]]["n"]

    ## ** Extract statistics for reporting
    my_coeff <- my_model_sum$coefficients

    ## ** Store statistics in an output data frame
    odf <- NULL
    odf_vars <- NULL

    ## ** If the predictor is a factor variable, then...
    # if (pred_class == "factor") {
    if ("factor" %in% class(df[[predictor]])) {

        ## *** Loop over groups levels to grab the estimates and p-values
        for (gr in seq(2, length(grvars))) {

            ## **** Grab info per group
            grp <- grvars[gr]
            gr_est <- formatC(
                round(my_coeff[grp, "Estimate"], numdec),
                format = "f", digits = numdec)
            gr_std <- formatC(
                round(my_coeff[grp, "Std. Error"], numdec),
                format = "f", digits = numdec)
            # For Negative Binomial Models
            if (glmmnb == TRUE) {
                gr_pvl <- formatC(
                    round(my_coeff[grp, "Pr(>|z|)"], 4), format = "f", digits = 4)
                # Store unrounded p-values for FDR correction (done separately)
                gr_fdr <- my_coeff[grp, "Pr(>|z|)"]
            # For other (linear) models
            } else if (glmmnb == FALSE) {
                gr_pvl <- formatC(
                    round(my_coeff[grp, "Pr(>|t|)"], 4), format = "f", digits = 4)
                # Store unrounded p-values for FDR correction (done separately)
                gr_fdr <- my_coeff[grp, "Pr(>|t|)"]
            }
            gr_pr2 <- formatC(
                round(my_esize$Rsq[my_esize$Effect == grp], 2),
                format = "f", digits = 2
            )

            ## **** Combine this information
            odf <- rbind(odf, gr_est, gr_std, gr_pvl, gr_fdr, gr_pr2)

            ## **** Variable labels
            odf_vars <- c(
                odf_vars,
                paste(
                    groups[gr], c("beta", "std", "p", "fdr", "pr2"), sep = "_")
            )
        }

        ## *** Add the number of observations included in the entire model
        odf <- c(odf, my_n_to)
        odf_vars <- c(odf_vars, "n")

        ## ** ...otherwise, if the predictor is a numeric variable, then...
    } else if (pred_class == "numeric") {

        pr_est <- formatC(
            round(my_coeff[predictor, "Estimate"], numdec),
            format = "f", digits = numdec)
        pr_std <- formatC(
            round(
                my_coeff[predictor, "Std. Error"], numdec),
            format = "f", digits = numdec)
        # For Negative Binomial Models
        if (glmmnb == TRUE) {
            pr_pvl <- formatC(
                round(my_coeff[predictor, "Pr(>|z|)"], 4), format = "f", digits = 4)
            # Store unrounded p-values for FDR correction (done separately)
            pr_fdr <- my_coeff[predictor, "Pr(>|z|)"]
        # For other (linear) models
        } else if (glmmnb == FALSE) {
            pr_pvl <- formatC(
                round(my_coeff[predictor, "Pr(>|t|)"], 4), format = "f", digits = 4)
            # Store unrounded p-values for FDR correction (done separately)
            pr_fdr <- my_coeff[predictor, "Pr(>|t|)"]
        }
        pr_pr2 <- formatC(
            round(my_esize$Rsq[my_esize$Effect == predictor], 2),
            format = "f", digits = 2
        )

        ## **** Combine this information
        odf <- rbind(pr_est, pr_std, pr_pvl, pr_fdr, pr_pr2, my_n_to)

        ## **** Variable labels
        odf_vars <- c("beta", "std", "p", "fdr", "pr2", "n")

    }

    ## ** Return output data frame
    odf <- as.data.frame(t(c(outcome, predictor, odf)))
    names(odf) <- c("outcome", "predictor", odf_vars)
    return(odf)

}
