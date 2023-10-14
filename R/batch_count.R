#' Print summary statistics for count models
#'
#' This function prints summary statistics for a count models such as the
#' estimate, std.err, p-value, and effect size for a predictor. It will
#' automatically detect if the predictor variable is numeric or a factor
#' variable. If it is a factor variable, then estimates etc. will be provided
#' for all levels of the facor variable (except the first level, that will be
#' the reference category).
#'
#' It will also detect if a Poisson model or negative binomial regression model
#' is a better fit fo the data, and will report the information for the most
#' appropriate model. Model selection is done by running both models and then
#' using a likelihood ratio test to evaluate if either Poisson or negative
#' binomial is the better fit.
#'
#' References:
#' 1) https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
#'
#' @importFrom MASS "glm.nb"
#' @importFrom lmtest "lrtest"
#' @param df Input data frame
#' @param outcome Outcome measure
#' @param predictor Predictor
#' @param covariates Single covariate or array of covariates
#' @param numdec Number of decimals for the beta and std statistics
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_count <- function(df,
                       outcome,
                       predictor,
                       covariates = NULL,
                       numdec = 2,
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
        my_formula <- paste(outcome, "~", covariates, "+", grvars_model)

        ## ** ...otherwise, if the predictor is a numeric variable, then:
    } else if (pred_class == "numeric") {

        ## *** Build formula for linear model
        my_formula <- paste(outcome, "~", covariates, "+", predictor)
    }

    ## ** Run count regression models
    ## *** Poisson
    my_model_poi <-
        glm(
            formula = my_formula,
            data = df,
            na.action = na.exclude,
            family = "poisson"
            )
    if (verbose) print(summary(my_model_poi))

    ## *** Negative binomial regression
    my_model_nbr <-
        MASS::glm.nb(
            formula = my_formula,
            data = df,
            na.action = na.exclude,
            )
    if (verbose) print(summary(my_model_nbr))

    ## *** Run a likelihood ratio test to see which is the better model
    my_lrtest <- lmtest::lrtest(my_model_poi, my_model_nbr)
    if (verbose) print(my_lrtest)
    my_lrtest_p <- my_lrtest$"Pr(>Chisq)"[2]

    ## **** Select the correct model
    # If the likelihood ratio test is significant, than the nbreg model is a
    # better fit than the Poisson model.
    if (my_lrtest_p < 0.05) {
        my_model <- my_model_nbr
        sel_mod <- "nbr"
    } else {
        my_model <- my_model_poi
        sel_mod <- "psn"
    }

    ## ** Extract the number of observations from the model
    my_model_sum <- summary(my_model)
    my_n_to <- my_model_sum[[9]] + 1

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
            gr_pvl <- formatC(
                round(my_coeff[grp, "Pr(>|z|)"], 4), format = "f", digits = 4)
            # Store unrounded p-values for FDR correction (done separately)
            gr_fdr <- my_coeff[grp, "Pr(>|z|)"]
            gr_irr <- formatC(
                round(exp(my_coeff[grp, "Estimate"]), numdec),
                format = "f", digits = numdec)

            ## **** Combine this information
            odf <- rbind(odf, gr_est, gr_std, gr_pvl, gr_fdr, gr_irr)

            ## **** Variable labels
            odf_vars <- c(
                odf_vars,
                paste(
                    groups[gr], c("beta", "std", "p", "fdr", "irr"), sep = "_")
            )
        }

        ## *** Add the sample size and type of model (Poisson or nbreg)
        odf <- c(odf, my_n_to, sel_mod)
        odf_vars <- c(odf_vars, "n", "model")

        ## ** ...otherwise, if the predictor is a numeric variable, then...
    } else if (pred_class == "numeric") {

        pr_est <- formatC(
            round(my_coeff[predictor, "Estimate"], numdec),
            format = "f", digits = numdec)
        pr_std <- formatC(
            round(
                my_coeff[predictor, "Std. Error"], numdec),
            format = "f", digits = numdec)
        pr_pvl <- formatC(
            round(my_coeff[predictor, "Pr(>|z|)"], 4), format = "f", digits = 4)
        # Store unrounded p-values for FDR correction (done separately)
        pr_fdr <- my_coeff[predictor, "Pr(>|z|)"]

        # Incidence Rate Ratio
        pr_irr <- formatC(
            round(exp(my_coeff[predictor, "Estimate"]), numdec),
            format = "f", digits = numdec)

        ## **** Combine this information
        odf <- rbind(pr_est, pr_std, pr_pvl, pr_fdr, pr_irr, my_n_to, sel_mod)

        ## **** Variable labels
        odf_vars <- c("beta", "std", "p", "fdr", "irr", "n", "model")

    }

    ## ** Return output data frame
    odf <- as.data.frame(t(c(outcome, predictor, odf)))
    names(odf) <- c("outcome", "predictor", odf_vars)
    return(odf)

}
