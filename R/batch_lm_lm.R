#' Print summary statistics for a linear model
#'
#' This function prints summary statistics for a linear model such as the
#' estimate, std.err, p-value, and effect size for a predictor. It will
#' automatically detect if the predictor variable is numeric or a factor
#' variable. If it is a factor variable, then estimates etc. will be provided
#' for all levels of the facor variable (except the first level, that will be
#' the reference categorie)
#'
#' @importFrom lmPerm "lmp"
#' @importFrom car "Anova"
#' @param df Input data frame
#' @param outcome Outcome measure
#' @param predictor Predictor
#' @param covariates Single covariate or array of covariates
#' @param numdec Number of decimals for the beta and std statistics
#' @param np If set with two values (Ca and maxIter), the code will run lmp,
#' from the lmPerm package, i.e., fitting and testing linear models with
#' permutation tests. Ca=Stop iterations when estimated standard error of the
#' estimated p is less than Ca*p. maxIter=The maximum number of iterations.
#' Note that this is a random permutation method, but the seed has been
#' explicitely set to 357 so that results will be replicable.
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lm <- function(
                     df,
                     outcome,
                     predictor,
                     covariates = NULL,
                     numdec = 2,
                     np = FALSE,
                     verbose = FALSE
                     ) {

    ## ** Disable scientific notations
    options(scipen = 999)

    ## ** Parse non-parametric information
    if (is.numeric(np[1]) && is.numeric(np[2])) {
        lmp_ca <- np[1]
        lmp_maxiter <- np[2]
        npset <- TRUE
        if (verbose) print("Running non-parametric linear models")
    } else {
        npset <- FALSE
    }

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

        ## ** ...otherwise, if the predictor is a numeric variable, then...
    } else if (length(grep("numeric", pred_class)) > 0) {

        ## *** Build formula for linear model
        my_formula <- paste(outcome, "~", covariates, "+", predictor)
    }

    ## ** Run linear model
    if (npset != TRUE) {
        my_model <- lm(formula = my_formula, data = df, na.action = na.exclude)
    } else {
        set.seed(357)
        my_model <- lmPerm::lmp(formula = as.formula(my_formula),
                                data = df,
                                na.action = na.exclude,
                                Ca = lmp_ca,
                                maxIter = lmp_maxiter)
    }
    if (verbose) print(summary(my_model))

    ## ** Calculate effect sizes
    my_esize <- lsr::etaSquared(my_model)
    if (verbose) print(my_esize)

    ## ** Extract the number of observations and dfs from the model
    my_model_sum <- summary(my_model)
    my_n_df <- my_model_sum$fstatistic["dendf"][[1]]
    my_n_pr <- my_model_sum$fstatistic["numdf"][[1]]
    my_n_to <- my_n_df + my_n_pr + 1

    ## ** Extract statistics for reporting
    my_coeff <- summary(my_model)$coefficients

    ## ** Store statistics in an output data frame
    odf <- NULL
    odf_vars <- NULL

    ## ** If the predictor is a factor variable, then...
    # if (pred_class == "factor") {
    if ("factor" %in% class(df[[predictor]])) {

        ## *** Loop over groups levels to grab the estimates and p-values
        for (gr in seq(2, length(grvars))) {

            ## **** Grab info per group
            ## ***** Group
            grp <- grvars[gr]

            ## ***** Estimate
            gr_est <- formatC(
                round(my_coeff[grp, "Estimate"], numdec),
                format = "f", digits = numdec)

            ## ***** Standard error
            if (npset != TRUE) {
            gr_std <- formatC(
                round(my_coeff[grp, "Std. Error"], numdec),
                format = "f", digits = numdec)
            } else {
                gr_std <- NA
            }

            ## ***** P-value
            if (npset != TRUE) {
                gr_pvl <- formatC(round(
                    my_coeff[grp, "Pr(>|t|)"], 4), format = "f", digits = 4)
            } else {
                ## gr_pvl <- formatC(round(
                ##     as.data.frame(car::Anova(my_model))[grp, "Pr(>F)"], 4),
                ##     format = "f", digits = 4)
                gr_pvl <- formatC(round(
                    my_coeff[grp, "Pr(Prob)"], 4), format = "f", digits = 4)
            }

            ## *****  Store unrounded p-values for FDR correction
            if (npset != TRUE) {
                gr_fdr <- my_coeff[grp, "Pr(>|t|)"]
            } else {
                ## gr_fdr <- as.data.frame(car::Anova(my_model))[grp, "Pr(>F)"]
                gr_fdr <- my_coeff[grp, "Pr(Prob)"]
            }

            ## ***** Effect size
            gr_eta <- formatC(
                round(my_esize[grp, "eta.sq"], 2), format = "f", digits = 2)

            ## ***** Combine this information
            odf <- rbind(odf, gr_est, gr_std, gr_pvl, gr_fdr, gr_eta)

            ## ***** Variable labels
            odf_vars <- c(
                odf_vars,
                paste(
                    groups[gr], c("beta", "std", "p", "fdr", "eta2"), sep = "_")
            )
        }

        ## *** Add the number of observations included in the entire model
        odf <- c(odf, my_n_to)
        odf_vars <- c(odf_vars, "n")

        ## ** ...otherwise, if the predictor is a numeric variable, then...
    } else if (length(grep("numeric", pred_class)) > 0) {

        ## *** Estimate
        pr_est <- formatC(
            round(my_coeff[predictor, "Estimate"], numdec),
            format = "f", digits = numdec)

        ## *** Standard Error
        if (npset != TRUE) {
            pr_std <- formatC(
                round(
                    my_coeff[predictor, "Std. Error"], numdec),
                format = "f", digits = numdec)
        } else {
            pr_std <- NA
        }

        ## *** P-value
        if (npset != TRUE) {
            pr_pvl <- formatC(
                round(my_coeff[predictor, "Pr(>|t|)"], 4),
                format = "f", digits = 4)
        } else {
            ## pr_pvl <- formatC(round(
            ##     as.data.frame(car::Anova(my_model))[predictor, "Pr(>F)"], 4),
            ##     format = "f", digits = 4)
            pr_pvl <- formatC(
                round(my_coeff[predictor, "Pr(Prob)"], 4),
                format = "f", digits = 4)
        }

        ## *** Store unrounded p-values for FDR correction (done separately)
        if (npset != TRUE) {
            pr_fdr <- my_coeff[predictor, "Pr(>|t|)"]
        } else {
            ## pr_fdr <- as.data.frame(car::Anova(my_model))[predictor, "Pr(>F)"]
            pr_fdr <- my_coeff[predictor, "Pr(Prob)"]
        }

        ## *** Eta-squared
        pr_eta <- formatC(
            round(my_esize[predictor, "eta.sq"], 2), format = "f", digits = 2)

        ## *** Combine this information
        odf <- rbind(pr_est, pr_std, pr_pvl, pr_fdr, pr_eta, my_n_to)

        ## *** Variable labels
        odf_vars <- c("beta", "std", "p", "fdr", "eta2", "n")

    }

    ## ** Return output data frame
    odf <- as.data.frame(t(c(outcome, predictor, odf)))
    names(odf) <- c("outcome", "predictor", odf_vars)
    return(odf)

}
