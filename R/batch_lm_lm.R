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
#' Note that this is a random permutation method, but the seed has been
#' explicitely set to 357 so that results will be replicable.
#' @param plot Generate plots and store in the output folder
#' @param verbose Set to TRUE if you want to see the full lm output
#' @export

batch_lm <- function(
                     df,
                     outcome,
                     predictor,
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

    ## ** Disable scientific notations
    options(scipen = 999)

    ## ** Check requested sums of squares type
    if (!ss_type %in% c(2, 3)) {
        stop("`ss_type` must be either 2 (Type II) or 3 (Type III).")
    }

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
    covariates_bkp <- covariates

    ## ** Remove rows where there the predictor or outcome measure are NA
    df <- df[!is.na(df[[predictor]]) & !is.na(df[[outcome]]), ]

    ## ** Test predictor type and build model formula
    if (is.factor(df[[predictor]])) {

        ## *** Remove unused levels
        df[[predictor]] <- droplevels(df[[predictor]])

        if (nlevels(df[[predictor]]) < 2) {
            stop("`predictor` must have at least two observed factor levels.")
        }

        ## *** Grab all groups
        groups <- levels(df[[predictor]])

        ## *** Create group dummies
        my_dummies <- as.data.frame(
            model.matrix(~ eval(as.symbol(predictor)) - 1, data = df)
        )
        grvars <- paste0("gr_", groups)
        names(my_dummies) <- grvars
        df <- cbind(df, my_dummies)

        ## *** Build formula: covariates + all non-reference dummy variables
        my_formula <- stats::reformulate(
                                 c(covariates_bkp, grvars[-1]),
                                 response = outcome
                             )

    } else if (is.numeric(df[[predictor]])) {

        ## *** Build formula: covariates + numeric predictor
        my_formula <- stats::reformulate(
                                 c(covariates_bkp, predictor),
                                 response = outcome
                             )

    } else {
        stop("`predictor` must be numeric/integer or a factor.")
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

    ## ** Plot the scatter + regression line plots
    if (!isFALSE(plot)) {
        vkr::batch_lm_plot(
            df = df,
            outcome = outcome,
            covariates = covariates_bkp,
            predictor = predictor,
            output_dir = plot,
            width_cm = 30,
            height_cm = 15
        )
    }

    ## ** Calculate effect sizes
    ## Use ordinary lm for effect sizes when permutation testing is used
    if (npset != TRUE) {
        my_esize_model <- my_model
    } else {
        my_esize_model <- lm(
            formula = my_formula,
            data = df,
            na.action = na.exclude
        )
    }

    my_esize <- car::Anova(my_esize_model, type = ss_type)

    ## *** Extract error and total sums of squares
    ss_error <- my_esize["Residuals", "Sum Sq"]

    my_outcome <- stats::model.response(
        stats::model.frame(my_esize_model)
    )
    ss_total <- sum((my_outcome - mean(my_outcome))^2)

    ## *** Calculate eta-squared or partial eta-squared for a model term
    get_eta2 <- function(term, partial = FALSE) {
        ss_effect <- my_esize[term, "Sum Sq"]

        if (isTRUE(partial)) {
            ss_effect / (ss_effect + ss_error)
        } else {
            ss_effect / ss_total
        }
    }

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
                    my_coeff[grp, "Pr(>|t|)"], numdecp), format = "f", digits = numdecp)
            } else {
                ## gr_pvl <- formatC(round(
                ##     as.data.frame(car::Anova(my_model))[grp, "Pr(>F)"], 4),
                ##     format = "f", digits = 4)
                gr_pvl <- formatC(round(
                    my_coeff[grp, "Pr(Prob)"], numdecp), format = "f", digits = numdecp)
            }

            ## *****  Store unrounded p-values for FDR correction
            if (npset != TRUE) {
                gr_fdr <- my_coeff[grp, "Pr(>|t|)"]
            } else {
                ## gr_fdr <- as.data.frame(car::Anova(my_model))[grp, "Pr(>F)"]
                gr_fdr <- my_coeff[grp, "Pr(Prob)"]
            }

            ## ***** Effect size
            if (isTRUE(partialh2)) {
                gr_eta <- formatC(
                    round(get_eta2(grp, partial = TRUE), numdech),
                    format = "f", digits = numdech
                )
                eta_lab <- "peta2"
            } else if (isFALSE(partialh2)) {
                gr_eta <- formatC(
                    round(get_eta2(grp, partial = FALSE), numdech),
                    format = "f", digits = numdech
                )
                eta_lab <- "eta2"
            }

            ## ***** Combine this information
            odf <- rbind(odf, gr_est, gr_std, gr_pvl, gr_fdr, gr_eta)

            ## ***** Variable labels
            odf_vars <- c(
                odf_vars,
                paste(
                    groups[gr], c("beta", "std", "p", "fdr", eta_lab), sep = "_")
            )
        }

        ## *** Add the number of observations included in the entire model
        odf <- c(odf, my_n_to)
        odf_vars <- c(odf_vars, "n")

        ## ** ...otherwise, if the predictor is a numeric variable, then...
    } else if (is.numeric(df[[predictor]])) {

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
                round(my_coeff[predictor, "Pr(>|t|)"], numdecp),
                format = "f", digits = numdecp)
        } else {
            ## pr_pvl <- formatC(round(
            ##     as.data.frame(car::Anova(my_model))[predictor, "Pr(>F)"], 4),
            ##     format = "f", digits = 4)
            pr_pvl <- formatC(
                round(my_coeff[predictor, "Pr(Prob)"], numdecp),
                format = "f", digits = numdecp)
        }

        ## *** Store unrounded p-values for FDR correction (done separately)
        if (npset != TRUE) {
            pr_fdr <- my_coeff[predictor, "Pr(>|t|)"]
        } else {
            ## pr_fdr <- as.data.frame(car::Anova(my_model))[predictor, "Pr(>F)"]
            pr_fdr <- my_coeff[predictor, "Pr(Prob)"]
        }

        ## *** Eta-squared
        if (isTRUE(partialh2)) {
            pr_eta <- formatC(
                round(get_eta2(predictor, partial = TRUE), numdech),
                format = "f", digits = numdech
            )
            eta_lab <- "peta2"
        } else if (isFALSE(partialh2)) {
            pr_eta <- formatC(
                round(get_eta2(predictor, partial = FALSE), numdech),
                format = "f", digits = numdech
            )
            eta_lab <- "eta2"
        }

        ## *** Combine this information
        odf <- rbind(pr_est, pr_std, pr_pvl, pr_fdr, pr_eta, my_n_to)

        ## *** Variable labels
        odf_vars <- c("beta", "std", "p", "fdr", eta_lab, "n")

    }

    ## ** Return output data frame
    odf <- as.data.frame(t(c(outcome, predictor, odf)))
    names(odf) <- c("outcome", "predictor", odf_vars)
    return(odf)

}
