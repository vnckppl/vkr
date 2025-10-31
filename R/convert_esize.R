#' Convert effect-sizes
#'
#' This function converts between the following effect sizes:
#' Cohen's d, Cohen's f, Eta-squared, and OR
#' The formulas that are used are from:
#' - Cohen J., Statistical Power Analysis for the Behavioral
#'   Sciences 2nd Ed (1988)
#' - Sánchez-Meca, Effect-Size Indices for Dichotomized Outcomes in
#'   Meta-Analysis (2003)
#' - Mathur, A Simple Interpretable Conversion from Pearson's Correlation to
#'   Cohen's for d Continuous Exposures (2020)
#'
#' 1) Cohen's f to Cohen's d
#' Cohen 1988: Page 276
#' d = 2f
#'
#' 2) Cohen's f to eta-squared
#' Cohen 1988: Page 281
#' eta-squared = f^2 / (1 + f^2)
#'
#' 3) OR to Cohen's d
#' Sánchez-Meca 2003: Formula 14
#' Cohen's d = log(OR) * (sqrt(3) / pi)
#'
#' 4) Pearson's r to Cohen's d
#' Mathur 2020
#' d = 2 * r / sqrt(1 - r^2)
#'
#'
#' Interpretation of the effec sizes is as follows:
#' (https://en.wikipedia.org/wiki/Effect_size)
#' (http://core.ecu.edu/psyc/wuenschk/docs30/EffectSizeConventions.pdf)
#' (https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize)
#'
#' | Measure    | Small | Medium | Large |
#' |------------|-------|--------|-------|
#' | Cohen's d  | 0.2   | 0.5    | 0.8   |
#' | Cohen's f  | 0.1   | 0.25   | 0.4   |
#' | eta^2      | 0.01  | 0.06   | 0.14  |
#' | Odds Ratio |       |        |       |
#'
#' @param Value of an effect size
#' @param Type of effect size (h, f, d, OR, pr)
#' @export
convert_esize <- function(my_e, my_t) {

    ## * Helper functions
    con_Cf_2_Cd <- function(Cf) return(Cf * 2)
    con_Cf_2_n2 <- function(Cf) return(Cf^2 / (1 + Cf^2))
    con_Cd_2_Cf <- function(Cd) return(Cd / 2)
    con_Cd_2_n2 <- function(Cd) return((Cd / 2)^2 / (1 + (Cd / 2)^2))
    con_n2_2_Cf <- function(n2) return(sqrt(n2 / (1 - n2)))
    con_OR_2_Cd <- function(OR) return(log(OR) * (sqrt(3) / pi))
    con_Cd_2_OR <- function(Cd) return(exp(Cd * (pi / sqrt(3))))
    con_Pr_2_Cd <- function(Pr) return(2 * Pr / sqrt(1 - Pr^2))
    con_Cd_2_Pr <- function(Cd) return(Cd / sqrt(Cd^2 + 4))


    ## * Output data frame
    odf <- as.data.frame(matrix(
        nrow = 5,
        ncol = 2
    ))
    names(odf) <- c("EffectSize", "Interpretation")
    rownames(odf) <-
        c("Cohen's f", "Cohen's d", "eta-squared", "odds ratio", "Pearson r")


    ## * Convert effect size
    if (my_t == "f") {

        ## ** Store input
        odf$EffectSize[1] <- my_e

        ## ** Convert f to d
        odf$EffectSize[2] <- con_Cf_2_Cd(my_e)

        ## ** Convert f to eta-squared
        odf$EffectSize[3] <- con_Cf_2_n2(my_e)

        ## ** Convert f to d to OR
        odf$EffectSize[4] <- con_Cd_2_OR(odf$EffectSize[2])

        ## ** Convert f to d to r
        odf$EffectSize[5] <- con_Cd_2_Pr(odf$EffectSize[2])

    } else if (my_t == "d") {

        ## ** Convert d to f
        odf$EffectSize[1] <- con_Cd_2_Cf(my_e)

        ## ** Store input
        odf$EffectSize[2] <- my_e

        ## ** Convert d to eta-squared
        odf$EffectSize[3] <- con_Cd_2_n2(my_e)

        ## ** Convert d to OR
        odf$EffectSize[4] <- con_Cd_2_OR(my_e)

        ## ** Convert d to r
        odf$EffectSize[5] <- con_Cd_2_Pr(my_e)

    } else if (my_t == "h") {

        ## ** Convert eta-squared to f
        odf$EffectSize[1] <- con_n2_2_Cf(my_e)

        ## ** Convert eta-squared to d
        odf$EffectSize[2] <- con_Cf_2_Cd(odf$EffectSize[1])

        ## ** Store input
        odf$EffectSize[3] <- my_e

        ## ** Convert eta-squared to d to OR
        odf$EffectSize[4] <- con_Cd_2_OR(odf$EffectSize[2])

        ## ** Convert eta-squared to d to r
        odf$EffectSize[5] <- con_Cd_2_Pr(odf$EffectSize[2])

    } else if (my_t == "OR") {

        ## ** Convert OR to d
        odf$EffectSize[2] <- con_OR_2_Cd(my_e)

        ## ** Convert OR to d to f
        odf$EffectSize[1] <- con_Cd_2_Cf(odf$EffectSize[2])

        ## ** Convert OR to d to eta-squared
        odf$EffectSize[3] <- con_Cd_2_n2(odf$EffectSize[2])

        ## ** Store input
        odf$EffectSize[4] <- my_e

        ## ** Convert OR to d to r
        odf$EffectSize[5] <- con_Cd_2_Pr(odf$EffectSize[2])

    } else if (my_t == "pr") {

        ## ** Convert Pearson r to d
        odf$EffectSize[2] <- con_Pr_2_Cd(my_e)

        ## ** Convert Pearson r to d to f
        odf$EffectSize[1] <- con_Cd_2_Cf(odf$EffectSize[2])

        ## ** Convert Pearson r to d to eta-squared
        odf$EffectSize[3] <- con_Cd_2_n2(odf$EffectSize[2])

        ## ** Convert Pearson r to d to OR
        odf$EffectSize[4] <- con_Cd_2_OR(odf$EffectSize[2])

        ## ** Store input
        odf$EffectSize[5] <- my_e
    }


    ## * Add interpretation
    ## ** Cohen's f
    if (odf$EffectSize[1] < .1) {
        odf$Interpretation[1] <- "< small"
    }
    if (odf$EffectSize[1] >= .1 & odf$EffectSize[1] < .25) {
        odf$Interpretation[1] <- "small"
    }
    if (odf$EffectSize[1] >= .25 & odf$EffectSize[1] < .40) {
        odf$Interpretation[1] <- "medium"
    }
    if (odf$EffectSize[1] >= .40) {
        odf$Interpretation[1] <- "large"
    }

    ## ** Cohen's d
    if (odf$EffectSize[2] < .2) {
        odf$Interpretation[2] <- "< small"
    }
    if (odf$EffectSize[2] >= .2 & odf$EffectSize[2] < .5) {
        odf$Interpretation[2] <- "small"
    }
    if (odf$EffectSize[2] >= .5 & odf$EffectSize[2] < .8) {
        odf$Interpretation[2] <- "medium"
    }
    if (odf$EffectSize[2] >= .8) {
        odf$Interpretation[2] <- "large"
    }

    ## ** eta-squared
    if (odf$EffectSize[3] < .01) {
        odf$Interpretation[3] <- "< small"
    }
    if (odf$EffectSize[3] >= .01 & odf$EffectSize[3] < .06) {
        odf$Interpretation[3] <- "small"
    }
    if (odf$EffectSize[3] >= .06 & odf$EffectSize[3] < .14) {
        odf$Interpretation[3] <- "medium"
    }
    if (odf$EffectSize[3] >= .14) {
        odf$Interpretation[3] <- "large"
    }

    ## ** Odds Ratio
    if (odf$EffectSize[4] < 1.44) {
        odf$Interpretation[4] <- "< small"
    }
    if (odf$EffectSize[4] >= 1.44 & odf$EffectSize[4] < 2.48) {
        odf$Interpretation[4] <- "small"
    }
    if (odf$EffectSize[4] >= 2.48 & odf$EffectSize[4] < 4.27) {
        odf$Interpretation[4] <- "medium"
    }
    if (odf$EffectSize[4] >= 4.27) {
        odf$Interpretation[4] <- "large"
    }

    ## ** Pearson r
    if (odf$EffectSize[5] < .10) {
        odf$Interpretation[5] <- "< small"
    }
    if (odf$EffectSize[5] >= .10 & odf$EffectSize[5] < .20) {
        odf$Interpretation[5] <- "small"
    }
    if (odf$EffectSize[5] >= .20 & odf$EffectSize[5] < .30) {
        odf$Interpretation[5] <- "medium"
    }
    if (odf$EffectSize[5] >= .30) {
        odf$Interpretation[5] <- "large"
    }

    ## * Return output
    return(odf)

}
