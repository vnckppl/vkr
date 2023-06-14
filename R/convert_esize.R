#' Convert effect-sizes
#'
#' This function converts between the following effect sizes:
#' Cohen's d, Cohen's f, and Eta-squared
#' The formulas that are used are from:
#' Cohen J., Statistical Power Analysis for the Behavioral
#' Sciences 2nd Ed (1988)
#'
#' 1) Cohen's f to Cohen's d
#' d = 2f
#' (Page 276)
#'
#' 2) Cohen's f to eta-squared
#' eta-squared = f^2 / (1 + f^2)
#' (Page 281)
#'
#' Interpretation of the effec sizes is as follows:
#' (https://en.wikipedia.org/wiki/Effect_size)
#' (http://core.ecu.edu/psyc/wuenschk/docs30/EffectSizeConventions.pdf)
#' (https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize)
#'
#' Cohen's d:
#' small: 0.2
#' medium: 0.5
#' large: 0.8
#'
#' Cohen's f:
#' small: 0.10
#' medium: 0.25
#' large: 0.40
#'
#' eta-squared:
#' small: 0.01
#' medium: 0.06
#' large: 0.14
#'
#' @param Value of an effect size
#' @param Type of effect size (h, f, d)
#' @export
convert_esize <- function(my_e, my_t) {

    ## * Output data frame
    odf <- as.data.frame(matrix(
        nrow = 3,
        ncol = 2
    ))
    names(odf) <- c("EffectSize", "Interpretation")
    rownames(odf) <- c("Cohen's f", "Cohen's d", "eta-squared")

    ## * Convert effect size
    if (my_t == "f") {

        ## ** Store input
        odf$EffectSize[1] <- my_e

        ## ** Convert f to d
        odf$EffectSize[2] <- 2 * my_e

        ## ** Convert f to eta-squared
        odf$EffectSize[3] <- my_e^2 / (1 + my_e^2)

    } else if (my_t == "d") {

        ## ** Convert d to f
        odf$EffectSize[1] <- my_e / 2

        ## ** Store input
        odf$EffectSize[2] <- my_e

        ## ** Convert d to eta-squared
        odf$EffectSize[3] <- (my_e / 2)^2 / (1 + (my_e / 2)^2)

    } else if (my_t == "h") {

        ## ** Convert eta-squared to f
        odf$EffectSize[1] <- sqrt(my_e / (1 - my_e^2))

        ## ** Convert eta-squared to d
        odf$EffectSize[2] <- 2 * sqrt(my_e / (1 - my_e^2))

        ## ** Store input
        odf$EffectSize[3] <- my_e

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

    ## * Return output
    return(odf)

}
