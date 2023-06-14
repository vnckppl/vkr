#' Print demograhics for a variable by group
#'
#' This function prints demographics for a specific variable and stratifies this
#' by the factor levels of a second variable. It detects if your first variable
#' is a factor or numeric variable. Based on that, it will provides means and sd
#' for all groups of the numeric variable and counts and percentages for all
#' groups for the factor variable. It also tests the distributions of all
#' groups against the reference category. For categorical variables, it also
#' prvovides the chi-square p-value for the entire table. Results are returned
#' as a data frame.
#'
#' @param idf Input data frame
#' @param voi Variable of interest
#' @param group Group variable to stratify the variable of interest by
#' @export
demogr <- function(idf, voi, group) {

  ## ** Remove rows where group is NaN / NAs from the data frame
  n_old <- nrow(idf)
  idf <- idf[!is.na(idf[[group]]) & !is.nan(idf[[group]]), ]
  n_new <- nrow(idf)
  n_dif <- n_old - n_new
  if (n_dif > 0) {
    paste0(
      "Warning: removed ", as.character(n_dif), " rows because the ", group,
      " variable had missing values"
      )
  }

  ## ** Group levels
  groups <- levels(idf[[group]])

  ## ** Test type of variable (factor vs continuous)
  # * Test if grep returns an answer longer than length 0
  if (length(grep("numeric", class(idf[[voi]]))) > 0) {
    vartype <- "numeric"
  } else if (length(grep("factor", class(idf[[voi]]))) > 0) {
    vartype <- "factor"
  }

  ## ** Numeric variables
  if (vartype == "numeric") {

    ## *** Calculate mean and sd
    my_means <- aggregate(idf[[voi]], list(idf[[group]]), FUN = mean)
    my_stdev <- aggregate(idf[[voi]], list(idf[[group]]), FUN = sd)

    ## *** Test for group differences
    model1 <- lm(idf[[voi]] ~ idf[[group]])

    ## *** Effect size

    ## *** Output odf
    # voi, avg C, sd C, avg MCI1, sd MCI1, p C-MCI1, avg MCI2, sd MCI2, p C-MCI2
    ## **** Information from first group in group variable (should be control)
    output_a <- rbind(
      voi,
      round(my_means$x[my_means[1] == groups[1]], 1),
      round(my_stdev$x[my_stdev[1] == groups[1]], 1)
    )

    ## **** Information from all other groups in group variable
    # Order: mean, sd, p (vs control group)
    output_b <- NULL
    for (gr in seq(2, length(groups))) {
      output_b <- rbind(
        output_b,
        round(my_means$x[my_means[1] == groups[[gr]]], 1),
        round(my_stdev$x[my_stdev[1] == groups[[gr]]], 1),
        round(summary(model1)$coefficients[
                                paste0("idf[[group]]", groups[[gr]]),
                                "Pr(>|t|)"
                              ], 3)
      )
    }

    ## *** Define variabel labels
    ## **** First group in groups variable (should be control)
    labels_a <- c(
      "variable",
      paste0(groups[1], "_mean"),
      paste0(groups[1], "_sd")
    )

    ## **** Other groups in groups variable
    labels_b <- NULL
    for (gr in seq(2, length(groups))) {
      labels_b <- rbind(
        labels_b,
        paste0(groups[[gr]], "_mean"),
        paste0(groups[[gr]], "_sd"),
        paste0(groups[1], "-", groups[[gr]], "_p")
      )
    }

    ## *** Merge the information
    odf <- as.data.frame(t(c(output_a, output_b)))

    ## *** Apply variable labels
    names(odf) <- c(labels_a, labels_b)

  ## ** Factor variables
  } else if (vartype == "factor") {

    ## *** Factor variable levels
    factors <- levels(idf[[voi]])

    ## *** Number and proportion of first category (should be reference)
    ref_n <- table(idf[[voi]][idf[[group]] == idf[[group]][1]])
    ref_p <- round((ref_n / sum(ref_n)) * 100, 2)
    odf <- as.data.frame(cbind(ref_n, ref_p))
    names(odf) <- c(paste0(groups[1], "_n"), paste0(groups[1], "_p"))

    ## *** Loop over other categories
    for (gr in seq(2, length(groups))) {

      ## **** Number and proportion
      grx_n <- table(idf[[voi]][idf[[group]] == groups[gr]])
      grx_p <- round((grx_n / sum(grx_n)) * 100, 2)

      ## **** p-value of crosstab between this group and the reference group
      gr_ref <- groups[1]
      gr_subset <- idf[idf[[group]] == gr_ref | idf[[group]] == groups[[gr]], ]

      ## ***** Only do a chi.square test if there are at least 2 levels present
      grx_pval <- tryCatch(exp = {
        grx_c <- chisq.test(gr_subset[[group]], gr_subset[[voi]])
        grx_pval <- round(grx_c$p.value, 3)
      },
      error = function(e) {
        return("NA")
      })
      ## ***** Create array with zeros (as many as groups -1) and pvalue
      # This will look better in the output table
      grx_parray <- c(grx_pval, rep("", length(factors) - 1))

      ## **** Add to output data frame
      tmp_df <- as.data.frame(cbind(grx_n, grx_p, grx_parray))
      # Labels
      names(tmp_df) <- c(
        paste0(groups[gr], "_n"),
        paste0(groups[gr], "_p"),
        paste0(groups[1], "_vs_", groups[gr], "_pval")
      )

      odf <- as.data.frame(cbind(odf, tmp_df))

    }

    ## **** Save variable name in odf and set rownames as column
    varname <- c(voi, rep("", length(factors) - 1))
    namesdf <- as.data.frame(cbind(varname, row.names(odf)))
    names(namesdf) <- c("Variable", "Factor")
    odf <- cbind(namesdf, odf)
    row.names(odf) <- NULL

    ## **** Add p-value for overall Chi-square
    ## ***** Only do a chi.square test if there are at least 2 levels present
    all_pval <- tryCatch(exp = {
      all_cs <- chisq.test(idf[[group]], idf[[voi]])
      all_pval <- round(all_cs$p.value, 3)
    },
    error = function(e) {
      return("NA")
    })

    all_pval_array <- as.data.frame(c(all_pval, rep("", length(factors) - 1)))
    names(all_pval_array) <- "all_pval"
    odf <- cbind(odf, all_pval_array)

  }

  ## ** Return data frame
  return(odf)

}
