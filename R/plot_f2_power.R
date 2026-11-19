#' Plot regression-model power curves from Cohen's f
#'
#' Calculates required total sample sizes for one or more linear-regression or
#' ANCOVA effects using \code{pwr::pwr.f2.test()}, then plots statistical power
#' against total sample size. The input effect sizes must be Cohen's \eqn{f};
#' partial eta-squared values can first be converted with \code{peta_to_f()}.
#'
#' The \code{model_df} column represents the total number of predictor degrees
#' of freedom in the full model, excluding the intercept. This includes all
#' covariates, factor dummy variables, interaction terms, and polynomial terms.
#' The \code{test_df} column represents the numerator degrees of freedom for the
#' effect being tested. For one interaction coefficient, \code{test_df = 1}.
#'
#' For example, for \code{outcome ~ age + group + age:group}, use
#' \code{model_df = 3} and \code{test_df = 1}. The required total sample size is
#' calculated as \code{ceiling(v + model_df + 1)}, where \code{v} is the
#' residual degrees of freedom returned by \code{pwr::pwr.f2.test()}.
#'
#' @param idf Data frame containing one row per effect to plot. It must contain
#'   columns named \code{label}, \code{f}, \code{model_df}, and \code{test_df}.
#' @param powers Numeric vector of desired power values for the curves. Values
#'   must be greater than zero and less than one.
#' @param alpha Type-I error rate. Defaults to \code{0.05}.
#' @param target_power Power value for which a summary required sample size is
#'   returned and a horizontal reference line is drawn. Defaults to \code{0.80}.
#' @param xlim Optional numeric vector of length two giving x-axis limits. If
#'   \code{NULL} (default), limits are determined from the calculated values.
#' @param output_file Optional filename, including extension, to which the plot
#'   is saved. If \code{NULL} (default), the plot is not saved.
#' @param plot_title Plot title.
#' @param plot_subtitle Optional plot subtitle.
#'
#' @return A list with three elements:
#'   \itemize{
#'     \item \code{plot}: the \code{ggplot} power-curve figure;
#'     \item \code{curve_data}: long-format data used to create the curves;
#'     \item \code{required_n}: required total sample size at
#'       \code{target_power}.
#'   }
#'
#' @examples
#' effects <- data.frame(
#'   label = c("Cognition: group × age", "Motor: group × age"),
#'   f = c(0.289, 0.220),
#'   model_df = c(3, 3),
#'   test_df = c(1, 1)
#' )
#'
#' result <- plot_f2_power(effects, xlim = c(0, 500))
#' result$plot
#' result$required_n
#'
#' @export
plot_f2_power <- function(idf,
                          powers = seq(0.05, 0.99, by = 0.01),
                          alpha = 0.05,
                          target_power = 0.80,
                          xlim = NULL,
                          output_file = NULL,
                          plot_title = "Required sample size by power",
                          plot_subtitle = NULL) {

  ## * Check that the input data frame has all required information
  required_cols <- c("label", "f", "model_df", "test_df")

  if (!all(required_cols %in% names(idf))) {
    stop(
      "idf must contain the following columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  ## * Check that the requested power values are mathematically valid
  ## Power of exactly 0 or 1 cannot be used to obtain a finite sample size.
  if (any(powers <= 0 | powers >= 1)) {
    stop("All values in powers must be greater than 0 and less than 1.")
  }

  ## * Check that the requested target power is mathematically valid
  if (target_power <= 0 || target_power >= 1) {
    stop("target_power must be greater than 0 and less than 1.")
  }

  ## * Check that Cohen's f values can be used for an F-test power calculation
  if (any(is.na(idf$f) | idf$f <= 0)) {
    stop("All Cohen's f values must be positive and non-missing.")
  }

  ## * Check model degrees of freedom
  ## model_df is the total number of predictor df in the full model.
  ## test_df is the df associated with the focal effect being tested.
  if (any(
    is.na(idf$model_df) |
    is.na(idf$test_df) |
    idf$model_df < 1 |
    idf$test_df < 1 |
    idf$test_df > idf$model_df
  )) {
    stop(
      "model_df and test_df must be positive, and test_df cannot exceed model_df."
    )
  }

  ## * Calculate a required total N at each requested power level
  ##
  ## pwr.f2.test() requires:
  ##   u  = numerator df for the effect of interest;
  ##   v  = residual/denominator df, which it solves for;
  ##   f2 = Cohen's f squared.
  ##
  ## In a linear regression model:
  ##   residual df = N - model_df - 1
  ##
  ## Therefore:
  ##   total N = residual df + model_df + 1
  curve_list <- lapply(seq_len(nrow(idf)), function(i) {

    lapply(powers, function(this_power) {

      ## ** Calculate residual df required for this effect and power
      fit <- tryCatch(
        pwr::pwr.f2.test(
          u = idf$test_df[i],
          v = NULL,
          f2 = idf$f[i]^2,
          sig.level = alpha,
          power = this_power
        ),
        error = function(e) NULL
      )

      ## ** Convert the returned residual df to total required sample size
      ##
      ## ceiling() ensures that the reported total N is an integer that reaches
      ## or exceeds the requested power.
      required_n <- if (is.null(fit)) {
        NA_real_
      } else {
        ceiling(fit$v + idf$model_df[i] + 1)
      }

      ## ** Return one row of long-format plotting data
      data.frame(
        label = idf$label[i],
        f = idf$f[i],
        model_df = idf$model_df[i],
        test_df = idf$test_df[i],
        power = this_power,
        sample_size = required_n
      )
    })
  })

  ## * Combine rows from all effects and all requested power values
  curve_data <- do.call(rbind, unlist(curve_list, recursive = FALSE))

  ## * Remove any unsuccessful calculations before plotting
  curve_data <- curve_data[!is.na(curve_data$sample_size), ]

  ## * Calculate and retain the total N specifically required at target_power
  required_n <- do.call(
    rbind,
    lapply(seq_len(nrow(idf)), function(i) {

      ## ** Solve for residual df at the requested target power
      fit <- pwr::pwr.f2.test(
        u = idf$test_df[i],
        v = NULL,
        f2 = idf$f[i]^2,
        sig.level = alpha,
        power = target_power
      )

      ## ** Return one summary row per effect
      data.frame(
        label = idf$label[i],
        f = idf$f[i],
        model_df = idf$model_df[i],
        test_df = idf$test_df[i],
        target_power = target_power,
        required_n = ceiling(fit$v + idf$model_df[i] + 1)
      )
    })
  )

  ## * Create the power curves
  ##
  ## geom_line() directly connects the calculated sample-size requirements.
  ## It does not fit a statistical smoother or regression line to the points.
  p <- ggplot2::ggplot(
    curve_data,
    ggplot2::aes(
      x = sample_size,
      y = power,
      colour = label,
      group = label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.5) +

    ## ** Show the conventional target-power threshold
    ggplot2::geom_hline(
      yintercept = target_power,
      linetype = "dashed",
      colour = "grey40"
    ) +

    ## ** Restrict the visible region without excluding data before plotting
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0, 1)) +

    ## ** Use interpretable power-axis labels
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, by = 0.1),
      minor_breaks = NULL
    ) +

    ggplot2::theme_light() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      legend.position = "right"
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Total sample size",
      y = "Power",
      colour = "Effect"
    )

  ## * Save the figure only when an output filename was supplied
  if (!is.null(output_file)) {
    ggplot2::ggsave(
      filename = output_file,
      plot = p,
      width = 7,
      height = 6,
      dpi = 300
    )
  }

  ## * Return both the figure and the underlying sample-size calculations
  return(
    list(
      plot = p,
      curve_data = curve_data,
      required_n = required_n
    )
  )
}


#' Convert partial eta-squared to Cohen's f
#'
#' Converts partial eta-squared to Cohen's \eqn{f}, suitable for use in
#' \code{plot_f2_power()} and \code{pwr::pwr.f2.test()}.
#'
#' @param peta Numeric vector of partial eta-squared values. Values must be
#'   greater than or equal to zero and less than one.
#'
#' @return A numeric vector of Cohen's \eqn{f} values.
#'
#' @examples
#' peta_to_f(0.042)
#'
#' @export
peta_to_f <- function(peta) {

  ## * Ensure partial eta-squared values are within their valid range
  if (any(is.na(peta) | peta < 0 | peta >= 1)) {
    stop("peta values must be non-missing and between 0 (inclusive) and 1 (exclusive).")
  }

  ## * Cohen's f = sqrt(partial eta-squared / (1 - partial eta-squared))
  return(sqrt(peta / (1 - peta)))
}
