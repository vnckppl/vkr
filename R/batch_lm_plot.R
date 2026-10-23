#' Regression plot with covariate adjustment
#'
#' Fits \code{outcome ~ covariates + predictor} on complete cases and plots the
#' result. For a numeric predictor, draws the raw data with the covariate-adjusted
#' regression line (with 95\% CI) and an added-variable plot. For a factor
#' predictor, draws a boxplot with jittered raw points and covariate-adjusted
#' group means (with 95\% CI).
#'
#' @param df Data frame
#' @param outcome Name of the outcome column (string)
#' @param covariates Character vector of covariate column names
#' @param predictor Name of the predictor column (string)
#' @param output_dir Folder to save the plot in. If \code{NULL} (default), the
#'   plot is displayed instead. Otherwise it is written to
#'   \code{output_dir/outcome_predictor.pdf}. The folder is created if missing.
#' @param width_cm,height_cm PDF size in cm. Defaults: 24 x 12 for a numeric
#'   predictor (two panels), 14 x 12 for a factor predictor.
#' @return The fitted \code{lm} object, invisibly
#' @export
batch_lm_plot <- function(df, outcome, covariates = character(0), predictor,
                            output_dir = NULL, width_cm = NULL, height_cm = 12) {

  # ---- Prep ------------------------------------------------------------
  vars  <- c(outcome, covariates, predictor)
  df_cc <- df[complete.cases(df[, vars]), vars, drop = FALSE]

  # Treat character/logical columns as factors
  for (v in c(covariates, predictor)) {
    if (is.character(df_cc[[v]]) || is.logical(df_cc[[v]])) {
      df_cc[[v]] <- factor(df_cc[[v]])
    }
  }
  df_cc <- droplevels(df_cc)

  rhs        <- paste(c(covariates, predictor), collapse = " + ")
  my_formula <- paste(outcome, "~", rhs)
  fit <- lm(formula = as.formula(my_formula), data = df_cc, na.action = na.exclude)

  # Covariates held at mean (numeric) / reference level (factor)
  make_newdata <- function(n) {
    nd <- df_cc[rep(1, n), , drop = FALSE]
    for (v in covariates) {
      nd[[v]] <- if (is.numeric(df_cc[[v]])) {
        mean(df_cc[[v]])
      } else {
        factor(levels(df_cc[[v]])[1], levels = levels(df_cc[[v]]))
      }
    }
    nd
  }

  is_cat <- is.factor(df_cc[[predictor]])

  # ---- Optional: send plot to a PDF instead of the screen ---------------
  # The device is opened before any par() call. dev.off() is registered first,
  # and the par() reset below uses after = FALSE so it runs BEFORE dev.off().
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(output_dir, paste0(outcome, "_", predictor, ".pdf"))
    if (is.null(width_cm)) width_cm <- if (is_cat) 14 else 24
    pdf(out_file, width = width_cm / 2.54, height = height_cm / 2.54)
    on.exit(dev.off(), add = TRUE)
  }

  # ======================================================================
  # Categorical predictor: boxplot + jittered points + adjusted means
  # ======================================================================
  if (is_cat) {
    lv <- levels(df_cc[[predictor]])
    k  <- length(lv)

    # Overall F-test for the predictor, adjusted for covariates
    p_overall <- drop1(fit, scope = predictor, test = "F")[predictor, "Pr(>F)"]

    # Covariate-adjusted group means with 95% CI
    nd <- make_newdata(k)
    nd[[predictor]] <- factor(lv, levels = lv)
    pred <- predict(fit, newdata = nd, interval = "confidence")

    cols <- adjustcolor(seq_len(k) + 1, alpha.f = 0.6)

    op <- par(mar = c(4.5, 4.5, 3, 1))
    on.exit(par(op), add = TRUE, after = FALSE)

    boxplot(df_cc[[outcome]] ~ df_cc[[predictor]],
            outline = FALSE, col = "grey92", border = "grey40",
            xlab = predictor, ylab = outcome,
            main = sprintf("%s by %s (overall p = %.3g)", outcome, predictor, p_overall),
            ylim = range(df_cc[[outcome]], pred[, c("lwr", "upr")]))
    stripchart(df_cc[[outcome]] ~ df_cc[[predictor]],
               vertical = TRUE, method = "jitter", jitter = 0.15,
               pch = 19, col = cols, add = TRUE)

    # Adjusted means (diamonds) offset slightly so they don't hide the box median
    xo <- seq_len(k) + 0.3
    arrows(xo, pred[, "lwr"], xo, pred[, "upr"],
           angle = 90, code = 3, length = 0.05, lwd = 2)
    points(xo, pred[, "fit"], pch = 23, bg = "red", cex = 1.5)
    legend("topright", bty = "n", pch = 23, pt.bg = "red",
           legend = "Adjusted mean \u00b1 95% CI")

    return(invisible(fit))
  }

  # ======================================================================
  # Numeric predictor: raw data + adjusted line, and added-variable plot
  # ======================================================================
  b <- coef(fit)

  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  on.exit(par(op), add = TRUE, after = FALSE)

  ## Plot 1: raw data + covariate-adjusted line
  x_seq <- seq(min(df_cc[[predictor]]), max(df_cc[[predictor]]), length.out = 100)
  nd    <- make_newdata(length(x_seq))
  nd[[predictor]] <- x_seq
  pred  <- predict(fit, newdata = nd, interval = "confidence")

  plot(df_cc[[predictor]], df_cc[[outcome]],
       pch = 19, col = adjustcolor("black", alpha.f = 0.6),
       xlab = predictor, ylab = outcome,
       main = "Raw data + adjusted fit")
  polygon(c(x_seq, rev(x_seq)), c(pred[, "lwr"], rev(pred[, "upr"])),
          col = adjustcolor("red", alpha.f = 0.15), border = NA)
  lines(x_seq, pred[, "fit"], col = "red", lwd = 2)

  ## Plot 2: added-variable (partial regression) plot
  if (length(covariates) > 0) {
    cov_str <- paste(covariates, collapse = " + ")
    res_y <- resid(lm(as.formula(paste(outcome,   "~", cov_str)), data = df_cc))
    res_x <- resid(lm(as.formula(paste(predictor, "~", cov_str)), data = df_cc))
  } else {
    res_y <- df_cc[[outcome]]   - mean(df_cc[[outcome]])
    res_x <- df_cc[[predictor]] - mean(df_cc[[predictor]])
  }

  plot(res_x, res_y,
       pch = 19, col = adjustcolor("black", alpha.f = 0.6),
       xlab = paste(predictor, "| covariates"),
       ylab = paste(outcome,   "| covariates"),
       main = "Added-variable plot")
  abline(lm(res_y ~ res_x), col = "red", lwd = 2)
  legend("topleft", bty = "n",
         legend = sprintf("beta = %.3g, p = %.3g",
                          b[predictor],
                          summary(fit)$coefficients[predictor, "Pr(>|t|)"]))

  invisible(fit)
}
