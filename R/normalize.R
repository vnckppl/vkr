#' Display Normality and Normalize Data
#'
#' This function provides an overview of the normality of data and allows for
#' normalizing data that is not normally distributed
#'
#' @importFrom moments "skewness"
#' @importFrom moments "kurtosis"
#' @importFrom moments "jarque.test"
#' @importFrom Hmisc "label"
#' @param df Input data frame
#' @param inputs List of input variables that are being processed
#' @param transform If TRUE, apply log transformation
#' @param truncsd If set to a value, truncate the log transformed data to mean
#' +/- truncsd
#' @param plotdir If set to a path, it will output the normality plots there
#' @param verbose If TRUE, display verbose output
#' @export

normalize <-
    function(
             df,
             inputs,
             transform = FALSE,
             truncsd = FALSE,
             plotdir = FALSE,
             verbose = FALSE
             ) {

        ## * Create empty output matrix
        # 5 Columns: 1) skweness; 2) kurtosis; 3) Jarque-Bera normality
        # estimate; 4) JB p-value; 5) JB alternative hypothesis
        omatjb <- data.frame(
            matrix(, nrow = length(inputs), ncol = 5)
        )
        row.names(omatjb) <- inputs
        names(omatjb) <- c(
            "Skewness", "Kurtosis", "JB_est", "JB_pval", "JB_hyp"
        )

        ## * Fill table with Skewness, Kurtosis & Jarque-Bera Normality estimate
        for (var in inputs) {

            ## ** Calculate values
            var_skewness <- round(moments::skewness(df[[var]], na.rm = TRUE), 2)
            var_kurtosis <- round(moments::kurtosis(df[[var]], na.rm = TRUE), 2)
            jbtest <- moments::jarque.test(
                as.numeric(df[[var]][!is.na(as.numeric(df[[var]]))]
                           ))

            ## ** Store them in the output matrix
            omatjb[var, "Skewness"] <- var_skewness
            omatjb[var, "Kurtosis"] <- var_kurtosis
            omatjb[var, "JB_est"]   <- round(jbtest$statistic, 3)
            omatjb[var, "JB_pval"]  <- round(jbtest$p.value, 3)
            omatjb[var, "JB_hyp"]   <- jbtest$alternative
        }

        ## * Plot an overview figure with the skewness for all variables
        if (grepl("character", class(plotdir))) {

            ## ** Create plot output folder
            if (!dir.exists(plotdir)) {
                dir.create(plotdir, showWarnings = FALSE, recursive = TRUE)
            }

            ## ** Initiate to plot the overview figure
            figfile <- file.path(plotdir, "skewness_overview.pdf")
            pdf(figfile, width = 10, height = 10)

            ## ** Bar plot
            barplot(omatjb$Skewness, names.arg = rownames(omatjb), las = 2)

            ## ** Add horizontal lines to easily see high values
            abline(h = -1, col = "blue", lty = "dashed")
            abline(h = 1, col = "blue")
            abline(h = 2, col = "red")
            abline(h = 3, col = "red", lwd = 2)

            ## ** Save
            dev.off()
        }

        ## * [function] Z-transformation
        ztrans <- function(data) {
            z <- ((data - mean(data, na.rm = TRUE)) / (sd(data, na.rm = TRUE)))
            return(z)
        }

        ## * [function] Truncation: set values < x SD or > x SD to -/+x SD
        # For removal of driving outliers. So, e.g., set a value that is larger
        # than mean+3sd to 3sd.
        resetoutl <- function(tmpdf, sdthreshold) {

            ## ** Calculate standard deviation
            mysd <- sd(tmpdf, na.rm = TRUE)
            myavg <- mean(tmpdf, na.rm = TRUE)

            ## ** Calculate critical values
            out_neg <- myavg - (sdthreshold * mysd)
            if (verbose) print(paste("Negative threshold:", out_neg))
            out_pos <- myavg + (sdthreshold * mysd)
            if (verbose) print(paste("Positive threshold:", out_pos))

            ## ** Report number of outliers
            if (verbose) print(paste("Number of negative outliers:",
                                     length(tmpdf[tmpdf < out_neg])))
            if (verbose) print(paste("Number of positive outliers:",
                                     length(tmpdf[tmpdf > out_pos])))

            ## ** Reset values to mean -/+ `sdthreshold`*SD
            # (if they fall outside that range)
            tmpdf[tmpdf < out_neg] <- out_neg
            tmpdf[tmpdf > out_pos] <- out_pos

            ## ** Return new data frame
            return(tmpdf)
        }

        ## * Log transform data
        ## It does not matter for the data to be log transformed, or log-base-10
        ## transformed for the association, so I will just use log.
        for (var in inputs) {

            ## ** Announce
            cat("\n")
            if (verbose) print(paste("Working on:", var))

            ## ** New variable name
            newvar <- paste0(var, "_log")

            ## ** Prepare the variable for log transformation
            # When there are zeros in the raw data, a log transform will result
            # in Inf variables. In this case, remove a small number form the
            # zeros before transformation.
            if (any(df[[var]] == 0, na.rm = TRUE)) {

                ## *** Calculate how much to add (5% of the median value)
                toadd <- median(df[[var]], na.rm = TRUE) * 0.05
                stattype <- "median"

                ## *** If this results in 0, because the median is 0, use mean
                if (toadd == 0) {
                    toadd <- mean(df[[var]], na.rm = TRUE) * 0.05
                    stattype <- "mean"
                }

                ## *** Announce
                if (verbose) {
                    print(paste(
                        var, "contains zeros and will be adjusted by adding",
                        "5% of the", stattype, "(i.e.,", toadd, ") of the",
                        "variable to the array."
                    ))
                }

                ## *** If zeros exist, add 5% of the median value
                tmp <- df[[var]] + toadd

            } else {

                ## *** Announce
                if (verbose) {
                    print(paste(
                        var,
                        "does NOT contain zeros and thus will NOT be adjusted"
                    ))
                }

                ## *** Just use the original data
                tmp <- df[[var]]

            }

            ## ** Apply log transformation
            if (transform == TRUE) {

                ## *** If requested, also apply truncation
                if (grepl("numeric", class(truncsd))) {

                    # Log transform and then remove outliers
                    df[[newvar]] <- log(tmp) |>
                        resetoutl(tmpdf = _, sdthreshold = truncsd)

                } else {

                    # Log transform only
                    df[[newvar]] <- log(tmp)
                }
            }

            ## ** Create density curves of the data for inspection
            # Test if plotdir was set (if we want plots)
            if (grepl("character", class(plotdir))) {

                ## *** Calculate density curve data
                ## **** Raw data
                mydensity_rd <- density(ztrans(df[[var]]), na.rm = TRUE)
                maxy_rd <- max(mydensity_rd$y)
                maxx_rd <- max(mydensity_rd$x)
                minx_rd <- min(mydensity_rd$x)

                ## **** Square root transformation
                mydensity_sr <- density(ztrans(sqrt(df[[var]])), na.rm = TRUE)
                maxy_sr <- max(mydensity_sr$y)
                maxx_sr <- max(mydensity_sr$x)
                minx_sr <- min(mydensity_sr$x)

                ## **** Arc-sine transformation
                ## (values have to be between 0-1)
                maxvar <- max(abs(sqrt(df[[var]])), na.rm = TRUE)
                mydensity_as <- density(
                    ztrans(asin(sqrt(df[[var]]) / maxvar)), na.rm = TRUE
                )
                maxy_as <- max(mydensity_as$y)
                maxx_as <- max(mydensity_as$x)
                minx_as <- min(mydensity_as$x)

                ## **** Log transformation
                mydensity_lo <- density(ztrans(log(tmp)), na.rm = TRUE)
                maxy_lo <- max(mydensity_lo$y)
                maxx_lo <- max(mydensity_lo$x)
                minx_lo <- min(mydensity_lo$x)

                ## **** Log transformation with truncation
                if (is.null(truncsd)) {
                    truncsdfig <- 2.5
                } else {
                    truncsdfig <- truncsd
                }
                mydensity_lt <- log(tmp) |>
                    resetoutl(tmpdf = _, sdthreshold = truncsdfig) |>
                    ztrans(data = _) |>
                    density(x = _, na.rm = TRUE)
                maxy_lt <- max(mydensity_lo$y)
                maxx_lt <- max(mydensity_lo$x)
                minx_lt <- min(mydensity_lo$x)

                ## *** Calculate max maximum for all distributions
                maxy4fig <- max(maxy_rd, maxy_sr, maxy_as, maxy_lo, maxy_lt)
                maxx4fig <- max(maxx_rd, maxx_sr, maxx_as, maxx_lo, maxx_lt)
                minx4fig <- min(minx_rd, minx_sr, minx_as, minx_lo, minx_lt)

                ## *** Create plots
                figfile <- file.path(plotdir, paste0("density_", var, ".pdf"))
                pdf(figfile, width = 10, height = 10)

                ## *** Check if a variable label is available
                if (Hmisc::label(df[[var]]) == "") {
                    plottitle <- var
                } else {
                    plottitle <- Hmisc::label(df[[var]])
                }

                ## **** Raw data
                plot(
                    mydensity_rd,
                    ylim = c(0, 1.02 * maxy4fig),
                    xlim = c(0.98 * minx4fig, 1.02 * maxx4fig),
                    main = plottitle
                )

                ## **** Square root transformation
                lines(mydensity_sr, col = "green")

                ## **** Arc-sine transformation
                lines(mydensity_as, col = "blue")

                ## **** Log transformation (lwd = 3)
                lines(mydensity_lo, col = "red")

                ## **** Log transformation
                lines(mydensity_lt, col = "pink")

                ## *** Add legend
                legend(
                    x = "topleft",
                    legend = c("Raw", "Sqrt", "Arcsine", "Log", "Log + Trunc"),
                    fill = c("black", "green", "blue", "red", "pink")
                )

                ## *** Add annotation
                text(
                    x = c(rep(0.95 * maxx4fig, 6)),
                    y = c(
                        1.00 * maxy4fig, 0.97 * maxy4fig, 0.94 * maxy4fig,
                        0.91 * maxy4fig, 0.88 * maxy4fig, 0.85 * maxy4fig
                    ),
                    labels = c(
                        paste("Skewness Raw:", omatjb[var, "Skewness"]),
                        paste("Skewness Sqrt:",
                              moments::skewness(mydensity_sr$y, na.rm = TRUE)),
                        paste("Skewness Asin:",
                              moments::skewness(mydensity_as$y, na.rm = TRUE)),
                        paste("Skewness Log:",
                              moments::skewness(mydensity_lo$y, na.rm = TRUE)),
                        paste("Skewness Log+Trunc:",
                              moments::skewness(mydensity_lt$y, na.rm = TRUE)),
                        paste("Zeros:",
                              as.character(any(df[[var]] == 0, na.rm = TRUE)))
                    ),
                    pos = 2,
                    col = "gray"
                )

                ## *** Save
                dev.off()
            }

        }

        ## * Return output table with skewness information and new df
        return(list(omatjb, df))
    }
