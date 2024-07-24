#' Quick Principal component Analysis
#'
#' @param input_data Data frame for PCA
#' @param rotation Type of rotation (varimax or oblimin)
#' @param out_path Path for storing the scree plot
#' @param out_name File name for the scree plot
#' @export
quick_pca <- function(input_data, rotation, out_path, out_name) {

    ## * Input Parameters
    ## ** Check input data
    if (!exists("input_data")) {
        stop("Input data does not exist")
    }
    if (!is.data.frame(get("input_data"))) {
        stop("Input data is not a data frame")
    }

    ## ** Check rotation
    if (!exists("rotation")) {
        stop("Rotation not specified")
    }
    if (!grepl("^varimax$", rotation) && !grepl("^oblimin$", rotation)) {
        stop(paste(
            "Rotation not specified correctly.",
            "Please enter 'varimax' or 'oblimin'",
            sep = "",
            ))
    }

    ## ** Output Path
    if (!exists("out_path")) {
        out_path <- path.expand("~")
    }
    if (!file.exists(out_path)) {
        dir.create(out_path)
    }

    ## ** Output File
    if (!exists("out_name")) {
        out_name <- paste("PCA_", format(Sys.time(), "%Y%m%d_%H%M%S"), sep = "")
    }

    ## * Run PCA
    ## ** Find optimal number of components
    ## This is just the number of components with an eigen value > 1
    c.PCA1 <- psych::principal(
        input_data,
        nfactors = length(input_data),
        residuals = FALSE,
        rotate = "none",
        covar = FALSE,
        scores = TRUE
    )

    ## ** Prepare Plots
    ## *** Eigen Value
    scree_data <- as.data.frame(c.PCA1$values)
    scree_data[2] <- seq(1, nrow(scree_data))
    colnames(scree_data) <- c("eigen_value", "component")
    ## *** Give eigen values > 1 a different color
    scree_data$eigen_value_g <- "<1"
    scree_data$eigen_value_g[scree_data$eigen_value > 1] <- ">1"
    ## *** Explained Variance
    scree_data2 <- as.data.frame(c.PCA1$Vaccounted[2, ])
    scree_data2[2] <- seq(1, nrow(scree_data2))
    colnames(scree_data2) <- c("ExplainedVariance", "component")
    ## *** Give explained variance for eigen values > 1 a different color
    scree_data2$eigen_value_g <- "<1"
    scree_data2$eigen_value_g[scree_data$eigen_value > 1] <- ">1"

    ## ** Plot 1: Scree Plot
    plot_1 <- ggplot2::ggplot() +
        ggplot2::geom_point(data = scree_data,
                            ggplot2::aes(x = component,
                                         y = eigen_value,
                                         color = eigen_value_g)) +
        ggplot2::geom_line(data = scree_data,
                           ggplot2::aes(x = component,
                                        y = eigen_value,
                                        color = eigen_value_g)) +
        ggplot2::geom_hline(
                     yintercept = 1, linetype = "dashed", color = "black") +
        ggplot2::theme(legend.position = c(0.9, 0.8)) +
        ggplot2::labs(
                     color = "Eigen Value",
                     y = "Eigen Value",
                     caption = "--- EV = 1",
                     title = "Scree Plot"
                 ) +
        ggplot2::scale_x_discrete(
                     name = "Principal Component Number",
                     limits = as.factor(c(seq(1, length(scree_data$component))))
                 )

    ## ** Plot 2: Explained Variance Plot
    plot_2 <- ggplot2::ggplot() +
        ggplot2::geom_point(data = scree_data2,
                            ggplot2::aes(
                                         x = component,
                                         y = ExplainedVariance * 100,
                                         color = eigen_value_g)) +
        ggplot2::theme(legend.position = c(0.9, 0.8)) +
        ggplot2::labs(
                     color = "Eigen Value",
                     y = "Explained Variance (%)",
                     title = "Explained Variance"
                 ) +
        ggplot2::scale_x_discrete(
                     name <- "Principal Component Number",
                     limits = factor(c(seq(1, length(scree_data$component))))
                 )

    ## ** Combine plots into a single plot
    plots_combined <- ggpubr::ggarrange(plot_1, plot_2, ncol = 1, nrow = 2)
    plots_combined

    ## *** Save Plot
    ggplot2::ggsave(file.path(out_path, out_name),
                    plot = ggplot2::last_plot(), device = NULL, path = NULL,
                    scale = 1, width = NA, height = NA,
                    dpi = 300, limitsize = TRUE
                    )

    ## ** Re-run PCA with the optimal number of components and selected rotation
    c.PCA2 <- psych::principal(
        input_data,
        nfactors = sum(c.PCA1$values > 1),
        residuals = FALSE,
        rotate = rotation,
        covar = FALSE,
        scores = TRUE
    )

    ## ** Extract values of interest
    ## Create a table that shows per input variable on which
    ## principal component it loads the strongest. This way, you
    ## can easily see how variables cluster.
    ## *** Automatically extract factors based on the loadings
    loadings <- c.PCA2$loadings[seq_len(length(names(input_data))), ]

    ## *** If there is only one component, an array is returned.
    # Convert this to a data frame
    if (class(loadings)[1] == "numeric") loadings <- as.data.frame(loadings)

    ## *** Create empty data frame for storing output
    loading_select <- loadings[, 1:sum(c.PCA2$values > 1)]
    if (class(loading_select)[1] == "numeric") loading_select <- loadings

    loading_select[] <- NA
    ## *** For each row find the column with the largest -absolute- value
    for (row in seq(1, nrow(loadings))) {

        ## **** Get all abslute row values
        row_v <- abs(loadings[row, 1:sum(c.PCA2$values > 1)])

        ## **** Get max position
        col <- which(row_v == max(row_v))

        ## **** Set the highest value in the row to 1 in our new matrix
        loading_select[row, col] <- max(row_v)

    }

    ## ** Store original data along the principal components ("scores")
    my_output <- list()
    ## *** Overview of how variables cluster
    ## (For the PCA with the optimal number of components)
    my_output[[1]] <- loading_select
    ## *** Output of the first PCA that has all components
    my_output[[2]] <- c.PCA1
    ## *** Output of the second PCA with the optimized number of components
    ## (e.g., factor scores are under my_output[[2]]$scores
    my_output[[3]] <- c.PCA2

    ## ** Return output
    return(my_output)
    print(data.frame(loading_select))

}
