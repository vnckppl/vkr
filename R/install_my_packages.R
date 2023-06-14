#' Install and/or update all packages I use frequently
#'
#' @export
install_my_packages <- function() {

    ## * List of my packages with short description
    my_packages <- c(
        "car", "Levene's test (homogeneity of variance)",
        "devtools", "Install source packages",
        "foreign", "Read SPSS files",
        "ggeffects", "Marginal means",
        "ggplot2", "Plotting",
        "ggsignif", "Significance bars for ggplot",
        "gmodels", "Cross tables",
        "lintr", "Syntax checking for ESS",
        "lsr", "Effect sizes",
        "pastecs", "Descriptive statistics",
        "pwr", "Power analysis",
        "readxl", "Read xls(x)",
        "writexl", "Write xls(x)",
        "roxygen2", "Build your own package",
        "sjstats", "Effect sizes"
        )

    ## * Filter package names
    my_packages <- my_packages[c(TRUE, FALSE)]

    ## * CRAN Repo
    my_repo <- "https://repo.miserver.it.umich.edu/cran/"

    ## * Install packages
    for (my_package in my_packages) {

        ## ** Announce
        cat(paste("------------ Checking:", my_package, "\n"))

        ## ** Test if package has already been installed
        if (my_package %in% rownames(installed.packages()) == FALSE) {
            install.packages(
                my_package,
                repos = my_repo
            )
        } else if (my_package %in% rownames(installed.packages()) == TRUE) {
            update.packages(
                my_package,
                repos = my_repo
            )
        }
    }
}
