#' Install and/or update a package without bugging for selecting a repo
#'
#' @param my_package Name of the package to be installed
#'
#' @export
install <- function(my_package) {

    ## * CRAN Repo
    my_repo <- "https://repo.miserver.it.umich.edu/cran/"

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
